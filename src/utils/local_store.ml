open Std

type ref_and_reset = F : 'a ref * (unit -> 'a) -> ref_and_reset
type bindings = {
  mutable refs: ref_and_reset list;
  mutable frozen : bool;
  is_bound: bool ref
}

let new_bindings () =
  { refs = []; is_bound = ref false; frozen = false }

let is_bound t = !(t.is_bound)

let reset t =
  assert (is_bound t);
  List.iter ~f:(fun (F (ref, initializer_)) -> ref := initializer_ ()) t.refs

let ref t f =
  let result = ref (f ()) in
  assert (not t.frozen);
  t.refs <- (F (result, f)) :: t.refs;
  result

type slot = Slot : { ref : 'a ref; mutable value : 'a } -> slot
type scope = { slots: slot list; scope_bound : bool ref }

let fresh t =
  t.frozen <- true;
  { slots = List.map ~f:(fun (F(ref,f)) -> Slot {ref; value = f ()}) t.refs;
    scope_bound = t.is_bound }

let with_scope { slots; scope_bound } f =
  assert (not !scope_bound);
  scope_bound := true;
  List.iter ~f:(fun (Slot {ref;value}) -> ref := value) slots;
  Fun.protect f ~finally:(fun () ->
    List.iter ~f:(fun (Slot s) -> s.value <- !(s.ref)) slots;
    scope_bound := false
  )

module Compiler = struct
  let compiler_state = new_bindings ()
  let sref f = ref compiler_state f
  let srefk k = ref compiler_state (fun () -> k)
end
