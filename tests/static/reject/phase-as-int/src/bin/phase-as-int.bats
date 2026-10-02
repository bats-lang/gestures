#include "share/atspre_staload.hats"
#use gestures as G
staload "gestures/src/consts.sats"
staload "gestures/src/classify.sats"
staload "gestures/src/pointer.sats"
staload "gestures/src/tracker.sats"

(* a phase is one of seven, not a number *)
fn f (p: pointer(2)): void = let val (_ | e) = pointer_abort(p) in
  case+ e of ~EndNone() => () | ~EndCommit(_, _) => () | ~EndCancel(_, _) => () end

implement main0 () = ()
