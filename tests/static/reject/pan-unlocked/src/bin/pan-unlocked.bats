#include "share/atspre_staload.hats"
#use gestures as G
staload "gestures/src/consts.sats"
staload "gestures/src/classify.sats"
staload "gestures/src/pointer.sats"
staload "gestures/src/tracker.sats"

(* only a locked pointer pans *)
fn f (): panned(DeadZone) = Panned(LockedOnH() | 1, 5)

implement main0 () = ()
