#include "share/atspre_staload.hats"
#use gestures as G
staload "gestures/src/consts.sats"
staload "gestures/src/classify.sats"
staload "gestures/src/pointer.sats"
staload "gestures/src/tracker.sats"

(* a locked pointer cannot long-press *)
fn f (p: pointer(LockedH)): @(pointer(LongPressed), region) = pointer_long_press(p)

implement main0 () = ()
