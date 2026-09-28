#include "share/atspre_staload.hats"
#use gestures as G
staload "gestures/src/consts.sats"
staload "gestures/src/classify.sats"
staload "gestures/src/pointer.sats"
staload "gestures/src/tracker.sats"

(* one that never locked cannot commit *)
prval _ = RLcommit(): RELEASE(AMB, END_COMMIT)

implement main0 () = ()
