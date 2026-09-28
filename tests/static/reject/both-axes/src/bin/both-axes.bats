#include "share/atspre_staload.hats"
#use gestures as G
staload "gestures/src/consts.sats"
staload "gestures/src/classify.sats"
staload "gestures/src/pointer.sats"
staload "gestures/src/tracker.sats"

(* 10 across and 1 down is not vertical *)
prval _ = CLv(ABSpos(), ABSpos()): CLASS(10, 1, VERT)

implement main0 () = ()
