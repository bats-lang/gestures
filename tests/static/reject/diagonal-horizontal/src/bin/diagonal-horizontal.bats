#include "share/atspre_staload.hats"
#use gestures as G
staload "gestures/src/consts.sats"
staload "gestures/src/classify.sats"
staload "gestures/src/pointer.sats"
staload "gestures/src/tracker.sats"

(* a diagonal is not horizontal *)
prval _ = CLh(ABSpos(), ABSpos()): CLASS(1, 1, Horizontal)

implement main0 () = ()
