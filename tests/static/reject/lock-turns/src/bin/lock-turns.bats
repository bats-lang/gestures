#include "share/atspre_staload.hats"
#use gestures as G
staload "gestures/src/consts.sats"
staload "gestures/src/classify.sats"
staload "gestures/src/pointer.sats"
staload "gestures/src/tracker.sats"

(* a pointer locked horizontally cannot move to vertical *)
prval _ = MVstay(): MOVE(LOCKH, LOCKV)

implement main0 () = ()
