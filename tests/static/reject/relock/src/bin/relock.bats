#include "share/atspre_staload.hats"
#use gestures as G
staload "gestures/src/consts.sats"
staload "gestures/src/classify.sats"
staload "gestures/src/pointer.sats"
staload "gestures/src/tracker.sats"

(* nor can a locked pointer lock again: only a deciding phase locks *)
prval _ = MVlockv(DecidingWithinSlop(), CLv(ABSpos(), ABSpos()): CLASS(0, 10, Vertical)): MOVE(LockedH, LockedV)

implement main0 () = ()
