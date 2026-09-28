#include "share/atspre_staload.hats"
#use gestures as G
staload "gestures/src/consts.sats"
staload "gestures/src/classify.sats"
staload "gestures/src/pointer.sats"
staload "gestures/src/tracker.sats"

(* nor can a locked pointer lock again *)
prval _ = MVlockv(deadzone_diagonal{0}()): MOVE(LOCKH, LOCKV)

implement main0 () = ()
