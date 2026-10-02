#include "share/atspre_staload.hats"
#use gestures as G
staload "gestures/src/consts.sats"
staload "gestures/src/classify.sats"
staload "gestures/src/pointer.sats"
staload "gestures/src/tracker.sats"

(* a cancel cannot commit *)
prval _ = ABcancel(LockedOnH()): ABORT(LockedH, Committed)

implement main0 () = ()
