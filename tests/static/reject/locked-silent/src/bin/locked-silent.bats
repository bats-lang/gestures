#include "share/atspre_staload.hats"
#use gestures as G
staload "gestures/src/consts.sats"
staload "gestures/src/classify.sats"
staload "gestures/src/pointer.sats"
staload "gestures/src/tracker.sats"

(* a locked drag cannot end with neither commit nor cancel *)
prval _ = RLnone(): RELEASE(LOCKH, END_NONE)

implement main0 () = ()
