#include "share/atspre_staload.hats"
#use gestures as G
staload "gestures/src/consts.sats"
staload "gestures/src/classify.sats"
staload "gestures/src/pointer.sats"
staload "gestures/src/tracker.sats"

(* a match on a pointer's phase that leaves inert out *)
fn dragging {p:phase} (phase: phase_is(p)): bool =
  case+ phase of
  | AtWithinSlop() => true
  | AtDeadZone() => true
  | AtLockedH() => true
  | AtLockedV() => true
  | AtRejected() => true
  | AtLongPressed() => true

implement main0 () = ()
