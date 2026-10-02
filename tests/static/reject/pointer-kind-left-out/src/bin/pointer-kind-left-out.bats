#include "share/atspre_staload.hats"
#use gestures as G
staload "gestures/src/consts.sats"
staload "gestures/src/classify.sats"
staload "gestures/src/pointer.sats"
staload "gestures/src/tracker.sats"

(* a match on a pointer's kind that leaves the pen out *)
fn is_mouse (kind: pointer_kind): bool =
  case+ kind of
  | Touch() => false
  | Mouse() => true

implement main0 () = ()
