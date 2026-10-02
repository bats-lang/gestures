#include "share/atspre_staload.hats"
#use gestures as G
staload "gestures/src/consts.sats"
staload "gestures/src/classify.sats"
staload "gestures/src/pointer.sats"
staload "gestures/src/tracker.sats"

(* a match on a displacement's class that leaves the dead zone out *)
fn axis_name {c:axis_class} (class: class_is(c)): string =
  case+ class of
  | IsHorizontal() => "horizontal"
  | IsVertical() => "vertical"

implement main0 () = ()
