#include "share/atspre_staload.hats"
#use gestures as G
staload "gestures/src/consts.sats"
staload "gestures/src/classify.sats"
staload "gestures/src/pointer.sats"
staload "gestures/src/tracker.sats"

(* Each choice matched in full; a region's parent is a region or none *)
fn dragging {p:phase} (phase: phase_is(p)): bool =
  case+ phase of
  | AtWithinSlop() => true
  | AtDeadZone() => true
  | AtLockedH() => true
  | AtLockedV() => true
  | AtRejected() => true
  | AtLongPressed() => true
  | AtInert() => false
fn is_mouse (kind: pointer_kind): bool =
  case+ kind of
  | Touch() => false
  | Mouse() => true
  | Pen() => false
fn axis_name {c:axis_class} (class: class_is(c)): string =
  case+ class of
  | IsHorizontal() => "horizontal"
  | IsVertical() => "vertical"
  | IsAmbiguous() => "dead zone"
fn declare (): void = let
  val st = gestures_new()
  val () = gestures_region(st, 1, NoRegion(), AxH(), false, false, DevAll())
  val () = gestures_region(st, 2, InRegion(1), AxV(), false, false, DevTouch())
in gestures_free(st) end

implement main0 () = ()
