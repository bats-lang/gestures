#include "share/atspre_staload.hats"
#use gestures as G
staload "gestures/src/consts.sats"
staload "gestures/src/classify.sats"
staload "gestures/src/pointer.sats"
staload "gestures/src/tracker.sats"

(* class_exclusive needs a horizontal and a vertical class: two horizontal
   ones are not a contradiction, so [false] is not proved from them *)
prfn two_horizontal {dx,dy:int} (a: CLASS(dx, dy, Horizontal), b: CLASS(dx, dy, Horizontal)): [false] void =
  class_exclusive(a, b)

implement main0 () = ()
