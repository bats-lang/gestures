#include "share/atspre_staload.hats"
#use gestures as G
staload "gestures/src/consts.sats"
staload "gestures/src/classify.sats"
staload "gestures/src/pointer.sats"
staload "gestures/src/tracker.sats"

(* a region's parent is a region or none, not -1 *)
fn declare (): void = let
  val st = gestures_new()
  val () = gestures_region(st, 1, ~1, AxH(), false, false, DevAll())
in gestures_free(st) end

implement main0 () = ()
