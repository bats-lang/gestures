#include "share/atspre_staload.hats"
#use gestures as G
staload "gestures/src/consts.sats"
staload "gestures/src/classify.sats"
staload "gestures/src/pointer.sats"
staload "gestures/src/tracker.sats"

(* locked_unlocked needs a locked and an unlocked proof: two locked ones
   are not a contradiction, so [false] is not proved from them *)
prfn two_locked {p:phase} (a: LOCKED(p), b: LOCKED(p)): [false] void =
  locked_unlocked(a, b)

implement main0 () = ()
