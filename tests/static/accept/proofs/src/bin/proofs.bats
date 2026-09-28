#include "share/atspre_staload.hats"
#use gestures as G
staload "gestures/src/consts.sats"
staload "gestures/src/classify.sats"
staload "gestures/src/pointer.sats"
staload "gestures/src/tracker.sats"

(* The proofs, used: a class is one of three and unique; flipping
   either sign keeps it; the diagonal is ambiguous; a locked pointer
   stays locked; its release is a commit or a cancel; a cancel is never
   a commit *)
prfn both_is_false {dx,dy:int} (h: CLASS(dx, dy, HORIZ), v: CLASS(dx, dy, VERT)): [false] void =
  class_exclusive(h, v)
prfn flip_both {dx,dy,c:int} (p: CLASS(dx, dy, c)): CLASS(~dx, ~dy, c) =
  class_flip_y(class_flip_x(p))
prfn mirror {dx,dy,c,d:int} (p: CLASS(dx, dy, c), q: CLASS(~dx, dy, d)): [c == d] void =
  class_unique(class_flip_x(p), q)
prval _ = deadzone_diagonal{7}()
prfn stays {q:int} (m: MOVE(LOCKH, q)): [q == LOCKH] void = lock_stable(m)
prfn ends {o:int} (r: RELEASE(LOCKV, o)): [o == END_COMMIT || o == END_CANCEL] void = locked_ends(r)
prfn quiet {o:int} (r: RELEASE(AMB, o)): [o == END_NONE] void = unlocked_silent(r)
prfn never {p,o:int} (a: ABORT(p, o)): [o != END_COMMIT] void = abort_never_commits(a)

implement main0 () = ()
