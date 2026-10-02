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
prfn both_is_false {dx,dy:int} (h: CLASS(dx, dy, Horizontal), v: CLASS(dx, dy, Vertical)): [false] void =
  class_exclusive(h, v)
prfn flip_both {dx,dy:int}{c:axis_class} (p: CLASS(dx, dy, c)): CLASS(~dx, ~dy, c) =
  class_flip_y(class_flip_x(p))
prfn mirror {dx,dy:int}{c,d:axis_class} (p: CLASS(dx, dy, c), q: CLASS(~dx, dy, d)): SAME_CLASS(c, d) =
  class_unique(class_flip_x(p), q)
prval _ = deadzone_diagonal{7}()
prfn stays {q:phase} (m: MOVE(LockedH, q)): SAME_PHASE(q, LockedH) = lock_stable(LockedOnH(), m)
prfn ends {o:drag_end} (r: RELEASE(LockedV, o)): LOCKED_END(o) = locked_ends(LockedOnV(), r)
prfn quiet {o:drag_end} (r: RELEASE(DeadZone, o)): SILENT(o) = unlocked_silent(UnlockedDeadZone(), r)
prfn never {p:phase}{o:drag_end} (a: ABORT(p, o)): NOT_COMMITTED(o) = abort_never_commits(a)
prfn from_class {p:phase} (m: MOVE(p, LockedH)): LOCK_H_CAUSE(p) = lock_is_class(m)

implement main0 () = ()
