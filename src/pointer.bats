(* pointer -- one pointer's drag recognizer: slop, dead zone, axis lock,
   pan, and commit or cancel at release. A pointer is a linear value,
   made at its down and consumed at its up or cancel; its phase is in
   its type (a datasort), and the type of each step says which phase can
   follow which. Proven here: a locked axis never changes before release; a
   locked drag ends in exactly one of commit or cancel; one that never
   locked ends in neither; a cancel never commits. *)

#include "share/atspre_staload.hats"

staload "./consts.sats"
staload "./classify.sats"

(* A pointer's kind, as the host says it *)
#pub datatype pointer_kind =
  | Touch
  | Mouse
  | Pen

(* The region a gesture belongs to, by its id, or none (no region there
   owns it). Linear: InRegion is a cell, freed by region_free. *)
#pub datavtype region =
  | NoRegion
  | InRegion of (int)

#pub fn region_free (r: region): void

implement region_free (r) =
  case+ r of
  | ~NoRegion() => ()
  | ~InRegion(_) => ()

(* Whether r is the region id *)
#pub fn region_is (r: !region, id: int): bool

implement region_is (r, id) =
  case+ r of
  | NoRegion() => false
  | InRegion(owner) => owner = id

(* The phases *)
#pub datasort phase =
  | WithinSlop   (* within slop so far: may long-press *)
  | DeadZone     (* past slop, in the dead zone: nothing yet *)
  | LockedH      (* locked on the horizontal axis *)
  | LockedV      (* locked on the vertical axis *)
  | Rejected     (* an axis its region does not own: ignored *)
  | LongPressed  (* long-pressed: cannot drag *)
  | Inert        (* not a drag (an extra pointer, a pinch's) *)

(* A phase, as a value *)
#pub datatype phase_is(phase) =
  | AtWithinSlop(WithinSlop)
  | AtDeadZone(DeadZone)
  | AtLockedH(LockedH)
  | AtLockedV(LockedV)
  | AtRejected(Rejected)
  | AtLongPressed(LongPressed)
  | AtInert(Inert)

(* LOCKED(p): p is a locked phase; UNLOCKED(p): it is one of the others *)
#pub dataprop LOCKED(phase) =
  | LockedOnH(LockedH)
  | LockedOnV(LockedV)

#pub dataprop UNLOCKED(phase) =
  | UnlockedWithinSlop(WithinSlop)
  | UnlockedDeadZone(DeadZone)
  | UnlockedRejected(Rejected)
  | UnlockedLongPressed(LongPressed)
  | UnlockedInert(Inert)

(* No phase is both *)
#pub prfn locked_unlocked {p:phase} (l: LOCKED(p), u: UNLOCKED(p)): [false] void

primplement locked_unlocked {p} (l, u) =
  case+ l of
  | LockedOnH() => (case+ u of
    | UnlockedWithinSlop() =/=> () | UnlockedDeadZone() =/=> () | UnlockedRejected() =/=> ()
    | UnlockedLongPressed() =/=> () | UnlockedInert() =/=> ())
  | LockedOnV() => (case+ u of
    | UnlockedWithinSlop() =/=> () | UnlockedDeadZone() =/=> () | UnlockedRejected() =/=> ()
    | UnlockedLongPressed() =/=> () | UnlockedInert() =/=> ())

(* DECIDING(p): a move in phase p decides the axis (within slop, or in
   the dead zone) *)
#pub dataprop DECIDING(phase) =
  | DecidingWithinSlop(WithinSlop)
  | DecidingDeadZone(DeadZone)

#pub prfn deciding_unlocked {p:phase} (d: DECIDING(p)): UNLOCKED(p)

primplement deciding_unlocked {p} (d) =
  case+ d of
  | DecidingWithinSlop() => UnlockedWithinSlop()
  | DecidingDeadZone() => UnlockedDeadZone()

(* STAYING(p): a move in phase p leaves it there *)
#pub dataprop STAYING(phase) =
  | StayingLockedH(LockedH)
  | StayingLockedV(LockedV)
  | StayingRejected(Rejected)
  | StayingLongPressed(LongPressed)
  | StayingInert(Inert)

(* DECIDED(c): c is an axis, not the dead zone *)
#pub dataprop DECIDED(axis_class) =
  | DecidedH(Horizontal)
  | DecidedV(Vertical)

(* SAME_PHASE(p, q): p and q are one phase *)
#pub dataprop SAME_PHASE(phase, phase) =
  | {p:phase} SamePhase(p, p)

(* MOVE(p, q): a move takes a pointer from phase p to phase q *)
#pub dataprop MOVE(phase, phase) =
  | MVwithin(WithinSlop, WithinSlop)
  | {p:phase}{dx,dy:int}
    MVamb(p, DeadZone) of (DECIDING(p), CLASS(dx, dy, Ambiguous))
  | {p:phase}{dx,dy:int}
    MVlockh(p, LockedH) of (DECIDING(p), CLASS(dx, dy, Horizontal))
  | {p:phase}{dx,dy:int}
    MVlockv(p, LockedV) of (DECIDING(p), CLASS(dx, dy, Vertical))
  | {p:phase}{dx,dy:int}{c:axis_class}
    MVrej(p, Rejected) of (DECIDING(p), CLASS(dx, dy, c), DECIDED(c))
  | {p:phase} MVstay(p, p) of (STAYING(p))

(* Once an axis is locked, no move changes it *)
#pub prfn lock_stable {p,q:phase} (l: LOCKED(p), m: MOVE(p, q)): SAME_PHASE(q, p)

primplement lock_stable {p,q} (l, m) =
  case+ m of
  | MVstay(_) => SamePhase()
  | MVwithin() =/=> locked_unlocked(l, UnlockedWithinSlop())
  | MVamb(d, _) =/=> locked_unlocked(l, deciding_unlocked(d))
  | MVlockh(d, _) =/=> locked_unlocked(l, deciding_unlocked(d))
  | MVlockv(d, _) =/=> locked_unlocked(l, deciding_unlocked(d))
  | MVrej(d, _, _) =/=> locked_unlocked(l, deciding_unlocked(d))

(* Why a pointer is locked horizontally after a move: it already was,
   or a horizontal displacement decided it *)
#pub dataprop LOCK_H_CAUSE(phase) =
  | WasLockedH(LockedH)
  | {p:phase}{dx,dy:int} SawHorizontal(p) of (DECIDING(p), CLASS(dx, dy, Horizontal))

(* A pointer locks horizontally only on a horizontal displacement *)
#pub prfn lock_is_class {p:phase} (m: MOVE(p, LockedH)): LOCK_H_CAUSE(p)

primplement lock_is_class {p} (m) =
  case+ m of
  | MVstay(_) => WasLockedH()
  | MVlockh(d, c) => SawHorizontal(d, c)

(* How a drag ends *)
#pub datasort drag_end =
  | NoEnd
  | Committed
  | Cancelled

(* RELEASE(p, o): a pointer released in phase p ends with o *)
#pub dataprop RELEASE(phase, drag_end) =
  | {p:phase} RLcommit(p, Committed) of (LOCKED(p))
  | {p:phase} RLcancel(p, Cancelled) of (LOCKED(p))
  | {p:phase} RLnone(p, NoEnd) of (UNLOCKED(p))

(* LOCKED_END(o): o is a commit or a cancel *)
#pub dataprop LOCKED_END(drag_end) =
  | EndedCommitted(Committed)
  | EndedCancelled(Cancelled)

(* A locked drag ends in exactly one of commit or cancel *)
#pub prfn locked_ends {p:phase}{o:drag_end} (l: LOCKED(p), r: RELEASE(p, o)): LOCKED_END(o)

primplement locked_ends {p}{o} (l, r) =
  case+ r of
  | RLcommit(_) => EndedCommitted()
  | RLcancel(_) => EndedCancelled()
  | RLnone(u) =/=> locked_unlocked(l, u)

(* SILENT(o): o is no end *)
#pub dataprop SILENT(drag_end) =
  | Silent(NoEnd)

(* One that never locked ends in neither *)
#pub prfn unlocked_silent {p:phase}{o:drag_end} (u: UNLOCKED(p), r: RELEASE(p, o)): SILENT(o)

primplement unlocked_silent {p}{o} (u, r) =
  case+ r of
  | RLnone(_) => Silent()
  | RLcommit(l) =/=> locked_unlocked(l, u)
  | RLcancel(l) =/=> locked_unlocked(l, u)

(* ABORT(p, o): a pointer cancelled in phase p ends with o *)
#pub dataprop ABORT(phase, drag_end) =
  | {p:phase} ABcancel(p, Cancelled) of (LOCKED(p))
  | {p:phase} ABnone(p, NoEnd) of (UNLOCKED(p))

(* NOT_COMMITTED(o): o is no end, or a cancel *)
#pub dataprop NOT_COMMITTED(drag_end) =
  | NotCommittedNone(NoEnd)
  | NotCommittedCancelled(Cancelled)

(* A cancel never commits *)
#pub prfn abort_never_commits {p:phase}{o:drag_end} (a: ABORT(p, o)): NOT_COMMITTED(o)

primplement abort_never_commits {p}{o} (a) =
  case+ a of
  | ABcancel(_) => NotCommittedCancelled()
  | ABnone(_) => NotCommittedNone()

(* A direction: only commit and cancel name one *)
#pub datatype dir = DLeft | DRight | DUp | DDown

(* A drag's end, as its region sees it *)
#pub datavtype ending(drag_end) =
  | EndNone(NoEnd)
  | EndCommit(Committed) of (int, dir)
  | EndCancel(Cancelled) of (int, dir)

(* A move's pan: only a locked pointer pans, by its displacement along
   its axis from where it locked (plus the region's rendered offset) *)
#pub datavtype panned(phase) =
  | {q:phase} Panned(q) of (LOCKED(q) | int, int)
  | {q:phase} Still(q) of (UNLOCKED(q) | )

(* A sample for the release velocity: time, x, y *)
#pub typedef sample = @(int, int, int)

(* What a pointer knows *)
#pub typedef pinfo = @{
  id = int,
  x0 = coord, y0 = coord, t0 = stamp,
  x = coord, y = coord,
  lx = coord, ly = coord, lsign = int,
  ox = int, oy = int
}

(* The regions that own an unlocked pointer's horizontal drag, vertical
   drag and long-press: the innermost that own them where it went down
   (none for the horizontal one at a screen edge) *)
#pub datavtype owners =
  | Owners of (region, region, region)

fn _owners_free (owners: owners): void = let
  val+ ~Owners(horizontal, vertical, long_press) = owners
  val () = region_free(horizontal)
  val () = region_free(vertical)
in region_free(long_press) end

fn _no_owners (): owners = Owners(NoRegion(), NoRegion(), NoRegion())

(* The phase of a pointer that has not locked, as a value *)
#pub datatype free_phase(phase) =
  | FreeWithinSlop(WithinSlop)
  | FreeDeadZone(DeadZone)
  | FreeRejected(Rejected)
  | FreeLongPressed(LongPressed)
  | FreeInert(Inert)

(* The axis a locked pointer locked on, as a value *)
#pub datatype lock_axis(phase) =
  | AxisH(LockedH)
  | AxisV(LockedV)

(* A pointer in phase p. One that has not locked keeps the regions that
   may own it; a locked one keeps the region it locked in. Each keeps its
   pinch region (none when no region there owns pinch) *)
#pub datavtype pointer(phase) =
  | {p:phase} Free(p) of (UNLOCKED(p) | free_phase(p), pinfo, owners, region, [n:nat] list_vt(sample, n))
  | {p:phase} Locked(p) of (LOCKED(p) | lock_axis(p), pinfo, int, region, [n:nat] list_vt(sample, n))

fun _samples_free {n:nat} .<n>. (xs: list_vt(sample, n)): void =
  case+ xs of
  | ~list_vt_nil() => ()
  | ~list_vt_cons(_, rest) => _samples_free(rest)

(* The samples at or after tmin, at most k of them (newest first) *)
fun _prune {n:nat}{k:nat} .<n>.
  (xs: list_vt(sample, n), tmin: int, k: int k): [m:nat | m <= n] list_vt(sample, m) =
  case+ xs of
  | ~list_vt_nil() => list_vt_nil()
  | ~list_vt_cons(s, rest) =>
    if k > 0 then
      (if s.0 >= tmin then list_vt_cons(s, _prune(rest, tmin, k - 1))
       else let val () = _samples_free(rest) in list_vt_nil() end)
    else let val () = _samples_free(rest) in list_vt_nil() end

fn _record {n:nat} (xs: list_vt(sample, n), t: int, x: int, y: int): [m:nat] list_vt(sample, m) =
  _prune(list_vt_cons(@(t, x, y), xs), t - vel_window(), vel_samples())

(* The oldest sample, and how many there are *)
fun _oldest {n:pos} .<n>. (xs: !list_vt(sample, n)): sample =
  case+ xs of
  | list_vt_cons(s, rest) =>
    (case+ rest of
     | list_vt_nil() => s
     | list_vt_cons(_, _) => _oldest(rest))

#pub fn pointer_new
  (id: int, x: coord, y: coord, t: stamp,
   horizontal: region, vertical: region, long_press: region, pinch: region, ox: int, oy: int): pointer(WithinSlop)

fn _info (id: int, x: coord, y: coord, t: stamp, ox: int, oy: int): pinfo = let
  var i: pinfo
  val () = i.id := id
  val () = i.x0 := x
  val () = i.y0 := y
  val () = i.t0 := t
  val () = i.x := x
  val () = i.y := y
  val () = i.lx := x
  val () = i.ly := y
  val () = i.lsign := 0
  val () = i.ox := ox
  val () = i.oy := oy
in i end

implement pointer_new (id, x, y, t, horizontal, vertical, long_press, pinch, ox, oy) =
  Free(UnlockedWithinSlop() | FreeWithinSlop(), _info(id, x, y, t, ox, oy),
       Owners(horizontal, vertical, long_press), pinch, list_vt_cons(@(t, x, y), list_vt_nil()))

(* A pointer that is not a drag: an extra pointer, or a pinch's *)
#pub fn pointer_inert (id: int, x: coord, y: coord, t: stamp, pinch: region): pointer(Inert)

implement pointer_inert (id, x, y, t, pinch) =
  Free(UnlockedInert() | FreeInert(), _info(id, x, y, t, 0, 0), _no_owners(), pinch,
       list_vt_cons(@(t, x, y), list_vt_nil()))

#pub fn pointer_id {p:phase} (pt: !pointer(p)): int

implement pointer_id {p} (pt) =
  case+ pt of
  | Free(_ | _, i, _, _, _) => i.id
  | Locked(_ | _, i, _, _, _) => i.id

#pub fn pointer_phase {p:phase} (pt: !pointer(p)): phase_is(p)

implement pointer_phase {p} (pt) =
  case+ pt of
  | Free(_ | phase, _, _, _, _) => (case+ phase of
    | FreeWithinSlop() => AtWithinSlop()
    | FreeDeadZone() => AtDeadZone()
    | FreeRejected() => AtRejected()
    | FreeLongPressed() => AtLongPressed()
    | FreeInert() => AtInert())
  | Locked(_ | axis, _, _, _, _) => (case+ axis of
    | AxisH() => AtLockedH()
    | AxisV() => AtLockedV())

#pub fn pointer_info {p:phase} (pt: !pointer(p)): pinfo

implement pointer_info {p} (pt) =
  case+ pt of
  | Free(_ | _, i, _, _, _) => i
  | Locked(_ | _, i, _, _, _) => i

(* Whether a pointer in this phase is a drag (every phase but inert) *)
#pub fn phase_is_drag {p:phase} (phase: phase_is(p)): bool

implement phase_is_drag {p} (phase) =
  case+ phase of
  | AtWithinSlop() => true
  | AtDeadZone() => true
  | AtLockedH() => true
  | AtLockedV() => true
  | AtRejected() => true
  | AtLongPressed() => true
  | AtInert() => false

(* Whether the pointer is a drag in pinch region id *)
#pub fn pointer_in_pinch {p:phase} (pt: !pointer(p), id: int): bool

implement pointer_in_pinch {p} (pt, id) =
  case+ pt of
  | @Free(unlocked | phase, _, _, pinch, _) => let
      val drag = (case+ phase of
        | FreeWithinSlop() => true | FreeDeadZone() => true | FreeRejected() => true
        | FreeLongPressed() => true | FreeInert() => false): bool
      val found = drag && region_is(pinch, id)
      prval () = fold@(pt)
    in found end
  | @Locked(locked | _, _, _, pinch, _) => let
      val found = region_is(pinch, id)
      prval () = fold@(pt)
    in found end

(* Whether (dx, dy) is past the slop: max(|dx|, |dy|) first, so the
   squares are only taken of numbers at most SLOP *)
#pub fn beyond_slop {dx,dy:int} (dx: int dx, dy: int dy): bool

implement beyond_slop {dx,dy} (dx, dy) = let
  val (_ | ax) = absv(dx)
  val (_ | ay) = absv(dy)
in
  if ax > slop() then true
  else if ay > slop() then true
  else ax * ax + ay * ay > slop() * slop()
end

fn _sign (d: int): int = if d > 0 then 1 else if d < 0 then ~1 else 0

(* The axis a move past slop (or in the dead zone) decides: a lock when
   the region where it went down owns that axis, else rejected *)
fn _decide {p:phase}{n:nat}
  (deciding: DECIDING(p) | i: pinfo, owners: owners, pinch: region, ss: list_vt(sample, n))
  : [q:phase] (MOVE(p, q) | pointer(q), panned(q)) = let
  val dx = i.x - i.x0
  val dy = i.y - i.y0
  val (class | c) = classify(dx, dy)
in
  case+ c of
  | IsHorizontal() => let
      val+ ~Owners(horizontal, vertical, long_press) = owners
      val () = region_free(vertical)
      val () = region_free(long_press)
    in
      case+ horizontal of
      | ~InRegion(owner) => let
          var j = i
          val () = j.lx := i.x
          val () = j.ly := i.y
          val () = j.lsign := _sign(dx)
        in (MVlockh(deciding, class) | Locked(LockedOnH() | AxisH(), j, owner, pinch, ss), Panned(LockedOnH() | owner, i.ox)) end
      | ~NoRegion() =>
        (MVrej(deciding, class, DecidedH()) | Free(UnlockedRejected() | FreeRejected(), i, _no_owners(), pinch, ss), Still(UnlockedRejected() | ))
    end
  | IsVertical() => let
      val+ ~Owners(horizontal, vertical, long_press) = owners
      val () = region_free(horizontal)
      val () = region_free(long_press)
    in
      case+ vertical of
      | ~InRegion(owner) => let
          var j = i
          val () = j.lx := i.x
          val () = j.ly := i.y
          val () = j.lsign := _sign(dy)
        in (MVlockv(deciding, class) | Locked(LockedOnV() | AxisV(), j, owner, pinch, ss), Panned(LockedOnV() | owner, i.oy)) end
      | ~NoRegion() =>
        (MVrej(deciding, class, DecidedV()) | Free(UnlockedRejected() | FreeRejected(), i, _no_owners(), pinch, ss), Still(UnlockedRejected() | ))
    end
  | IsAmbiguous() =>
    (MVamb(deciding, class) | Free(UnlockedDeadZone() | FreeDeadZone(), i, owners, pinch, ss), Still(UnlockedDeadZone() | ))
end

(* A move to (x, y) at t *)
#pub fn pointer_move {p:phase}
  (pt: pointer(p), x: coord, y: coord, t: stamp): [q:phase] (MOVE(p, q) | pointer(q), panned(q))

implement pointer_move {p} (pt, x, y, t) =
  case+ pt of
  | ~Locked(locked | axis, i0, owner, pinch, ss) => let
      val ss = _record(ss, t, x, y)
      var i = i0
      val () = i.x := x
      val () = i.y := y
    in
      case+ axis of
      | AxisH() => (MVstay(StayingLockedH()) | Locked(locked | axis, i, owner, pinch, ss), Panned(locked | owner, i.ox + (x - i.lx)))
      | AxisV() => (MVstay(StayingLockedV()) | Locked(locked | axis, i, owner, pinch, ss), Panned(locked | owner, i.oy + (y - i.ly)))
    end
  | ~Free(unlocked | phase, i0, owners, pinch, ss) => let
      val ss = _record(ss, t, x, y)
      var i = i0
      val () = i.x := x
      val () = i.y := y
      val dx = x - i.x0
      val dy = y - i.y0
    in
      case+ phase of
      | FreeWithinSlop() =>
        if beyond_slop(dx, dy) then _decide(DecidingWithinSlop() | i, owners, pinch, ss)
        else (MVwithin() | Free(unlocked | phase, i, owners, pinch, ss), Still(unlocked | ))
      | FreeDeadZone() => _decide(DecidingDeadZone() | i, owners, pinch, ss)
      | FreeRejected() => (MVstay(StayingRejected()) | Free(unlocked | phase, i, owners, pinch, ss), Still(unlocked | ))
      | FreeLongPressed() => (MVstay(StayingLongPressed()) | Free(unlocked | phase, i, owners, pinch, ss), Still(unlocked | ))
      | FreeInert() => (MVstay(StayingInert()) | Free(unlocked | phase, i, owners, pinch, ss), Still(unlocked | ))
    end

(* The direction of a drag on the axis of phase p: the sign of its
   displacement from where it went down, or of where it locked when
   that is 0 *)
fn _dir (horiz: bool, d: int, lsign: int): dir = let
  val s = (if d > 0 then 1 else if d < 0 then ~1 else lsign): int
in
  if horiz then (if s < 0 then DLeft() else DRight())
  else (if s < 0 then DUp() else DDown())
end

(* Whether a drag on an axis commits: its displacement d from where it
   went down is at least COMMIT_DIST in its direction s, or its
   velocity over the last VEL_WINDOW ms (from its oldest sample there
   to its release, at least VEL_SPAN ms apart) is at least COMMIT_VEL
   px/s in that same direction *)
fn _commits {n:nat} (d: int, s: int, xs: !list_vt(sample, n), horiz: bool, t: int, pos: int): bool =
  if s * d >= commit_dist() then true
  else case+ xs of
  | list_vt_nil() => false
  | list_vt_cons(_, _) => let
    val @(ot, ox, oy) = _oldest(xs)
    val dt = t - ot
    val po = (if horiz then ox else oy): int
    val dp = pos - po
  in
    (* the release sample is the newest, so dt >= VEL_SPAN also says
       there are at least 2 *)
    if dt < vel_span() then false
    else s * dp * 1000 >= commit_vel() * 16 * dt
  end

(* The direction of a locked drag, by its axis: the sign of its
   displacement from where it went down, or of where it locked when that
   is 0 *)
fn _locked_dir {p:phase} (axis: lock_axis(p), i: pinfo): dir =
  case+ axis of
  | AxisH() => _dir(true, i.x - i.x0, i.lsign)
  | AxisV() => _dir(false, i.y - i.y0, i.lsign)

(* Whether a locked drag released at (x, y) at t, in direction dr,
   commits *)
fn _release_commits {p:phase}{n:nat}
  (axis: lock_axis(p), i: pinfo, dr: dir, ss: !list_vt(sample, n), x: coord, y: coord, t: stamp): bool =
  case+ axis of
  | AxisH() => _commits(x - i.x0, (case+ dr of DLeft() => ~1 | DRight() => 1 | DUp() => 1 | DDown() => 1), ss, true, t, x)
  | AxisV() => _commits(y - i.y0, (case+ dr of DUp() => ~1 | DDown() => 1 | DLeft() => 1 | DRight() => 1), ss, false, t, y)

(* The pointer goes up at (x, y) at t: its drag commits or cancels if
   it locked, and ends silently if not *)
#pub fn pointer_release {p:phase}
  (pt: pointer(p), x: coord, y: coord, t: stamp): [o:drag_end] (RELEASE(p, o) | ending(o))

implement pointer_release {p} (pt, x, y, t) =
  case+ pt of
  | ~Locked(locked | axis, i, owner, pinch, ss) => let
      val () = region_free(pinch)
      val ss = _record(ss, t, x, y)
      val dr = _locked_dir(axis, i)
      val c = _release_commits(axis, i, dr, ss, x, y, t)
      val () = _samples_free(ss)
    in
      if c then (RLcommit(locked) | EndCommit(owner, dr))
      else (RLcancel(locked) | EndCancel(owner, dr))
    end
  | ~Free(unlocked | _, _, owners, pinch, ss) => let
      val () = _owners_free(owners)
      val () = region_free(pinch)
      val () = _samples_free(ss)
    in (RLnone(unlocked) | EndNone()) end

(* The pointer is cancelled (pointercancel, or the host lost it): a
   locked drag cancels; nothing ever commits *)
#pub fn pointer_abort {p:phase} (pt: pointer(p)): [o:drag_end] (ABORT(p, o) | ending(o))

implement pointer_abort {p} (pt) =
  case+ pt of
  | ~Locked(locked | axis, i, owner, pinch, ss) => let
      val () = region_free(pinch)
      val () = _samples_free(ss)
    in (ABcancel(locked) | EndCancel(owner, _locked_dir(axis, i))) end
  | ~Free(unlocked | _, _, owners, pinch, ss) => let
      val () = _owners_free(owners)
      val () = region_free(pinch)
      val () = _samples_free(ss)
    in (ABnone(unlocked) | EndNone()) end

(* A pinch takes the pointer over: its drag ends as a cancel would, and
   it stays down, inert *)
#pub fn pointer_retire {p:phase} (pt: pointer(p)): [o:drag_end] (ABORT(p, o) | pointer(Inert), ending(o))

implement pointer_retire {p} (pt) =
  case+ pt of
  | ~Locked(locked | axis, i, owner, pinch, ss) =>
    (ABcancel(locked) | Free(UnlockedInert() | FreeInert(), i, _no_owners(), pinch, ss), EndCancel(owner, _locked_dir(axis, i)))
  | ~Free(unlocked | _, i, owners, pinch, ss) => let
      val () = _owners_free(owners)
    in (ABnone(unlocked) | Free(UnlockedInert() | FreeInert(), i, _no_owners(), pinch, ss), EndNone()) end

(* Whether the pointer has been held within slop long enough to
   long-press, at t, where a region owns long-press *)
#pub fn pointer_long_due {p:phase} (pt: !pointer(p), t: stamp): bool

implement pointer_long_due {p} (pt, t) =
  case+ pt of
  | @Free(unlocked | phase, i, owners, _, _) => let
      val+ @Owners(_, _, long_press_owner) = owners
      val owned = (case+ long_press_owner of InRegion(_) => true | NoRegion() => false): bool
      prval () = fold@(owners)
      val start = i.t0
      val within_slop = (case+ phase of
        | FreeWithinSlop() => true | FreeDeadZone() => false | FreeRejected() => false
        | FreeLongPressed() => false | FreeInert() => false): bool
      prval () = fold@(pt)
    in
      within_slop && owned && t - start >= long_press()
    end
  | Locked(_ | _, _, _, _, _) => false

(* It long-presses: from now on it cannot drag. The region that owns its
   long-press is given back with it *)
#pub fn pointer_long_press (pt: pointer(WithinSlop)): @(pointer(LongPressed), region)

implement pointer_long_press (pt) =
  case+ pt of
  | ~Free(_ | _, i, owners, pinch, ss) => let
      val+ ~Owners(horizontal, vertical, long_press) = owners
      val () = region_free(horizontal)
      val () = region_free(vertical)
    in @(Free(UnlockedLongPressed() | FreeLongPressed(), i, _no_owners(), pinch, ss), long_press) end
  (* no locked pointer is within slop *)
  | ~Locked(_ | axis, _, _, pinch, ss) => let
      val () = region_free(pinch)
      val () = _samples_free(ss)
    in
      case+ axis of
      | AxisH() =/=> ()
      | AxisV() =/=> ()
    end
