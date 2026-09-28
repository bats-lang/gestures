(* pointer -- one pointer's drag recognizer: slop, dead zone, axis lock,
   pan, and commit or cancel at release. A pointer is a linear value,
   made at its down and consumed at its up or cancel; its phase is in
   its type, and the type of each step says which phase can follow
   which. Proven here: a locked axis never changes before release; a
   locked drag ends in exactly one of commit or cancel; one that never
   locked ends in neither; a cancel never commits. *)

#include "share/atspre_staload.hats"

staload "./consts.sats"
staload "./classify.sats"

(* The phases *)
#pub stadef PEND = 0   (* within slop so far: may long-press *)
#pub stadef AMB = 1    (* past slop, in the dead zone: nothing yet *)
#pub stadef LOCKH = 2  (* locked on the horizontal axis *)
#pub stadef LOCKV = 3  (* locked on the vertical axis *)
#pub stadef REJ = 4    (* an axis its region does not own: ignored *)
#pub stadef LP = 5     (* long-pressed: cannot drag *)
#pub stadef INERT = 6  (* not a drag (an extra pointer, a pinch's) *)

#pub stadef locked(p:int): bool = p == LOCKH || p == LOCKV

(* MOVE(p, q): a move takes a pointer from phase p to phase q *)
#pub dataprop MOVE(int, int) =
  | MVwithin(PEND, PEND)
  | {p:int | p == PEND || p == AMB}{dx,dy:int}
    MVamb(p, AMB) of CLASS(dx, dy, AMBIG)
  | {p:int | p == PEND || p == AMB}{dx,dy:int}
    MVlockh(p, LOCKH) of CLASS(dx, dy, HORIZ)
  | {p:int | p == PEND || p == AMB}{dx,dy:int}
    MVlockv(p, LOCKV) of CLASS(dx, dy, VERT)
  | {p:int | p == PEND || p == AMB}{dx,dy:int}{c:int | c != AMBIG}
    MVrej(p, REJ) of CLASS(dx, dy, c)
  | {p:int | p >= LOCKH} MVstay(p, p)

(* Once an axis is locked, no move changes it *)
#pub prfn lock_stable {p,q:int | locked(p)} (m: MOVE(p, q)): [q == p] void

primplement lock_stable {p,q} (m) =
  case+ m of
  | MVstay() => ()
  | MVwithin() =/=> ()
  | MVamb(_) =/=> ()
  | MVlockh(_) =/=> ()
  | MVlockv(_) =/=> ()
  | MVrej(_) =/=> ()

(* A pointer locks horizontally only on a horizontal displacement *)
#pub prfn lock_is_class {p:int} (m: MOVE(p, LOCKH)):
  [p == LOCKH || (p == PEND || p == AMB)] void

primplement lock_is_class {p} (m) =
  case+ m of
  | MVstay() => ()
  | MVlockh(_) => ()
  | MVwithin() =/=> ()
  | MVamb(_) =/=> ()
  | MVlockv(_) =/=> ()
  | MVrej(_) =/=> ()

(* How a drag ends *)
#pub stadef END_NONE = 0
#pub stadef END_COMMIT = 1
#pub stadef END_CANCEL = 2

(* RELEASE(p, o): a pointer released in phase p ends with o *)
#pub dataprop RELEASE(int, int) =
  | {p:int | locked(p)} RLcommit(p, END_COMMIT)
  | {p:int | locked(p)} RLcancel(p, END_CANCEL)
  | {p:int | ~locked(p)} RLnone(p, END_NONE)

(* A locked drag ends in exactly one of commit or cancel *)
#pub prfn locked_ends {p,o:int | locked(p)} (r: RELEASE(p, o)):
  [o == END_COMMIT || o == END_CANCEL] void

primplement locked_ends {p,o} (r) =
  case+ r of
  | RLcommit() => ()
  | RLcancel() => ()
  | RLnone() =/=> ()

(* One that never locked ends in neither *)
#pub prfn unlocked_silent {p,o:int | ~locked(p)} (r: RELEASE(p, o)): [o == END_NONE] void

primplement unlocked_silent {p,o} (r) =
  case+ r of
  | RLnone() => ()
  | RLcommit() =/=> ()
  | RLcancel() =/=> ()

(* ABORT(p, o): a pointer cancelled in phase p ends with o *)
#pub dataprop ABORT(int, int) =
  | {p:int | locked(p)} ABcancel(p, END_CANCEL)
  | {p:int | ~locked(p)} ABnone(p, END_NONE)

(* A cancel never commits *)
#pub prfn abort_never_commits {p,o:int} (a: ABORT(p, o)): [o != END_COMMIT] void

primplement abort_never_commits {p,o} (a) =
  case+ a of
  | ABcancel() => ()
  | ABnone() => ()

(* A direction: only commit and cancel name one *)
#pub datatype dir = DLeft | DRight | DUp | DDown

(* A drag's end, as its region sees it *)
#pub datavtype ending(int) =
  | EndNone(END_NONE)
  | EndCommit(END_COMMIT) of (int, dir)
  | EndCancel(END_CANCEL) of (int, dir)

(* A move's pan: only a locked pointer pans, by its displacement along
   its axis from where it locked (plus the region's rendered offset) *)
#pub datavtype panned(int) =
  | {q:int | locked(q)} Panned(q) of (int, int)
  | {q:int | ~locked(q)} Still(q)

(* A sample for the release velocity: time, x, y *)
#pub typedef sample = @(int, int, int)

(* What a pointer knows. The regions are the innermost that own its
   gestures where it went down, or -1 (a horizontal region is -1 at a
   screen edge) *)
#pub typedef pinfo = @{
  id = int, kind = int,
  hreg = int, vreg = int, lpreg = int, pinchreg = int,
  x0 = coord, y0 = coord, t0 = stamp,
  x = coord, y = coord,
  lx = coord, ly = coord, lsign = int,
  ox = int, oy = int
}

(* A pointer in phase p *)
#pub datavtype pointer(int) =
  | {p:int | p >= PEND; p <= INERT} Pointer(p) of (int p, pinfo, [n:nat] list_vt(sample, n))

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
  (id: int, kind: int, x: coord, y: coord, t: stamp,
   hreg: int, vreg: int, lpreg: int, pinchreg: int, ox: int, oy: int): pointer(PEND)

implement pointer_new (id, kind, x, y, t, hreg, vreg, lpreg, pinchreg, ox, oy) = let
  var i: pinfo
  val () = i.id := id
  val () = i.kind := kind
  val () = i.hreg := hreg
  val () = i.vreg := vreg
  val () = i.lpreg := lpreg
  val () = i.pinchreg := pinchreg
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
in Pointer(0, i, list_vt_cons(@(t, x, y), list_vt_nil())) end

(* A pointer that is not a drag: an extra pointer, or a pinch's *)
#pub fn pointer_inert (id: int, kind: int, x: coord, y: coord, t: stamp, pinchreg: int): pointer(INERT)

implement pointer_inert (id, kind, x, y, t, pinchreg) =
  case+ pointer_new(id, kind, x, y, t, ~1, ~1, ~1, pinchreg, 0, 0) of
  | ~Pointer(_, i, ss) => Pointer(6, i, ss)

#pub fn pointer_id {p:int} (pt: !pointer(p)): int

implement pointer_id {p} (pt) = case+ pt of Pointer(_, i, _) => i.id

#pub fn pointer_phase {p:int} (pt: !pointer(p)): int p

implement pointer_phase {p} (pt) = case+ pt of Pointer(ph, _, _) => ph

#pub fn pointer_info {p:int} (pt: !pointer(p)): pinfo

implement pointer_info {p} (pt) = case+ pt of Pointer(_, i, _) => i

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

(* A move to (x, y) at t *)
#pub fn pointer_move {p:int}
  (pt: pointer(p), x: coord, y: coord, t: stamp): [q:int] (MOVE(p, q) | pointer(q), panned(q))

implement pointer_move {p} (pt, x, y, t) =
  case+ pt of
  | ~Pointer(ph, i0, ss) => let
      val ss = _record(ss, t, x, y)
      var i = i0
      val () = i.x := x
      val () = i.y := y
      val dx = x - i.x0
      val dy = y - i.y0
      (* past slop (or already past it, in the dead zone): the axis *)
      fn decide {p:int | p == PEND || p == AMB} {n:nat}
        (ph: int p, i: pinfo, ss: list_vt(sample, n)): [q:int] (MOVE(p, q) | pointer(q), panned(q)) = let
        val dx = i.x - i.x0
        val dy = i.y - i.y0
        val (pc | c) = classify(dx, dy)
      in
        if c = 1 then
          (if i.hreg >= 0 then let
             var j = i
             val () = j.lx := i.x
             val () = j.ly := i.y
             val () = j.lsign := _sign(dx)
           in (MVlockh(pc) | Pointer(2, j, ss), Panned(i.hreg, i.ox)) end
           else (MVrej(pc) | Pointer(4, i, ss), Still()))
        else if c = 2 then
          (if i.vreg >= 0 then let
             var j = i
             val () = j.lx := i.x
             val () = j.ly := i.y
             val () = j.lsign := _sign(dy)
           in (MVlockv(pc) | Pointer(3, j, ss), Panned(i.vreg, i.oy)) end
           else (MVrej(pc) | Pointer(4, i, ss), Still()))
        else (MVamb(pc) | Pointer(1, i, ss), Still())
      end
    in
      if ph = 0 then
        (if beyond_slop(dx, dy) then decide(ph, i, ss)
         else (MVwithin() | Pointer(ph, i, ss), Still()))
      else if ph = 1 then decide(ph, i, ss)
      else if ph = 2 then (MVstay() | Pointer(ph, i, ss), Panned(i.hreg, i.ox + (x - i.lx)))
      else if ph = 3 then (MVstay() | Pointer(ph, i, ss), Panned(i.vreg, i.oy + (y - i.ly)))
      else (MVstay() | Pointer(ph, i, ss), Still())
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

(* The pointer goes up at (x, y) at t: its drag commits or cancels if
   it locked, and ends silently if not *)
#pub fn pointer_release {p:int}
  (pt: pointer(p), x: coord, y: coord, t: stamp): [o:int] (RELEASE(p, o) | ending(o))

implement pointer_release {p} (pt, x, y, t) =
  case+ pt of
  | ~Pointer(ph, i, ss) => let
      val ss = _record(ss, t, x, y)
    in
      if ph = 2 then let
          val d = x - i.x0
          val dr = _dir(true, d, i.lsign)
          val s = (case+ dr of DLeft() => ~1 | _ => 1): int
          val c = _commits(d, s, ss, true, t, x)
          val () = _samples_free(ss)
        in
          if c then (RLcommit() | EndCommit(i.hreg, dr))
          else (RLcancel() | EndCancel(i.hreg, dr))
        end
      else if ph = 3 then let
          val d = y - i.y0
          val dr = _dir(false, d, i.lsign)
          val s = (case+ dr of DUp() => ~1 | _ => 1): int
          val c = _commits(d, s, ss, false, t, y)
          val () = _samples_free(ss)
        in
          if c then (RLcommit() | EndCommit(i.vreg, dr))
          else (RLcancel() | EndCancel(i.vreg, dr))
        end
      else let val () = _samples_free(ss) in (RLnone() | EndNone()) end
    end

(* The ending a cancel gives a pointer in phase ph: a locked drag
   cancels, in the direction it had *)
fn _abort_ending {p:int | p >= PEND; p <= INERT} (ph: int p, i: pinfo): [o:int] (ABORT(p, o) | ending(o)) =
  if ph = 2 then (ABcancel() | EndCancel(i.hreg, _dir(true, i.x - i.x0, i.lsign)))
  else if ph = 3 then (ABcancel() | EndCancel(i.vreg, _dir(false, i.y - i.y0, i.lsign)))
  else (ABnone() | EndNone())

(* The pointer is cancelled (pointercancel, or the host lost it): a
   locked drag cancels; nothing ever commits *)
#pub fn pointer_abort {p:int} (pt: pointer(p)): [o:int] (ABORT(p, o) | ending(o))

implement pointer_abort {p} (pt) =
  case+ pt of
  | ~Pointer(ph, i, ss) => let
      val () = _samples_free(ss)
    in _abort_ending(ph, i) end

(* A pinch takes the pointer over: its drag ends as a cancel would, and
   it stays down, inert *)
#pub fn pointer_retire {p:int} (pt: pointer(p)): [o:int] (ABORT(p, o) | pointer(INERT), ending(o))

implement pointer_retire {p} (pt) =
  case+ pt of
  | ~Pointer(ph, i, ss) => let
      val (pa | e) = _abort_ending(ph, i)
    in (pa | Pointer(6, i, ss), e) end

(* Whether the pointer has been held within slop long enough to
   long-press, at t *)
#pub fn pointer_long_due {p:int} (pt: !pointer(p), t: stamp): bool

implement pointer_long_due {p} (pt, t) =
  case+ pt of
  | Pointer(ph, i, _) => ph = 0 && i.lpreg >= 0 && t - i.t0 >= long_press()

(* It long-presses: from now on it cannot drag *)
#pub fn pointer_long_press (pt: pointer(PEND)): pointer(LP)

implement pointer_long_press (pt) =
  case+ pt of
  | ~Pointer(_, i, ss) => Pointer(5, i, ss)
