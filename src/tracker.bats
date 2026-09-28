(* tracker -- the library's entry point: the regions an app declares,
   the pointers down (at most SLOTS, each a linear pointer), long-press
   on tick, pinch, and the semantic events. gestures_step is total over
   its inputs: a move, up or cancel for a pointer it does not hold is
   ignored, as is a down beyond the slots. *)

#include "share/atspre_staload.hats"

staload "./consts.sats"
staload "./classify.sats"
staload "./pointer.sats"

(* ============================================================
   Regions
   ============================================================ *)

(* The axes a region owns *)
#pub datatype axes = AxNone | AxH | AxV | AxBoth

(* The touch-action a region with these axes (and pinch or not) must
   declare, so that the browser leaves those gestures to the library:
   pan-y for a horizontal drag, pan-x for a vertical one, none for both
   or for pinch *)
#pub fn touch_action (a: axes, pinch: bool): [n:pos | n <= 5] string n

implement touch_action (a, pinch) =
  if pinch then "none"
  else case+ a of
  | AxNone() => "auto"
  | AxH() => "pan-y"
  | AxV() => "pan-x"
  | AxBoth() => "none"

(* Which pointers a region's gestures take: every kind, or touch and pen
   only (a mouse drag there stays the browser's: text selection) *)
#pub datatype devices = DevAll | DevTouch

(* A region: its id, the region it is in (-1 for none), what it owns,
   and whether a mouse may make its gestures *)
#pub typedef rdef = @{id = int, parent = int, h = bool, v = bool, pinch = bool, lp = bool, mouse = bool}

(* What a pointer asks of a region *)
#pub datatype want = WantH | WantV | WantLongPress | WantPinch

fn _owns (r: rdef, w: want): bool =
  case+ w of
  | WantH() => r.h
  | WantV() => r.v
  | WantLongPress() => r.lp
  | WantPinch() => r.pinch

fun _has_region {n:nat} .<n>. (xs: !list_vt(rdef, n), id: int): bool =
  case+ xs of
  | list_vt_nil() => false
  | list_vt_cons(r, rest) => if r.id = id then true else _has_region(rest, id)

(* The innermost region, from region id outwards, that owns w, or -1
   (also when that region takes no mouse and this pointer is one). A
   region's parent was declared before it, so it is further down the
   list (newest first): the walk out is a walk down *)
fun _owner {n:nat} .<n>. (xs: !list_vt(rdef, n), id: int, w: want, mouse: bool): int =
  case+ xs of
  | list_vt_nil() => ~1
  | list_vt_cons(r, rest) =>
    if r.id <> id then _owner(rest, id, w, mouse)
    else if _owns(r, w) then (if mouse && ~r.mouse then ~1 else id)
    else if r.parent < 0 then ~1
    else _owner(rest, r.parent, w, mouse)

fun _rdefs_free {n:nat} .<n>. (xs: list_vt(rdef, n)): void =
  case+ xs of
  | ~list_vt_nil() => ()
  | ~list_vt_cons(_, rest) => _rdefs_free(rest)

(* ============================================================
   Inputs and events
   ============================================================ *)

(* A pointer's kind *)
#pub stadef KIND_TOUCH = 0
#pub stadef KIND_MOUSE = 1
#pub stadef KIND_PEN = 2

(* An input: pointer events (id, x, y, t; a down also has the pointer's
   kind, the id of the innermost region hit (-1 for none) and the
   viewport's width), the per-frame tick, a cancel of every pointer,
   a region's rendered offset (x, y) while a transition is in flight,
   and the native-owned ends a region is told of *)
#pub datavtype input =
  | IDown of (int, coord, coord, stamp, int, int, int)
  | IMove of (int, coord, coord, stamp)
  | IUp of (int, coord, coord, stamp)
  | ICancel of (int)
  | ITick of (stamp)
  | ICancelAll of ()
  | IRenderedOffset of (int, int, int)
  | IScrollEnd of (int, int)
  | ITransitionEnd of (int)
  | ITransitionCancel of (int)

(* A semantic event, with its region's id first *)
#pub datavtype gevent =
  (* the displacement along the locked axis, from the lock point, plus
     the region's rendered offset when the drag began *)
  | GPan of (int, int)
  | GCommit of (int, dir)
  | GCancel of (int, dir)
  (* x, y *)
  | GLongPress of (int, int, int)
  (* scale in 1/SCALE_ONE, midpoint x, y *)
  | GPinch of (int, int, int, int)
  | GPinchEnd of (int)
  (* the scroll offset *)
  | GScrollEnd of (int, int)
  | GTransitionEnd of (int)
  | GTransitionCancel of (int)

#pub vtypedef gevents = [n:nat] list_vt(gevent, n)

#pub fn gevents_free (es: gevents): void

fun _events_free {n:nat} .<n>. (es: list_vt(gevent, n)): void =
  case+ es of
  | ~list_vt_nil() => ()
  | ~list_vt_cons(e, rest) => let
      val () = (case+ e of
        | ~GPan(_, _) => () | ~GCommit(_, _) => () | ~GCancel(_, _) => ()
        | ~GLongPress(_, _, _) => () | ~GPinch(_, _, _, _) => ()
        | ~GPinchEnd(_) => () | ~GScrollEnd(_, _) => ()
        | ~GTransitionEnd(_) => () | ~GTransitionCancel(_) => ())
    in _events_free(rest) end

implement gevents_free (es) = _events_free(es)

fun _reverse {n,m:nat} .<n>. (xs: list_vt(gevent, n), acc: list_vt(gevent, m)): list_vt(gevent, n + m) =
  case+ xs of
  | ~list_vt_nil() => acc
  | ~list_vt_cons(x, rest) => _reverse(rest, list_vt_cons(x, acc))

(* A drag's end, as an event (none when it never locked) *)
fn _ended {o:int} (e: ending(o), evs: gevents): gevents =
  case+ e of
  | ~EndNone() => evs
  | ~EndCommit(r, d) => list_vt_cons(GCommit(r, d), evs)
  | ~EndCancel(r, d) => list_vt_cons(GCancel(r, d), evs)

fn _panned {q:int} (p: panned(q), evs: gevents): gevents =
  case+ p of
  | ~Panned(r, d) => list_vt_cons(GPan(r, d), evs)
  | ~Still() => evs

(* ============================================================
   State
   ============================================================ *)

#pub vtypedef held = [p:int] pointer(p)

(* A pinch: its region, its two pointers, their distance (px) when it
   was set up, and whether it has begun *)
#pub typedef pinch = @{on = bool, reg = int, a = int, b = int, d0 = int, began = bool}

(* A region's rendered offset: region, x, y *)
#pub typedef roff = @(int, int, int)

#pub datavtype gstate =
  | GState of (
      [n:nat | n <= SLOTS] list_vt(held, n),
      [r:nat] list_vt(rdef, r),
      [k:nat] list_vt(roff, k),
      pinch)

fn _no_pinch (): pinch = let
  var c: pinch
  val () = c.on := false
  val () = c.reg := ~1
  val () = c.a := 0
  val () = c.b := 0
  val () = c.d0 := 0
  val () = c.began := false
in c end

#pub fn gestures_new (): gstate

implement gestures_new () = GState(list_vt_nil(), list_vt_nil(), list_vt_nil(), _no_pinch())

fun _held_free {n:nat} .<n>. (xs: list_vt(held, n)): void =
  case+ xs of
  | ~list_vt_nil() => ()
  | ~list_vt_cons(p, rest) => let
      val (_ | e) = pointer_abort(p)
      val () = (case+ e of ~EndNone() => () | ~EndCommit(_, _) => () | ~EndCancel(_, _) => ())
    in _held_free(rest) end

fun _roffs_free {n:nat} .<n>. (xs: list_vt(roff, n)): void =
  case+ xs of
  | ~list_vt_nil() => ()
  | ~list_vt_cons(_, rest) => _roffs_free(rest)

#pub fn gestures_free (st: gstate): void

implement gestures_free (st) =
  case+ st of
  | ~GState(ps, rs, os, _) => let
      val () = _held_free(ps)
      val () = _rdefs_free(rs)
    in _roffs_free(os) end

(* Declares region id, inside region parent (-1 for none), owning the
   axes a, pinch or not, long-press or not, for the pointers d. The
   parent must have been declared first (else the region is taken as
   outermost); an id declared already, or below 0, is ignored *)
#pub fn gestures_region (st: !gstate, id: int, parent: int, a: axes, pinch: bool, lp: bool, d: devices): void

fn _add_region {n:nat} (rs: list_vt(rdef, n), id: int, parent: int, a: axes, pinch: bool, lp: bool, d: devices): [m:nat] list_vt(rdef, m) =
  if id < 0 then rs
  else if _has_region(rs, id) then rs
  else let
    var r: rdef
    val () = r.id := id
    val () = r.parent := (if parent >= 0 then (if _has_region(rs, parent) then parent else ~1) else ~1)
    val () = r.h := (case+ a of AxH() => true | AxBoth() => true | _ => false)
    val () = r.v := (case+ a of AxV() => true | AxBoth() => true | _ => false)
    val () = r.pinch := pinch
    val () = r.lp := lp
    val () = r.mouse := (case+ d of DevAll() => true | DevTouch() => false)
  in list_vt_cons(r, rs) end

implement gestures_region (st, id, parent, a, pinch, lp, d) =
  case+ st of
  | @GState(_, rs, _, _) => let
      val () = rs := _add_region(rs, id, parent, a, pinch, lp, d)
      prval () = fold@(st)
    in end

(* ============================================================
   Pointers
   ============================================================ *)

(* Takes pointer id out of xs, if it is there *)
fun _take {n:nat} .<n>. (xs: list_vt(held, n), id: int):
  [b:bool | b2i(b) <= n] @(list_vt(held, n - b2i(b)), option_vt(held, b)) =
  case+ xs of
  | ~list_vt_nil() => @(list_vt_nil(), None_vt())
  | ~list_vt_cons(p, rest) =>
    if pointer_id(p) = id then @(rest, Some_vt(p))
    else let val @(r, o) = _take(rest, id) in @(list_vt_cons(p, r), o) end

fun _count {n:nat} .<n>. (xs: !list_vt(held, n)): int n =
  case+ xs of
  | list_vt_nil() => 0
  | list_vt_cons(_, rest) => 1 + _count(rest)

(* Where pointer id is, if it is held *)
fun _where {n:nat} .<n>. (xs: !list_vt(held, n), id: int, x: &int? >> int, y: &int? >> int): bool =
  case+ xs of
  | list_vt_nil() => let val () = x := 0 val () = y := 0 in false end
  | list_vt_cons(p, rest) =>
    if pointer_id(p) = id then let
        val i = pointer_info(p)
        val () = x := i.x
        val () = y := i.y
      in true end
    else _where(rest, id, x, y)

(* Whether a drag (a pointer not inert) is held *)
fun _dragging {n:nat} .<n>. (xs: !list_vt(held, n)): bool =
  case+ xs of
  | list_vt_nil() => false
  | list_vt_cons(p, rest) => if pointer_phase(p) <> 6 then true else _dragging(rest)

(* A held drag's id in pinch region r, or -1 *)
fun _in_pinch {n:nat} .<n>. (xs: !list_vt(held, n), r: int): int =
  case+ xs of
  | list_vt_nil() => ~1
  | list_vt_cons(p, rest) =>
    if pointer_phase(p) = 6 then _in_pinch(rest, r)
    else if (pointer_info(p)).pinchreg = r then pointer_id(p)
    else _in_pinch(rest, r)

(* Region r's rendered offset, taken out of the list *)
fun _take_off {n:nat} .<n>. (xs: list_vt(roff, n), r: int, x: &int >> int, y: &int >> int): [m:nat | m <= n] list_vt(roff, m) =
  case+ xs of
  | ~list_vt_nil() => list_vt_nil()
  | ~list_vt_cons(o, rest) =>
    if o.0 = r then let
        val () = x := o.1
        val () = y := o.2
      in _take_off(rest, r, x, y) end
    else list_vt_cons(o, _take_off(rest, r, x, y))

(* Region r's offset, gone *)
fun _drop_off {n:nat} .<n>. (xs: list_vt(roff, n), r: int): [m:nat | m <= n] list_vt(roff, m) =
  case+ xs of
  | ~list_vt_nil() => list_vt_nil()
  | ~list_vt_cons(o, rest) =>
    if o.0 = r then _drop_off(rest, r) else list_vt_cons(o, _drop_off(rest, r))

(* Long-presses every pointer due at t *)
fun _tick {n:nat} .<n>. (xs: list_vt(held, n), t: stamp, evs: gevents): @(list_vt(held, n), gevents) =
  case+ xs of
  | ~list_vt_nil() => @(list_vt_nil(), evs)
  | ~list_vt_cons(p, rest) => let
      val @(rest, evs) = _tick(rest, t, evs)
    in
      if pointer_long_due(p, t) then let
          val ph = pointer_phase(p)
        in
          if ph = 0 then let
              val i = pointer_info(p)
              val p = pointer_long_press(p)
            in @(list_vt_cons(p, rest), list_vt_cons(GLongPress(i.lpreg, i.x, i.y), evs)) end
          else @(list_vt_cons(p, rest), evs)
        end
      else @(list_vt_cons(p, rest), evs)
    end

(* Cancels every pointer *)
fun _abort_all {n:nat} .<n>. (xs: list_vt(held, n), evs: gevents): gevents =
  case+ xs of
  | ~list_vt_nil() => evs
  | ~list_vt_cons(p, rest) => let
      val (_ | e) = pointer_abort(p)
    in _abort_all(rest, _ended(e, evs)) end

(* Retires pointer id (a pinch takes it over) *)
fun _retire {n:nat} .<n>. (xs: list_vt(held, n), id: int, evs: gevents): @(list_vt(held, n), gevents) =
  case+ xs of
  | ~list_vt_nil() => @(list_vt_nil(), evs)
  | ~list_vt_cons(p, rest) =>
    if pointer_id(p) = id then let
        val (_ | p, e) = pointer_retire(p)
      in @(list_vt_cons(p, rest), _ended(e, evs)) end
    else let
        val @(rest, evs) = _retire(rest, id, evs)
      in @(list_vt_cons(p, rest), evs) end

(* ============================================================
   Pinch
   ============================================================ *)

(* floor(sqrt(v)), for 0 <= v, found in [lo, hi) *)
fun _isqrt {lo,hi:nat | lo < hi} .<hi - lo>. (v: int, lo: int lo, hi: int hi): int =
  if hi - lo <= 1 then lo
  else let
    val mid = lo + (hi - lo) / 2
  in if mid * mid <= v then _isqrt(v, mid, hi) else _isqrt(v, lo, mid) end

fn _clampc (v: int): int =
  if v > 262144 then 262144 else if v < ~262144 then ~262144 else v

(* The distance, in px, between two positions (1/16 px, within
   COORD_MAX, so each difference is at most 2^14 px and the sum of
   their squares under 2^29) *)
fn _dist_px (x1: int, y1: int, x2: int, y2: int): int = let
  val dx = _clampc(x2 - x1) / 16
  val dy = _clampc(y2 - y1) / 16
in _isqrt(dx * dx + dy * dy, 0, 32768) end

(* The pinch after a move of one of its pointers *)
fn _pinch_moved {n:nat} (xs: !list_vt(held, n), c: &pinch >> pinch, evs: gevents): gevents = let
  var x1: int
  var y1: int
  var x2: int
  var y2: int
  val ha = _where(xs, c.a, x1, y1)
  val hb = _where(xs, c.b, x2, y2)
in
  if ~(ha && hb) then evs
  else let
    val d = _dist_px(x1, y1, x2, y2)
    val dd = d - c.d0
    val () = (if c.began then () else if dd * 16 > slop() then c.began := true
              else if ~dd * 16 > slop() then c.began := true else ())
    val d0 = (if c.d0 > 0 then c.d0 else 1): int
  in
    if c.began then
      list_vt_cons(GPinch(c.reg, d * scale_one() / d0, x1 + (x2 - x1) / 2, y1 + (y2 - y1) / 2), evs)
    else evs
  end
end

(* The pinch ends, when id is one of its pointers *)
fn _pinch_gone (c: &pinch >> pinch, id: int, evs: gevents): gevents =
  if ~c.on then evs
  else if id <> c.a && id <> c.b then evs
  else let
    val began = c.began
    val reg = c.reg
    val () = c := _no_pinch()
  in if began then list_vt_cons(GPinchEnd(reg), evs) else evs end

(* ============================================================
   The entry point
   ============================================================ *)

fn _down (st: !gstate, id: int, x: coord, y: coord, t: stamp, kind: int, reg: int, vw: int, evs: gevents): gevents =
  case+ st of
  | @GState(ps, rs, os, c) => let
      val @(ps2, old) = _take(ps, id)
    in
      case+ old of
      (* a second down for a pointer held: the host's sequence is wrong;
         the pointer stays as it was *)
      | ~Some_vt(p) => let
          val () = ps := list_vt_cons(p, ps2)
          prval () = fold@(st)
        in evs end
      | ~None_vt() => let
          val n = _count(ps2)
        in
          if n >= 4 then let
              (* beyond the slots: ignored *)
              val () = ps := ps2
              prval () = fold@(st)
            in evs end
          else let
            val mouse = (kind = 1)
            val pr = _owner(rs, reg, WantPinch(), mouse)
            val other = (if pr >= 0 then (if c.on then ~1 else _in_pinch(ps2, pr)) else ~1): int
          in
            if other >= 0 then let
                (* a pinch takes over: the drag there ends *)
                val @(ps3, evs) = _retire(ps2, other, evs)
                var ox: int
                var oy: int
                val ho = _where(ps3, other, ox, oy)
                val d0 = (if ho then _dist_px(ox, oy, x, y) else 0): int
                val () = c.on := true
                val () = c.reg := pr
                val () = c.a := other
                val () = c.b := id
                val () = c.d0 := d0
                val () = c.began := false
                val () = ps := list_vt_cons(pointer_inert(id, kind, x, y, t, pr), ps3)
                prval () = fold@(st)
              in evs end
            else if _dragging(ps2) then let
                (* a second pointer during a drag: ignored by it *)
                val () = ps := list_vt_cons(pointer_inert(id, kind, x, y, t, ~1), ps2)
                prval () = fold@(st)
              in evs end
            else let
              (* within the edge margin a horizontal drag cannot start *)
              val at_edge = (x < edge() || x > vw - edge()): bool
              val hr = (if at_edge then ~1 else _owner(rs, reg, WantH(), mouse)): int
              val vr = _owner(rs, reg, WantV(), mouse)
              val lr = _owner(rs, reg, WantLongPress(), mouse)
              var ox: int = 0
              var oy: int = 0
              var dummy: int = 0
              (* offsets are only kept for regions >= 0, so -1 takes none *)
              val () = os := _take_off(os, hr, ox, dummy)
              val () = os := _take_off(os, vr, dummy, oy)
              val p = pointer_new(id, kind, x, y, t, hr, vr, lr, pr, ox, oy)
              val () = ps := list_vt_cons(p, ps2)
              prval () = fold@(st)
            in evs end
          end
        end
    end

fn _move (st: !gstate, id: int, x: coord, y: coord, t: stamp, evs: gevents): gevents =
  case+ st of
  | @GState(ps, _, _, c) => let
      val @(ps2, found) = _take(ps, id)
    in
      case+ found of
      | ~None_vt() => let
          val () = ps := ps2
          prval () = fold@(st)
        in evs end
      | ~Some_vt(p) => let
          val (_ | p, pn) = pointer_move(p, x, y, t)
          val evs = _panned(pn, evs)
          val ps3 = list_vt_cons(p, ps2)
          val evs = (if c.on then (if id = c.a || id = c.b then _pinch_moved(ps3, c, evs) else evs) else evs): gevents
          val () = ps := ps3
          prval () = fold@(st)
        in evs end
    end

fn _up (st: !gstate, id: int, x: coord, y: coord, t: stamp, evs: gevents): gevents =
  case+ st of
  | @GState(ps, _, _, c) => let
      val @(ps2, found) = _take(ps, id)
      val () = ps := ps2
    in
      case+ found of
      | ~None_vt() => let prval () = fold@(st) in evs end
      | ~Some_vt(p) => let
          val evs = _pinch_gone(c, id, evs)
          val (_ | e) = pointer_release(p, x, y, t)
          prval () = fold@(st)
        in _ended(e, evs) end
    end

fn _cancel (st: !gstate, id: int, evs: gevents): gevents =
  case+ st of
  | @GState(ps, _, _, c) => let
      val @(ps2, found) = _take(ps, id)
      val () = ps := ps2
    in
      case+ found of
      | ~None_vt() => let prval () = fold@(st) in evs end
      | ~Some_vt(p) => let
          val evs = _pinch_gone(c, id, evs)
          val (_ | e) = pointer_abort(p)
          prval () = fold@(st)
        in _ended(e, evs) end
    end

fn _cancel_all (st: !gstate, evs: gevents): gevents =
  case+ st of
  | @GState(ps, _, _, c) => let
      val evs = (if c.on then (if c.began then list_vt_cons(GPinchEnd(c.reg), evs) else evs) else evs): gevents
      val () = c := _no_pinch()
      val evs = _abort_all(ps, evs)
      val () = ps := list_vt_nil()
      prval () = fold@(st)
    in evs end

fn _tick_all (st: !gstate, t: stamp, evs: gevents): gevents =
  case+ st of
  | @GState(ps, _, _, _) => let
      val @(ps2, evs) = _tick(ps, t, evs)
      val () = ps := ps2
      prval () = fold@(st)
    in evs end

fn _set_off (st: !gstate, r: int, x: int, y: int): void =
  if r < 0 then () else
  case+ st of
  | @GState(_, _, os, _) => let
      val o: roff = @(r, x, y)
      val rest = _drop_off(os, r)
      val () = os := list_vt_cons{roff}(o, rest)
      prval () = fold@(st)
    in end

fn _clear_off (st: !gstate, r: int): void =
  case+ st of
  | @GState(_, _, os, _) => let
      val () = os := _drop_off(os, r)
      prval () = fold@(st)
    in end

(* The events one input makes, in order. Total: every input is handled,
   and none can fault *)
#pub fn gestures_step (st: !gstate, i: input): gevents

implement gestures_step (st, i) = let
  val evs = (case+ i of
    | ~IDown(id, x, y, t, kind, reg, vw) => _down(st, id, x, y, t, kind, reg, vw, list_vt_nil())
    | ~IMove(id, x, y, t) => _move(st, id, x, y, t, list_vt_nil())
    | ~IUp(id, x, y, t) => _up(st, id, x, y, t, list_vt_nil())
    | ~ICancel(id) => _cancel(st, id, list_vt_nil())
    | ~ITick(t) => _tick_all(st, t, list_vt_nil())
    | ~ICancelAll() => _cancel_all(st, list_vt_nil())
    | ~IRenderedOffset(r, x, y) => let val () = _set_off(st, r, x, y) in list_vt_nil() end
    | ~IScrollEnd(r, o) => list_vt_cons(GScrollEnd(r, o), list_vt_nil())
    | ~ITransitionEnd(r) => let val () = _clear_off(st, r) in list_vt_cons(GTransitionEnd(r), list_vt_nil()) end
    | ~ITransitionCancel(r) => let val () = _clear_off(st, r) in list_vt_cons(GTransitionCancel(r), list_vt_nil()) end
    ): gevents
in _reverse(evs, list_vt_nil()) end
