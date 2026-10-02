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

(* A region: its id, what it owns, and whether a mouse may make its
   gestures *)
#pub typedef rdef = @{id = int, h = bool, v = bool, pinch = bool, lp = bool, mouse = bool}

(* The regions declared, newest first: each with the region it is in *)
#pub datavtype regions(int) =
  | NoRegions(0)
  | {n:nat} RegionDef(n + 1) of (rdef, region, regions(n))

(* What a pointer asks of a region *)
#pub datatype want = WantH | WantV | WantLongPress | WantPinch

fn _owns (r: rdef, w: want): bool =
  case+ w of
  | WantH() => r.h
  | WantV() => r.v
  | WantLongPress() => r.lp
  | WantPinch() => r.pinch

fun _has_region {n:nat} .<n>. (xs: !regions(n), id: int): bool =
  case+ xs of
  | NoRegions() => false
  | @RegionDef(r, _, rest) => let
      val found = (if r.id = id then true else _has_region(rest, id)): bool
      prval () = fold@(xs)
    in found end

(* The innermost region, from region id outwards, that owns w, or none
   (also when that region takes no mouse and this pointer is one). A
   region's parent was declared before it, so it is further down the
   list (newest first): the walk out is a walk down *)
fun _owner {n:nat} .<n>. (xs: !regions(n), id: int, w: want, mouse: bool): region =
  case+ xs of
  | NoRegions() => NoRegion()
  | @RegionDef(r, parent, rest) => let
      val owner = (
        if r.id <> id then _owner(rest, id, w, mouse)
        else if _owns(r, w) then (if mouse && ~r.mouse then NoRegion() else InRegion(id))
        else (case+ parent of
          | InRegion(outer) => _owner(rest, outer, w, mouse)
          | NoRegion() => NoRegion())): region
      prval () = fold@(xs)
    in owner end

(* The owner of w from the region hit outwards; none when none was hit *)
fn _owner_of {n:nat} (xs: !regions(n), hit: !region, w: want, mouse: bool): region =
  case+ hit of
  | InRegion(id) => _owner(xs, id, w, mouse)
  | NoRegion() => NoRegion()

fun _regions_free {n:nat} .<n>. (xs: regions(n)): void =
  case+ xs of
  | ~NoRegions() => ()
  | ~RegionDef(_, parent, rest) => let
      val () = region_free(parent)
    in _regions_free(rest) end

(* ============================================================
   Inputs and events
   ============================================================ *)

(* An input: pointer events (id, x, y, t; a down also has the pointer's
   kind, the innermost region hit and the viewport's width), the
   per-frame tick, a cancel of every pointer, a region's rendered offset
   (x, y) while a transition is in flight, and the native-owned ends a
   region is told of *)
#pub datavtype input =
  | IDown of (int, coord, coord, stamp, pointer_kind, region, int)
  | IMove of (int, coord, coord, stamp)
  | IUp of (int, coord, coord, stamp)
  | ICancel of (int)
  | ITick of (stamp)
  | ICancelAll of ()
  | IRenderedOffset of ([r:nat] int r, int, int)
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
fn _ended {o:drag_end} (e: ending(o), evs: gevents): gevents =
  case+ e of
  | ~EndNone() => evs
  | ~EndCommit(r, d) => list_vt_cons(GCommit(r, d), evs)
  | ~EndCancel(r, d) => list_vt_cons(GCancel(r, d), evs)

fn _panned {q:phase} (p: panned(q), evs: gevents): gevents =
  case+ p of
  | ~Panned(_ | r, d) => list_vt_cons(GPan(r, d), evs)
  | ~Still(_ | ) => evs

(* ============================================================
   State
   ============================================================ *)

#pub vtypedef held = [p:phase] pointer(p)

(* A pinch, or none: its region, its two pointers, their distance (px)
   when it was set up, and whether it has begun *)
#pub datavtype pinch =
  | NoPinch
  | Pinching of (int, int, int, int, bool)

(* c becomes next; what it was is freed *)
fn _pinch_set (c: &pinch >> pinch, next: pinch): void = let
  val old = c
  val () = c := next
in
  case+ old of
  | ~NoPinch() => ()
  | ~Pinching(_, _, _, _, _) => ()
end

fn _pinch_clear (c: &pinch >> pinch): void = _pinch_set(c, NoPinch())

(* A region's rendered offset: region, x, y *)
#pub typedef roff = @(int, int, int)

#pub datavtype gstate =
  | GState of (
      [n:nat | n <= SLOTS] list_vt(held, n),
      [r:nat] regions(r),
      [k:nat] list_vt(roff, k),
      pinch)

#pub fn gestures_new (): gstate

implement gestures_new () = GState(list_vt_nil(), NoRegions(), list_vt_nil(), NoPinch())

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
  | ~GState(ps, rs, os, c) => let
      val () = _held_free(ps)
      val () = _regions_free(rs)
      val () = _roffs_free(os)
    in
      case+ c of
      | ~NoPinch() => ()
      | ~Pinching(_, _, _, _, _) => ()
    end

(* Declares region id, inside region parent (NoRegion for none), owning
   the axes a, pinch or not, long-press or not, for the pointers d. The
   parent must have been declared first (else the region is taken as
   outermost); an id declared already, or below 0, is ignored *)
#pub fn gestures_region (st: !gstate, id: int, parent: region, a: axes, pinch: bool, lp: bool, d: devices): void

fn _add_region {n:nat} (rs: regions(n), id: int, parent: region, a: axes, pinch: bool, lp: bool, d: devices): [m:nat] regions(m) =
  if id < 0 then let val () = region_free(parent) in rs end
  else if _has_region(rs, id) then let val () = region_free(parent) in rs end
  else let
    val inside = (case+ parent of
      | ~InRegion(outer) => if _has_region(rs, outer) then InRegion(outer) else NoRegion()
      | ~NoRegion() => NoRegion()): region
    var r: rdef
    val () = r.id := id
    val () = r.h := (case+ a of AxH() => true | AxBoth() => true | AxNone() => false | AxV() => false)
    val () = r.v := (case+ a of AxV() => true | AxBoth() => true | AxNone() => false | AxH() => false)
    val () = r.pinch := pinch
    val () = r.lp := lp
    val () = r.mouse := (case+ d of DevAll() => true | DevTouch() => false)
  in RegionDef(r, inside, rs) end

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
  | list_vt_cons(p, rest) => if phase_is_drag(pointer_phase(p)) then true else _dragging(rest)

(* A held drag's id in pinch region r, if there is one *)
fun _in_pinch {n:nat} .<n>. (xs: !list_vt(held, n), r: int): Option_vt(int) =
  case+ xs of
  | list_vt_nil() => None_vt()
  | list_vt_cons(p, rest) =>
    if pointer_in_pinch(p, r) then Some_vt(pointer_id(p))
    else _in_pinch(rest, r)

(* Region r's rendered offset, taken out of the list (none for no
   region) *)
fun _take_off {n:nat} .<n>. (xs: list_vt(roff, n), r: !region, x: &int >> int, y: &int >> int): [m:nat | m <= n] list_vt(roff, m) =
  case+ xs of
  | ~list_vt_nil() => list_vt_nil()
  | ~list_vt_cons(o, rest) =>
    if region_is(r, o.0) then let
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
      if pointer_long_due(p, t) then
        (case+ pointer_phase(p) of
         | AtWithinSlop() => let
             val i = pointer_info(p)
             val @(p, owner) = pointer_long_press(p)
             val evs = (case+ owner of
               | ~InRegion(r) => list_vt_cons(GLongPress(r, i.x, i.y), evs)
               | ~NoRegion() => evs): gevents
           in @(list_vt_cons(p, rest), evs) end
         | AtDeadZone() => @(list_vt_cons(p, rest), evs)
         | AtLockedH() => @(list_vt_cons(p, rest), evs)
         | AtLockedV() => @(list_vt_cons(p, rest), evs)
         | AtRejected() => @(list_vt_cons(p, rest), evs)
         | AtLongPressed() => @(list_vt_cons(p, rest), evs)
         | AtInert() => @(list_vt_cons(p, rest), evs))
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

(* The pinch after a move of pointer id: a pinch event when id is one
   of its pointers and it has begun (or begins now) *)
fn _pinch_moved {n:nat} (xs: !list_vt(held, n), c: !pinch, id: int, evs: gevents): gevents =
  case+ c of
  | NoPinch() => evs
  | @Pinching(region, first, second, start_distance, began) =>
    if id <> first && id <> second then let prval () = fold@(c) in evs end
    else let
      var x1: int
      var y1: int
      var x2: int
      var y2: int
      val has_first = _where(xs, first, x1, y1)
      val has_second = _where(xs, second, x2, y2)
    in
      if ~(has_first && has_second) then let prval () = fold@(c) in evs end
      else let
        val d = _dist_px(x1, y1, x2, y2)
        val dd = d - start_distance
        val () = (if began then () else if dd * 16 > slop() then began := true
                  else if ~dd * 16 > slop() then began := true else ())
        val d0 = (if start_distance > 0 then start_distance else 1): int
        val evs = (if began then
            list_vt_cons(GPinch(region, d * scale_one() / d0, x1 + (x2 - x1) / 2, y1 + (y2 - y1) / 2), evs)
          else evs): gevents
        prval () = fold@(c)
      in evs end
    end

(* The pinch ends, when id is one of its pointers *)
fn _pinch_gone (c: &pinch >> pinch, id: int, evs: gevents): gevents =
  case+ c of
  | NoPinch() => evs
  | @Pinching(region, first, second, _, began) =>
    if id <> first && id <> second then let prval () = fold@(c) in evs end
    else let
      val ended = (if began then list_vt_cons(GPinchEnd(region), evs) else evs): gevents
      prval () = fold@(c)
      val () = _pinch_clear(c)
    in ended end

(* ============================================================
   The entry point
   ============================================================ *)

fn _down (st: !gstate, id: int, x: coord, y: coord, t: stamp, kind: pointer_kind, hit: region, vw: int, evs: gevents): gevents =
  case+ st of
  | @GState(ps, rs, os, c) => let
      val @(ps2, old) = _take(ps, id)
    in
      case+ old of
      (* a second down for a pointer held: the host's sequence is wrong;
         the pointer stays as it was *)
      | ~Some_vt(p) => let
          val () = region_free(hit)
          val () = ps := list_vt_cons(p, ps2)
          prval () = fold@(st)
        in evs end
      | ~None_vt() => let
          val n = _count(ps2)
        in
          if n >= 4 then let
              (* beyond the slots: ignored *)
              val () = region_free(hit)
              val () = ps := ps2
              prval () = fold@(st)
            in evs end
          else let
            val mouse = (case+ kind of Mouse() => true | Touch() => false | Pen() => false): bool
            val pinch_region = _owner_of(rs, hit, WantPinch(), mouse)
            (* a drag already in that pinch region, when no pinch is on:
               the region, and the drag's pointer *)
            val partner = (case+ pinch_region of
              | InRegion(region) => (case+ c of
                | Pinching(_, _, _, _, _) => None_vt()
                | NoPinch() => (case+ _in_pinch(ps2, region) of
                  | ~Some_vt(other) => Some_vt(@(region, other))
                  | ~None_vt() => None_vt()))
              | NoRegion() => None_vt()): Option_vt(@(int, int))
          in
            case+ partner of
            | ~Some_vt(found) => let
                (* a pinch takes over: the drag there ends *)
                val () = region_free(hit)
                val other = found.1
                val @(ps3, evs) = _retire(ps2, other, evs)
                var ox: int
                var oy: int
                val ho = _where(ps3, other, ox, oy)
                val d0 = (if ho then _dist_px(ox, oy, x, y) else 0): int
                val () = _pinch_set(c, Pinching(found.0, other, id, d0, false))
                val () = ps := list_vt_cons(pointer_inert(id, x, y, t, pinch_region), ps3)
                prval () = fold@(st)
              in evs end
            | ~None_vt() =>
              if _dragging(ps2) then let
                  (* a second pointer during a drag: ignored by it *)
                  val () = region_free(hit)
                  val () = region_free(pinch_region)
                  val () = ps := list_vt_cons(pointer_inert(id, x, y, t, NoRegion()), ps2)
                  prval () = fold@(st)
                in evs end
              else let
                (* within the edge margin a horizontal drag cannot start *)
                val at_edge = (x < edge() || x > vw - edge()): bool
                val horizontal = (if at_edge then NoRegion() else _owner_of(rs, hit, WantH(), mouse)): region
                val vertical = _owner_of(rs, hit, WantV(), mouse)
                val long_press = _owner_of(rs, hit, WantLongPress(), mouse)
                val () = region_free(hit)
                var ox: int = 0
                var oy: int = 0
                var dummy: int = 0
                (* offsets are only kept for regions, so none takes none *)
                val () = os := _take_off(os, horizontal, ox, dummy)
                val () = os := _take_off(os, vertical, dummy, oy)
                val p = pointer_new(id, x, y, t, horizontal, vertical, long_press, pinch_region, ox, oy)
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
          val evs = _pinch_moved(ps3, c, id, evs)
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
      val evs = (case+ c of
        | NoPinch() => evs
        | Pinching(region, _, _, _, began) => if began then list_vt_cons(GPinchEnd(region), evs) else evs): gevents
      val () = _pinch_clear(c)
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

fn _set_off {r:nat} (st: !gstate, r: int r, x: int, y: int): void =
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
