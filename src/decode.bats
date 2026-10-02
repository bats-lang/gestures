(* decode -- the inputs the host sends, as bytes: records of RECORD
   bytes, eight int32 little-endian fields each (kind, then the kind's
   fields). The host's words are checked here, once: positions are
   clamped to COORD_MAX, times to 0 and above, and a record of an
   unknown kind is ignored. *)

#include "share/atspre_staload.hats"

#use array as A
#use arith as AR

staload "./consts.sats"
staload "./pointer.sats"
staload "./tracker.sats"

#pub stadef RECORD = 32

(* The record kinds. The host writes each as a number, given here,
   which _record_kind reads, once *)
#pub datatype record_kind =
  | RecordDown               (* 0: id, x, y, t, pointer kind, region, viewport width *)
  | RecordMove               (* 1: id, x, y, t *)
  | RecordUp                 (* 2: id, x, y, t *)
  | RecordCancel             (* 3: id *)
  | RecordTick               (* 4: -, -, -, t *)
  | RecordCancelAll          (* 5 *)
  | RecordRenderedOffset     (* 6: -, x, y, -, -, region *)
  | RecordScrollEnd          (* 7: -, offset, -, -, -, region *)
  | RecordTransitionEnd      (* 8: -, -, -, -, -, region *)
  | RecordTransitionCancel   (* 9: -, -, -, -, -, region *)

fn _record_kind (k: int): Option_vt(record_kind) =
  if k = 0 then Some_vt(RecordDown())
  else if k = 1 then Some_vt(RecordMove())
  else if k = 2 then Some_vt(RecordUp())
  else if k = 3 then Some_vt(RecordCancel())
  else if k = 4 then Some_vt(RecordTick())
  else if k = 5 then Some_vt(RecordCancelAll())
  else if k = 6 then Some_vt(RecordRenderedOffset())
  else if k = 7 then Some_vt(RecordScrollEnd())
  else if k = 8 then Some_vt(RecordTransitionEnd())
  else if k = 9 then Some_vt(RecordTransitionCancel())
  else None_vt()

(* The int32 little-endian at p *)
#pub fn gestures_int32 {l:agz}{o:addr}{n:nat}{p:nat | p + 4 <= n} (b: !$A.arrx(byte, l, n, o), p: int p): [v:int] int v

implement gestures_int32 {l}{o}{n}{p} (b, p) = let
  val b0 = $AR.low_byte(byte2int0($A.get<byte>(b, p)))
  val b1 = $AR.low_byte(byte2int0($A.get<byte>(b, p + 1)))
  val b2 = $AR.low_byte(byte2int0($A.get<byte>(b, p + 2)))
  val b3 = $AR.low_byte(byte2int0($A.get<byte>(b, p + 3)))
  val hi = (if b3 < 128 then b3 else b3 - 256): [h:int | ~128 <= h; h < 128] int h
in b0 + b1 * 256 + b2 * 65536 + hi * 16777216 end

(* A position the host sent, clamped to COORD_MAX *)
#pub fn gestures_coord {v:int} (v: int v): coord

implement gestures_coord {v} (v) =
  if v > 131072 then 131072 else if v < ~131072 then ~131072 else v

(* A time the host sent, 0 and above *)
#pub fn gestures_stamp {v:int} (v: int v): stamp

implement gestures_stamp {v} (v) = if v < 0 then 0 else v

(* A pointer's kind, as the host writes it (0 touch, 1 mouse, 2 pen), or
   none for another number *)
#pub fn gestures_pointer_kind (v: int): Option_vt(pointer_kind)

implement gestures_pointer_kind (v) =
  if v = 0 then Some_vt(Touch())
  else if v = 1 then Some_vt(Mouse())
  else if v = 2 then Some_vt(Pen())
  else None_vt()

(* A region, as the host writes it: its id, or a negative number (-1)
   for none *)
#pub fn gestures_region_of (v: int): region

implement gestures_region_of (v) = if v < 0 then NoRegion() else InRegion(v)

(* The record at p, or none when its kind is unknown (a down's pointer
   kind too, and a rendered offset of no region) *)
#pub fn gestures_decode {l:agz}{o:addr}{n:nat}{p:nat | p + RECORD <= n}
  (b: !$A.arrx(byte, l, n, o), p: int p): Option_vt(input)

implement gestures_decode {l}{o}{n}{p} (b, p) = let
  val k = gestures_int32(b, p)
  val f1 = gestures_int32(b, p + 4)
  val f2 = gestures_int32(b, p + 8)
  val f3 = gestures_int32(b, p + 12)
  val f4 = gestures_int32(b, p + 16)
  val f5 = gestures_int32(b, p + 20)
  val f6 = gestures_int32(b, p + 24)
  val f7 = gestures_int32(b, p + 28)
in
  case+ _record_kind(k) of
  | ~None_vt() => None_vt()
  | ~Some_vt(kind) => (case+ kind of
    | RecordDown() => (case+ gestures_pointer_kind(f5) of
      | ~Some_vt(pointer_kind) =>
        Some_vt(IDown(f1, gestures_coord(f2), gestures_coord(f3), gestures_stamp(f4), pointer_kind, gestures_region_of(f6), f7))
      | ~None_vt() => None_vt())
    | RecordMove() => Some_vt(IMove(f1, gestures_coord(f2), gestures_coord(f3), gestures_stamp(f4)))
    | RecordUp() => Some_vt(IUp(f1, gestures_coord(f2), gestures_coord(f3), gestures_stamp(f4)))
    | RecordCancel() => Some_vt(ICancel(f1))
    | RecordTick() => Some_vt(ITick(gestures_stamp(f4)))
    | RecordCancelAll() => Some_vt(ICancelAll())
    | RecordRenderedOffset() => if f6 >= 0 then Some_vt(IRenderedOffset(f6, f2, f3)) else None_vt()
    | RecordScrollEnd() => Some_vt(IScrollEnd(f6, f2))
    | RecordTransitionEnd() => Some_vt(ITransitionEnd(f6))
    | RecordTransitionCancel() => Some_vt(ITransitionCancel(f6)))
end

fun _revapp {n,m:nat} .<n>. (xs: list_vt(gevent, n), acc: list_vt(gevent, m)): list_vt(gevent, n + m) =
  case+ xs of
  | ~list_vt_nil() => acc
  | ~list_vt_cons(x, rest) => _revapp(rest, list_vt_cons(x, acc))

fun _feed {l:agz}{o:addr}{n:nat}{p:nat | p <= n} .<n - p>.
  (st: !gstate, b: !$A.arrx(byte, l, n, o), n: int n, p: int p, acc: gevents): gevents =
  if p + 32 > n then _revapp(acc, list_vt_nil())
  else let
    val acc = (case+ gestures_decode(b, p) of
      | ~Some_vt(i) => _revapp(gestures_step(st, i), acc)
      | ~None_vt() => acc): gevents
  in _feed(st, b, n, p + 32, acc) end

(* The events a batch of records makes, in order (bytes past the last
   whole record are ignored) *)
#pub fn gestures_feed {l:agz}{o:addr}{n:nat}
  (st: !gstate, b: !$A.arrx(byte, l, n, o), n: int n): gevents

implement gestures_feed {l}{o}{n} (st, b, n) = _feed(st, b, n, 0, list_vt_nil())
