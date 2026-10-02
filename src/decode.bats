(* decode -- the inputs the host sends, as bytes: records of RECORD
   bytes, eight int32 little-endian fields each (kind, then the kind's
   fields). The host's words are checked here, once: positions are
   clamped to COORD_MAX, times to 0 and above, and a record of an
   unknown kind is ignored. *)

#include "share/atspre_staload.hats"

#use array as A
#use arith as AR

staload "./consts.sats"
staload "./tracker.sats"

#pub stadef RECORD = 32

(* The record kinds, as the host writes them *)
#pub stadef REC_DOWN = 0              (* id, x, y, t, kind, region, viewport width *)
#pub stadef REC_MOVE = 1              (* id, x, y, t *)
#pub stadef REC_UP = 2                (* id, x, y, t *)
#pub stadef REC_CANCEL = 3            (* id *)
#pub stadef REC_TICK = 4              (* -, -, -, t *)
#pub stadef REC_CANCEL_ALL = 5
#pub stadef REC_RENDERED_OFFSET = 6   (* -, x, y, -, -, region *)
#pub stadef REC_SCROLLEND = 7         (* -, offset, -, -, -, region *)
#pub stadef REC_TRANSITIONEND = 8     (* -, -, -, -, -, region *)
#pub stadef REC_TRANSITIONCANCEL = 9  (* -, -, -, -, -, region *)

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

(* The record at p, or none when its kind is unknown *)
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
  if k = 0 then Some_vt(IDown(f1, gestures_coord(f2), gestures_coord(f3), gestures_stamp(f4), f5, f6, f7))
  else if k = 1 then Some_vt(IMove(f1, gestures_coord(f2), gestures_coord(f3), gestures_stamp(f4)))
  else if k = 2 then Some_vt(IUp(f1, gestures_coord(f2), gestures_coord(f3), gestures_stamp(f4)))
  else if k = 3 then Some_vt(ICancel(f1))
  else if k = 4 then Some_vt(ITick(gestures_stamp(f4)))
  else if k = 5 then Some_vt(ICancelAll())
  else if k = 6 then Some_vt(IRenderedOffset(f6, f2, f3))
  else if k = 7 then Some_vt(IScrollEnd(f6, f2))
  else if k = 8 then Some_vt(ITransitionEnd(f6))
  else if k = 9 then Some_vt(ITransitionCancel(f6))
  else None_vt()
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
