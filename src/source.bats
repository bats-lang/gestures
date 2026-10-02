(* source -- the pointer events of a page, as the browser gives them one
   at a time, made into the recognizer's inputs: what was bridge's JS
   (batching once per animation frame, a tick each frame while a pointer
   is down, capturing a mouse once it has moved, cancelling every
   pointer when the page is hidden or loses the focus) is here, in Bats.
   The host forwards each event as a raw record (bridge's listen_pointer)
   and the frame times it is given, and does what the source asks of it:
   capture a pointer, or ask for a frame (bridge's pointer_capture and
   animation_frame). *)

#include "share/atspre_staload.hats"

#use array as A

staload "./consts.sats"
staload "./pointer.sats"
staload "./tracker.sats"
staload "./decode.sats"

(* ============================================================
   Raw records
   ============================================================ *)

(* A raw record: twelve int32 little-endian fields, its kind first *)
#pub stadef RAW_RECORD = 48

(* The raw kinds. The host writes each as a number, given here, which
   _raw_kind reads, once. Positions are in 1/16 CSS px, times in ms; a
   region is the innermost one hit (data-gesture-region), -1 for none *)
#pub datatype raw_kind =
  | RawDown               (* 0: id, x, y, t, pointer kind, button, region, viewport width,
                                transition running (1 or 0), its translate x, y *)
  | RawMove               (* 1: id, x, y, t *)
  | RawUp                 (* 2: id, x, y, t *)
  | RawCancel             (* 3: id *)
  | RawLostCapture        (* 4: id *)
  | RawHidden             (* 5: the page hidden, or the window's focus lost *)
  | RawScrollEnd          (* 6: region, offset *)
  | RawTransitionEnd      (* 7: region (only for the region's own element) *)
  | RawTransitionCancel   (* 8: region (likewise) *)

fn _raw_kind (k: int): Option_vt(raw_kind) =
  if k = 0 then Some_vt(RawDown())
  else if k = 1 then Some_vt(RawMove())
  else if k = 2 then Some_vt(RawUp())
  else if k = 3 then Some_vt(RawCancel())
  else if k = 4 then Some_vt(RawLostCapture())
  else if k = 5 then Some_vt(RawHidden())
  else if k = 6 then Some_vt(RawScrollEnd())
  else if k = 7 then Some_vt(RawTransitionEnd())
  else if k = 8 then Some_vt(RawTransitionCancel())
  else None_vt()

(* ============================================================
   What the source asks of the host
   ============================================================ *)

#pub datavtype action =
  | CapturePointer of (int)  (* capture this pointer to the listened node *)
  | WantFrame                (* call gestures_frame at the next animation frame *)

#pub vtypedef actions = [n:nat] list_vt(action, n)

#pub fn actions_free (xs: actions): void

fun _actions_free {n:nat} .<n>. (xs: list_vt(action, n)): void =
  case+ xs of
  | ~list_vt_nil() => ()
  | ~list_vt_cons(x, rest) => let
      val () = (case+ x of ~CapturePointer(_) => () | ~WantFrame() => ())
    in _actions_free(rest) end

implement actions_free (xs) = _actions_free(xs)

(* ============================================================
   State
   ============================================================ *)

(* A mouse pointer not yet captured, and where it went down *)
#pub typedef mouse_start = @(int, int, int)

(* The pointers down, the mice not yet captured, the inputs waiting for
   the next frame (newest first), and whether a frame was asked for *)
#pub datavtype source =
  | Source of (
      [n:nat] list_vt(int, n),
      [m:nat] list_vt(mouse_start, m),
      [k:nat] list_vt(input, k),
      bool)

#pub fn gestures_source_new (): source

implement gestures_source_new () = Source(list_vt_nil(), list_vt_nil(), list_vt_nil(), false)

fun _ints_free {n:nat} .<n>. (xs: list_vt(int, n)): void =
  case+ xs of
  | ~list_vt_nil() => ()
  | ~list_vt_cons(_, rest) => _ints_free(rest)

fun _starts_free {n:nat} .<n>. (xs: list_vt(mouse_start, n)): void =
  case+ xs of
  | ~list_vt_nil() => ()
  | ~list_vt_cons(_, rest) => _starts_free(rest)

fn _input_free (i: input): void =
  case+ i of
  | ~IDown(_, _, _, _, _, hit, _) => region_free(hit) | ~IMove(_, _, _, _) => () | ~IUp(_, _, _, _) => ()
  | ~ICancel(_) => () | ~ITick(_) => () | ~ICancelAll() => ()
  | ~IRenderedOffset(_, _, _) => () | ~IScrollEnd(_, _) => ()
  | ~ITransitionEnd(_) => () | ~ITransitionCancel(_) => ()

fun _inputs_free {n:nat} .<n>. (xs: list_vt(input, n)): void =
  case+ xs of
  | ~list_vt_nil() => ()
  | ~list_vt_cons(i, rest) => let val () = _input_free(i) in _inputs_free(rest) end

#pub fn gestures_source_free (src: source): void

implement gestures_source_free (src) =
  case+ src of
  | ~Source(down, starts, waiting, _) => let
      val () = _ints_free(down)
      val () = _starts_free(starts)
    in _inputs_free(waiting) end

fun _has {n:nat} .<n>. (xs: !list_vt(int, n), id: int): bool =
  case+ xs of
  | list_vt_nil() => false
  | list_vt_cons(x, rest) => if x = id then true else _has(rest, id)

fun _remove {n:nat} .<n>. (xs: list_vt(int, n), id: int): [m:nat] list_vt(int, m) =
  case+ xs of
  | ~list_vt_nil() => list_vt_nil()
  | ~list_vt_cons(x, rest) =>
    if x = id then _remove(rest, id) else list_vt_cons(x, _remove(rest, id))

fun _remove_start {n:nat} .<n>. (xs: list_vt(mouse_start, n), id: int): [m:nat] list_vt(mouse_start, m) =
  case+ xs of
  | ~list_vt_nil() => list_vt_nil()
  | ~list_vt_cons(x, rest) =>
    if x.0 = id then _remove_start(rest, id) else list_vt_cons(x, _remove_start(rest, id))

(* starts, with a mouse that went down at x, y when is_mouse *)
fn _add_start {n:nat} (starts: list_vt(mouse_start, n), id: int, x: int, y: int, is_mouse: bool): [m:nat] list_vt(mouse_start, m) =
  if is_mouse then list_vt_cons(@(id, x, y), starts) else starts

(* Whether mouse id has moved more than 4 CSS px (|dx| + |dy|, in 1/16
   px) from where it went down *)
fun _moved_far {n:nat} .<n>. (xs: !list_vt(mouse_start, n), id: int, x: int, y: int): bool =
  case+ xs of
  | list_vt_nil() => false
  | list_vt_cons(s, rest) =>
    if s.0 = id then let
      val dx = x - s.1
      val dy = y - s.2
      val dx = (if dx < 0 then ~dx else dx): int
      val dy = (if dy < 0 then ~dy else dy): int
    in dx + dy > 64 (* 4 px in 1/16 px *) end
    else _moved_far(rest, id, x, y)

fn _is_empty {n:nat} (xs: !list_vt(int, n)): bool =
  case+ xs of
  | list_vt_nil() => true
  | list_vt_cons(_, _) => false

(* ============================================================
   Steps
   ============================================================ *)

fun _revapp {n,m:nat} .<n>. (xs: list_vt(gevent, n), acc: list_vt(gevent, m)): list_vt(gevent, n + m) =
  case+ xs of
  | ~list_vt_nil() => acc
  | ~list_vt_cons(x, rest) => _revapp(rest, list_vt_cons(x, acc))

(* The events the waiting inputs make, oldest first; newest-first acc *)
fun _deliver {n:nat} .<n>. (st: !gstate, waiting: list_vt(input, n), acc: gevents): gevents =
  case+ waiting of
  | ~list_vt_nil() => acc
  | ~list_vt_cons(i, rest) => _deliver(st, rest, _revapp(gestures_step(st, i), acc))

fun _reverse_inputs {n,m:nat} .<n>. (xs: list_vt(input, n), acc: list_vt(input, m)): list_vt(input, n + m) =
  case+ xs of
  | ~list_vt_nil() => acc
  | ~list_vt_cons(x, rest) => _reverse_inputs(rest, list_vt_cons(x, acc))

fun _reverse_events {n,m:nat} .<n>. (xs: list_vt(gevent, n), acc: list_vt(gevent, m)): list_vt(gevent, n + m) =
  case+ xs of
  | ~list_vt_nil() => acc
  | ~list_vt_cons(x, rest) => _reverse_events(rest, list_vt_cons(x, acc))

(* Every input waiting, given to the recognizer now: its events *)
fn _flush (src: !source, st: !gstate): gevents =
  case+ src of
  | @Source(_, _, waiting, _) => let
      val ready = _reverse_inputs(waiting, list_vt_nil())
      val () = waiting := list_vt_nil()
      prval () = fold@(src)
    in _reverse_events(_deliver(st, ready, list_vt_nil()), list_vt_nil()) end

(* An input to wait for the next frame; a frame is asked for unless one
   is already *)
fn _wait (src: !source, i: input, asked: actions): actions =
  case+ src of
  | @Source(_, _, waiting, wanted) =>
    if wanted then let
      val () = waiting := list_vt_cons(i, waiting)
      prval () = fold@(src)
    in asked end
    else let
      val () = waiting := list_vt_cons(i, waiting)
      val () = wanted := true
      prval () = fold@(src)
    in list_vt_cons(WantFrame(), asked) end

fn _down_has (src: !source, id: int): bool =
  case+ src of
  | @Source(down, _, _, _) => let val r = _has(down, id) prval () = fold@(src) in r end

fn _any_down (src: !source): bool =
  case+ src of
  | @Source(down, _, _, _) => let val r = ~_is_empty(down) prval () = fold@(src) in r end

fn _forget (src: !source, id: int): void =
  case+ src of
  | @Source(down, starts, _, _) => let
      val () = down := _remove(down, id)
      val () = starts := _remove_start(starts, id)
      prval () = fold@(src)
    in end

(* Every pointer cancelled (the page hidden, the focus or a capture
   lost), when one is down *)
fn _cancel_all (src: !source): actions =
  if ~_any_down(src) then list_vt_nil()
  else let
    val () = (case+ src of
      | @Source(down, starts, _, _) => let
          val () = _ints_free(down)
          val () = down := list_vt_nil()
          val () = _starts_free(starts)
          val () = starts := list_vt_nil()
          prval () = fold@(src)
        in end)
  in _wait(src, ICancelAll(), list_vt_nil()) end

(* What the source does with one raw record: the recognizer's events it
   gives now (an up or a cancel is given at once, so its gesture ends
   before the click that follows it; everything else waits for the next
   frame) and what it asks of the host. A record of an unknown kind is
   ignored *)
#pub fn gestures_raw {l:agz}{o:addr}{n:nat}{p:nat | p + RAW_RECORD <= n}
  (src: !source, st: !gstate, b: !$A.arrx(byte, l, n, o), p: int p): @(gevents, actions)

(* What the source does with a raw record up or a cancel of pointer id:
   given at once, with what waited before it *)
fn _ended_now (src: !source, st: !gstate, id: int, ending: input): @(gevents, actions) =
  if ~_down_has(src, id) then let
    val () = _input_free(ending)
  in @(list_vt_nil(), list_vt_nil()) end
  else let
    val () = _forget(src, id)
    val () = (case+ src of
      | @Source(_, _, waiting, _) => let
          val () = waiting := list_vt_cons(ending, waiting)
          prval () = fold@(src)
        in end)
  in @(_flush(src, st), list_vt_nil()) end

implement gestures_raw {l}{o}{n}{p} (src, st, b, p) = let
  val id = gestures_int32(b, p + 4)
  val x = gestures_coord(gestures_int32(b, p + 8))
  val y = gestures_coord(gestures_int32(b, p + 12))
  val t = gestures_stamp(gestures_int32(b, p + 16))
in
  case+ _raw_kind(gestures_int32(b, p)) of
  | ~None_vt() => @(list_vt_nil(), list_vt_nil())
  | ~Some_vt(kind) => (case+ kind of
    | RawDown() => (case+ gestures_pointer_kind(gestures_int32(b, p + 20)) of
      | ~None_vt() => @(list_vt_nil(), list_vt_nil())
      | ~Some_vt(pointer_kind) => let
          val button = gestures_int32(b, p + 24)
          val region = gestures_int32(b, p + 28)
          val width = gestures_int32(b, p + 32)
          val transitioning = gestures_int32(b, p + 36) = 1
          val is_mouse = (case+ pointer_kind of Mouse() => true | Touch() => false | Pen() => false): bool
        in
          (* a mouse's other buttons are the browser's *)
          if is_mouse && button <> 0 then @(list_vt_nil(), list_vt_nil())
          else let
            (* a region caught in flight: where it is drawn now *)
            val asked = (if ~transitioning then list_vt_nil()
              else if region < 0 then list_vt_nil()
              else _wait(src, IRenderedOffset(region, gestures_int32(b, p + 40), gestures_int32(b, p + 44)), list_vt_nil())): actions
            val () = (case+ src of
              | @Source(down, starts, _, _) => let
                  val () = down := list_vt_cons(id, down)
                  val () = starts := _add_start(starts, id, x, y, is_mouse)
                  prval () = fold@(src)
                in end)
          in @(list_vt_nil(), _wait(src, IDown(id, x, y, t, pointer_kind, gestures_region_of(region), width), asked)) end
        end)
    | RawMove() =>
      if ~_down_has(src, id) then @(list_vt_nil(), list_vt_nil())
      else let
        (* a mouse is captured once it has moved 4 px: a click (no
           drag) keeps its own target *)
        val asked = (case+ src of
          | @Source(_, starts, _, _) =>
            if _moved_far(starts, id, x, y) then let
              val () = starts := _remove_start(starts, id)
              prval () = fold@(src)
            in list_vt_cons(CapturePointer(id), list_vt_nil()) end
            else let prval () = fold@(src) in list_vt_nil() end): actions
      in @(list_vt_nil(), _wait(src, IMove(id, x, y, t), asked)) end
    | RawUp() => _ended_now(src, st, id, IUp(id, x, y, t))
    | RawCancel() => _ended_now(src, st, id, ICancel(id))
    | RawLostCapture() =>
      if _down_has(src, id) then @(list_vt_nil(), _cancel_all(src)) else @(list_vt_nil(), list_vt_nil())
    | RawHidden() => @(list_vt_nil(), _cancel_all(src))
    | RawScrollEnd() =>
      if id >= 0 then @(list_vt_nil(), _wait(src, IScrollEnd(id, x), list_vt_nil()))
      else @(list_vt_nil(), list_vt_nil())
    | RawTransitionEnd() => @(list_vt_nil(), _wait(src, ITransitionEnd(id), list_vt_nil()))
    | RawTransitionCancel() => @(list_vt_nil(), _wait(src, ITransitionCancel(id), list_vt_nil())))
end

(* The animation frame the source asked for, at time t: while a pointer
   is down, a tick (for long-press) after what waited; everything that
   waited is given to the recognizer; another frame is asked for while a
   pointer is down *)
#pub fn gestures_frame (src: !source, st: !gstate, t: stamp): @(gevents, actions)

implement gestures_frame (src, st, t) = let
  val () = (case+ src of
    | @Source(_, _, _, wanted) => let val () = wanted := false prval () = fold@(src) in end)
  val pressing = _any_down(src)
  val () = (if pressing then (case+ src of
    | @Source(_, _, waiting, _) => let
        val () = waiting := list_vt_cons(ITick(t), waiting)
        prval () = fold@(src)
      in end) else ())
  val events = _flush(src, st)
in
  if pressing then
    (case+ src of
     | @Source(_, _, _, wanted) => let
         val () = wanted := true
         prval () = fold@(src)
       in @(events, list_vt_cons(WantFrame(), list_vt_nil())) end)
  else @(events, list_vt_nil())
end
