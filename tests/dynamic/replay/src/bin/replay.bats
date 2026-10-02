(* Trace replay: pointer sequences as Android delivers them (a flick,
   a slow drag, a diagonal, an edge start, a cancel, two fingers, a
   long press, ...) through the pure library; each event is printed and
   the output compared with `expected` *)

#include "share/atspre_staload.hats"
#use gestures as G
#use array as A

staload "gestures/src/pointer.sats"
staload "gestures/src/tracker.sats"
staload "gestures/src/decode.sats"

fn show_dir (d: dir): void =
  case+ d of
  | DLeft() => print! "left" | DRight() => print! "right"
  | DUp() => print! "up" | DDown() => print! "down"

fun show {n:nat} .<n>. (es: list_vt(gevent, n)): void =
  case+ es of
  | ~list_vt_nil() => ()
  | ~list_vt_cons(e, rest) => let
      val () = (case+ e of
        | ~GPan(r, d) => println! ("  pan ", r, " ", d)
        | ~GCommit(r, d) => let val () = print! ("  commit ", r, " ") val () = show_dir(d) in print_newline() end
        | ~GCancel(r, d) => let val () = print! ("  cancel ", r, " ") val () = show_dir(d) in print_newline() end
        | ~GLongPress(r, x, y) => println! ("  long-press ", r, " ", x, " ", y)
        | ~GPinch(r, s, x, y) => println! ("  pinch ", r, " ", s, " ", x, " ", y)
        | ~GPinchEnd(r) => println! ("  pinch-end ", r)
        | ~GScrollEnd(r, o) => println! ("  scrollend ", r, " ", o)
        | ~GTransitionEnd(r) => println! ("  transitionend ", r)
        | ~GTransitionCancel(r) => println! ("  transitioncancel ", r))
    in show(rest) end

fn step (st: !gstate, i: input): void = show(gestures_step(st, i))

(* Region 1: the page, owning horizontal drags, long-press; region 2: a
   map inside it, owning both axes and pinch; region 3: a list owning
   vertical drags. The viewport is 400 px wide *)
fn fresh (): gstate = let
  val st = gestures_new()
  val () = gestures_region(st, 1, NoRegion(), AxH(), false, true, DevAll())
  val () = gestures_region(st, 2, InRegion(1), AxBoth(), true, false, DevAll())
  val () = gestures_region(st, 3, NoRegion(), AxV(), false, false, DevAll())
  val () = gestures_region(st, 4, NoRegion(), AxH(), false, false, DevTouch())
in st end

#define W 6400

fn title (s: string): void = println! (s)

implement main0 () = let
  val () = title("flick left: 40 px in 60 ms")
  val st = fresh()
  val () = step(st, IDown(7, 3200, 3200, 1000, Touch(), InRegion(1), W))
  val () = step(st, IMove(7, 3040, 3210, 1016))
  val () = step(st, IMove(7, 2880, 3220, 1033))
  val () = step(st, IMove(7, 2720, 3220, 1050))
  val () = step(st, IUp(7, 2560, 3220, 1060))
  val () = gestures_free(st)

  val () = title("slow drag right: 60 px in 1 s")
  val st = fresh()
  val () = step(st, IDown(1, 3200, 3200, 0, Touch(), InRegion(1), W))
  val () = step(st, IMove(1, 3400, 3200, 300))
  val () = step(st, IMove(1, 3700, 3200, 700))
  val () = step(st, IMove(1, 4160, 3200, 1000))
  val () = step(st, IUp(1, 4160, 3200, 1100))
  val () = gestures_free(st)

  val () = title("long drag left: 120 px, slowly")
  val st = fresh()
  val () = step(st, IDown(1, 4800, 3200, 0, Touch(), InRegion(1), W))
  val () = step(st, IMove(1, 4000, 3200, 400))
  val () = step(st, IMove(1, 2880, 3200, 900))
  val () = step(st, IUp(1, 2880, 3200, 1500))
  val () = gestures_free(st)

  val () = title("flick back: 60 px right, then moving left fast")
  val st = fresh()
  val () = step(st, IDown(1, 3200, 3200, 0, Touch(), InRegion(1), W))
  val () = step(st, IMove(1, 4800, 3200, 400))
  val () = step(st, IMove(1, 4640, 3200, 430))
  val () = step(st, IMove(1, 4320, 3200, 460))
  val () = step(st, IUp(1, 4160, 3200, 480))
  val () = gestures_free(st)

  val () = title("diagonal: never leaves the dead zone")
  val st = fresh()
  val () = step(st, IDown(1, 3200, 3200, 0, Touch(), InRegion(1), W))
  val () = step(st, IMove(1, 3520, 3520, 50))
  val () = step(st, IMove(1, 4800, 4800, 100))
  val () = step(st, IUp(1, 6400, 6400, 150))
  val () = gestures_free(st)

  val () = title("vertical in a horizontal region: ignored, even when it turns")
  val st = fresh()
  val () = step(st, IDown(1, 3200, 3200, 0, Touch(), InRegion(1), W))
  val () = step(st, IMove(1, 3200, 4800, 50))
  val () = step(st, IMove(1, 1600, 4800, 100))
  val () = step(st, IUp(1, 0, 4800, 150))
  val () = gestures_free(st)

  val () = title("vertical in the list: locks vertically, then cannot turn")
  val st = fresh()
  val () = step(st, IDown(1, 3200, 3200, 0, Touch(), InRegion(3), W))
  val () = step(st, IMove(1, 3200, 3000, 16))
  val () = step(st, IMove(1, 3300, 2400, 33))
  val () = step(st, IMove(1, 6000, 2400, 50))
  val () = step(st, IUp(1, 6000, 1000, 66))
  val () = gestures_free(st)

  val () = title("edge start: 10 px from the left edge, no horizontal drag")
  val st = fresh()
  val () = step(st, IDown(1, 160, 3200, 0, Touch(), InRegion(1), W))
  val () = step(st, IMove(1, 1600, 3200, 50))
  val () = step(st, IUp(1, 3200, 3200, 100))
  val () = gestures_free(st)

  val () = title("pointercancel mid-drag: cancels, never commits")
  val st = fresh()
  val () = step(st, IDown(1, 3200, 3200, 0, Touch(), InRegion(1), W))
  val () = step(st, IMove(1, 1600, 3200, 30))
  val () = step(st, ICancel(1))
  val () = step(st, IUp(1, 0, 3200, 60))
  val () = gestures_free(st)

  val () = title("cancel-all (the app hidden) mid-drag")
  val st = fresh()
  val () = step(st, IDown(1, 3200, 3200, 0, Touch(), InRegion(1), W))
  val () = step(st, IMove(1, 4800, 3200, 30))
  val () = step(st, ICancelAll())
  val () = step(st, IMove(1, 6000, 3200, 60))
  val () = gestures_free(st)

  val () = title("second finger during a drag, no pinch: ignored")
  val st = fresh()
  val () = step(st, IDown(1, 3200, 3200, 0, Touch(), InRegion(1), W))
  val () = step(st, IMove(1, 2400, 3200, 30))
  val () = step(st, IDown(2, 4000, 4000, 40, Touch(), InRegion(1), W))
  val () = step(st, IMove(2, 1000, 4000, 50))
  val () = step(st, IUp(2, 1000, 4000, 60))
  val () = step(st, IMove(1, 1200, 3200, 70))
  val () = step(st, IUp(1, 1200, 3200, 80))
  val () = gestures_free(st)

  val () = title("two fingers in the map: the drag cancels, a pinch takes over")
  val st = fresh()
  val () = step(st, IDown(1, 3200, 3200, 0, Touch(), InRegion(2), W))
  val () = step(st, IMove(1, 2880, 3200, 20))
  val () = step(st, IDown(2, 4800, 3200, 30, Touch(), InRegion(2), W))
  val () = step(st, IMove(2, 4880, 3200, 40))
  val () = step(st, IMove(2, 6400, 3200, 60))
  val () = step(st, IMove(1, 1600, 3200, 70))
  val () = step(st, IUp(2, 6400, 3200, 80))
  val () = step(st, IMove(1, 800, 3200, 90))
  val () = step(st, IUp(1, 800, 3200, 100))
  val () = gestures_free(st)

  val () = title("long press: 500 ms within slop, then no drag")
  val st = fresh()
  val () = step(st, IDown(1, 3200, 3200, 1000, Touch(), InRegion(1), W))
  val () = step(st, ITick(1200))
  val () = step(st, IMove(1, 3250, 3200, 1300))
  val () = step(st, ITick(1499))
  val () = step(st, ITick(1500))
  val () = step(st, ITick(1600))
  val () = step(st, IMove(1, 800, 3200, 1700))
  val () = step(st, IUp(1, 800, 3200, 1800))
  val () = gestures_free(st)

  val () = title("long press cancelled by moving past slop")
  val st = fresh()
  val () = step(st, IDown(1, 3200, 3200, 0, Touch(), InRegion(1), W))
  val () = step(st, IMove(1, 3200, 3400, 100))
  val () = step(st, ITick(600))
  val () = step(st, IUp(1, 3200, 3400, 700))
  val () = gestures_free(st)

  val () = title("unknown pointers and a fifth finger: ignored")
  val st = fresh()
  val () = step(st, IMove(9, 0, 0, 0))
  val () = step(st, IUp(9, 0, 0, 0))
  val () = step(st, ICancel(9))
  val () = step(st, IDown(1, 3200, 3200, 0, Touch(), InRegion(3), W))
  val () = step(st, IDown(2, 3200, 3200, 0, Touch(), InRegion(3), W))
  val () = step(st, IDown(3, 3200, 3200, 0, Touch(), InRegion(3), W))
  val () = step(st, IDown(4, 3200, 3200, 0, Touch(), InRegion(3), W))
  val () = step(st, IDown(5, 3200, 3200, 0, Touch(), InRegion(3), W))
  val () = step(st, IMove(5, 3200, 0, 10))
  val () = step(st, IUp(5, 3200, 0, 20))
  val () = step(st, IMove(1, 3200, 0, 30))
  val () = step(st, IUp(1, 3200, 0, 40))
  val () = gestures_free(st)

  val () = title("rendered offset: a drag during a transition starts where it is")
  val st = fresh()
  val () = step(st, IRenderedOffset(1, ~800, 0))
  val () = step(st, IDown(1, 3200, 3200, 0, Touch(), InRegion(1), W))
  val () = step(st, IMove(1, 3520, 3200, 20))
  val () = step(st, IMove(1, 3680, 3200, 40))
  val () = step(st, IUp(1, 3680, 3200, 400))
  val () = step(st, IRenderedOffset(1, ~800, 0))
  val () = step(st, ITransitionEnd(1))
  val () = step(st, IDown(2, 3200, 3200, 500, Touch(), InRegion(1), W))
  val () = step(st, IMove(2, 3520, 3200, 520))
  val () = step(st, ICancel(2))
  val () = step(st, IScrollEnd(1, 1200))
  val () = gestures_free(st)

  val () = title("a touch-only region: a mouse drag is left alone, a pen drag is not")
  val st = fresh()
  val () = step(st, IDown(1, 3200, 3200, 0, Mouse(), InRegion(4), W))
  val () = step(st, IMove(1, 1600, 3200, 30))
  val () = step(st, IUp(1, 1600, 3200, 60))
  val () = step(st, IDown(2, 3200, 3200, 100, Pen(), InRegion(4), W))
  val () = step(st, IMove(2, 1600, 3200, 130))
  val () = step(st, IUp(2, 1600, 3200, 160))
  val () = gestures_free(st)

  val () = title("bytes: a flick right as the shim sends it")
  val st = fresh()
  val b = $A.alloc<byte>(4 * 32 + 5)
  fn put {l:agz}{n:nat}{p:nat | p + 32 <= n} (b: !$A.arr(byte, l, n), p: int p,
      k: int, f1: int, f2: int, f3: int, f4: int, f5: int, f6: int, f7: int): void = let
    val () = $A.write_i32(b, p, k)
    val () = $A.write_i32(b, p + 4, f1)
    val () = $A.write_i32(b, p + 8, f2)
    val () = $A.write_i32(b, p + 12, f3)
    val () = $A.write_i32(b, p + 16, f4)
    val () = $A.write_i32(b, p + 20, f5)
    val () = $A.write_i32(b, p + 24, f6)
  in $A.write_i32(b, p + 28, f7) end
  val () = put(b, 0, 0, 3, 3200, 3200, 0, 0, 1, W)
  val () = put(b, 32, 1, 3, 3500, 3200, 16, 0, 0, 0)
  val () = put(b, 64, 1, 3, 3900, 3210, 33, 0, 0, 0)
  val () = put(b, 96, 2, 3, 4300, 3210, 60, 0, 0, 0)
  val () = show(gestures_feed(st, b, 4 * 32 + 5))
  val () = $A.free<byte>(b)
  val () = gestures_free(st)
in end
