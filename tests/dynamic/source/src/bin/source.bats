(* The pointer source: raw records as the browser sends them one at a
   time, and animation frames, through gestures_raw and gestures_frame;
   each step's events and the host actions it asks for are printed, and
   the output compared with `expected` *)

#include "share/atspre_staload.hats"
#use gestures as G
#use array as A

staload "gestures/src/pointer.sats"
staload "gestures/src/tracker.sats"
staload "gestures/src/source.sats"

fn show_dir (d: dir): void =
  case+ d of
  | DLeft() => print! "left" | DRight() => print! "right"
  | DUp() => print! "up" | DDown() => print! "down"

fun show_events {n:nat} .<n>. (es: list_vt(gevent, n)): void =
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
    in show_events(rest) end

fun show_actions {n:nat} .<n>. (xs: list_vt(action, n)): void =
  case+ xs of
  | ~list_vt_nil() => ()
  | ~list_vt_cons(x, rest) => let
      val () = (case+ x of
        | ~CapturePointer(id) => println! ("  capture ", id)
        | ~WantFrame() => println! ("  want frame"))
    in show_actions(rest) end

fn show (r: @(gevents, actions)): void = let
  val () = show_events(r.0)
in show_actions(r.1) end

(* A raw record of kind with its fields (positions in px, made 1/16 px
   here) given to the source *)
fn raw (src: !source, st: !gstate, label: string, kind: int, id: int, x: int, y: int, t: int,
        pointer_kind: int, button: int, region: int, width: int, transitioning: int, tx: int, ty: int): void = let
  val b = $A.alloc<byte>(48)
  val () = $A.write_i32(b, 0, kind)
  val () = $A.write_i32(b, 4, id)
  val () = $A.write_i32(b, 8, x * 16)
  val () = $A.write_i32(b, 12, y * 16)
  val () = $A.write_i32(b, 16, t)
  val () = $A.write_i32(b, 20, pointer_kind)
  val () = $A.write_i32(b, 24, button)
  val () = $A.write_i32(b, 28, region)
  val () = $A.write_i32(b, 32, width * 16)
  val () = $A.write_i32(b, 36, transitioning)
  val () = $A.write_i32(b, 40, tx * 16)
  val () = $A.write_i32(b, 44, ty * 16)
  val () = println! (label)
  val () = show(gestures_raw(src, st, b, 0))
in $A.free<byte>(b) end

fn down (src: !source, st: !gstate, label: string, id: int, x: int, y: int, t: int, pointer_kind: int, button: int): void =
  raw(src, st, label, 0, id, x, y, t, pointer_kind, button, 1, 400, 0, 0, 0)

fn event (src: !source, st: !gstate, label: string, kind: int, id: int, x: int, y: int, t: int): void =
  raw(src, st, label, kind, id, x, y, t, 0, 0, 0, 0, 0, 0, 0)

fn frame {t:nat} (src: !source, st: !gstate, t: int t): void = let
  val () = println! ("frame ", t)
in show(gestures_frame(src, st, t)) end

implement main0 () = let
  val st = gestures_new()
  val () = gestures_region(st, 1, NoRegion(), AxH(), false, true, DevAll())
  val src = gestures_source_new()
  (* a touch drag to the left: one frame asked for while it moves, the
     events at the frame, the commit at the up, at once *)
  val () = down(src, st, "touch down", 7, 300, 100, 1000, 0, 0)
  val () = event(src, st, "move", 1, 7, 280, 101, 1016)
  val () = event(src, st, "move", 1, 7, 250, 102, 1032)
  val () = frame(src, st, 1033)
  val () = event(src, st, "move", 1, 7, 150, 103, 1048)
  val () = event(src, st, "up", 2, 7, 140, 103, 1060)
  val () = frame(src, st, 1066)
  (* a mouse's right button is the browser's; its left one is captured
     once it has moved past 4 px, not before *)
  val () = down(src, st, "mouse down, right button", 8, 200, 100, 2000, 1, 2)
  val () = down(src, st, "mouse down", 9, 200, 100, 2010, 1, 0)
  val () = event(src, st, "move 3 px", 1, 9, 203, 100, 2020)
  val () = event(src, st, "move 6 px", 1, 9, 206, 100, 2030)
  val () = event(src, st, "move again", 1, 9, 210, 100, 2040)
  val () = frame(src, st, 2041)
  (* the page hidden: every pointer cancelled at the next frame *)
  val () = event(src, st, "hidden", 5, 0, 0, 0, 0)
  val () = frame(src, st, 2050)
  val () = event(src, st, "up of a cancelled pointer", 2, 9, 210, 100, 2060)
  (* a long press: ticks while it is held *)
  val () = down(src, st, "touch down", 10, 100, 300, 3000, 0, 0)
  val () = frame(src, st, 3100)
  val () = frame(src, st, 3600)
  val () = event(src, st, "lost capture of another pointer", 4, 99, 0, 0, 0)
  val () = event(src, st, "lost capture", 4, 10, 0, 0, 0)
  val () = frame(src, st, 3700)
  (* native ends, passed through at the next frame *)
  val () = event(src, st, "scrollend outside every region", 6, ~1, 0, 0, 0)
  val () = event(src, st, "scrollend", 6, 1, 64, 0, 0)
  val () = event(src, st, "transitionend", 7, 1, 0, 0, 0)
  val () = frame(src, st, 4000)
  val () = gestures_source_free(src)
in gestures_free(st) end
