# gestures

Pointer gesture recognition for Bats: drags with axis lock, long-press
and pinch. It is written for a WASM app in an Android WebView, over
Pointer Events. The library is pure and safe (`unsafe = false`). The
browser side is bridge's `listen_gestures` shim, which forwards pointer
events and a per-frame tick, batched once per animation frame, as
records that `gestures_feed` decodes.

When input is ambiguous, the library does nothing. Scrolling, snap-paging
and taps stay native (`click`); the library never emits a tap.

## Use

```bats
val st = gestures_new ()
(* region 1 owns horizontal drags; its element must declare
   touch_action(AxH(), false), which is "pan-y" *)
val () = gestures_region (st, 1, ~1, AxH (), false, false, DevTouch ())
...
val evs = gestures_feed (st, bytes, n)   (* or gestures_step (st, input) *)
(* GPan(region, delta) while dragging, then exactly one of
   GCommit(region, dir) or GCancel(region, dir) *)
```

A region takes gestures from every pointer (`DevAll`) or from touch and
pen only (`DevTouch`). Use `DevTouch` where a mouse drag must stay the
browser's (text selection).

Every event carries its region's id. The events are:

- `GPan(r, delta)`: displacement along the locked axis, from where it locked.
- `GCommit(r, dir)` or `GCancel(r, dir)`: how a drag ends.
- `GLongPress(r, x, y)`.
- `GPinch(r, scale, cx, cy)` and `GPinchEnd(r)`.
- `GScrollEnd(r, offset)`, `GTransitionEnd(r)` and `GTransitionCancel(r)`: native-owned state, passed through.

All events are linear: free them with `gevents_free`, or consume them in a `case`.

## Proven (compile time)

In `src/classify.bats`, with the integer classifier
(horizontal iff `5·|dy| < 2·|dx|`, vertical iff `5·|dx| < 2·|dy|`):

* `class_unique` and `class_exclusive`: no displacement is both
  horizontal and vertical.
* `class_flip_x` and `class_flip_y`: the class does not change when dx
  or dy changes sign.
* `deadzone_diagonal`: the dead zone is not empty (every `(d, d)` is in it).

In `src/pointer.bats`, a pointer's phase is in its type, and each step's
type says which phase can follow which (`MOVE`, `RELEASE`, `ABORT`):

* `lock_stable`: once an axis is locked it does not change before release.
* `locked_ends`: a locked gesture ends in exactly one of commit or cancel.
* `unlocked_silent`: a gesture that never locked ends in neither.
* `abort_never_commits`: a cancel never commits.
* Only a locked pointer pans (`panned(q)`), and a long-press takes only
  a pointer still within slop (`pointer(PEND)`), so it cannot follow a
  drag.

In `src/tracker.bats`, the entry point `gestures_step` is total. A move,
up or cancel for an unknown pointer is ignored, as is a down past the
4 slots (the slot count is in the pointer list's type). Each pointer is a
linear value, made at its down and consumed at its up or cancel. The
host's bytes are checked once, in `src/decode.bats`: positions are
clamped to ±8192 px, so no product overflows, and a record of an unknown
kind is ignored.

`tests/static` holds programs that must be rejected: a diagonal claimed
horizontal, a locked pointer turning, a locked drag ending silently, a
cancel committing, an unlocked pointer panning, a locked pointer
long-pressing.

## Trace replay

`tests/dynamic/replay` replays pointer sequences through the pure
library and compares the events with `expected`, running under valgrind
(no leaks). It covers:

- a flick, a slow drag, a long drag, and a flick back against the drag;
- a diagonal, a vertical drag in a horizontal region, and a lock that then cannot turn;
- an edge start, a pointercancel, and cancel-all;
- a second finger, and pinch taking over a drag;
- a long-press, and a long-press lost to movement;
- unknown ids, and a fifth finger;
- a rendered offset, and a touch-only region;
- the byte format.

These traces are written by hand from the sequences Android delivers.
Recordings from real devices can be added in the same form.

## Constants

All thresholds are in `src/consts.bats`, in CSS px (never multiplied by
`devicePixelRatio`). Positions are in 1/16 px.

| Constant | Value |
|---|---|
| Slop | 8 px |
| Dead zone | `5·minor < 2·major` |
| Commit distance | 100 px |
| Commit velocity | 300 px/s, over the last 100 ms, samples ≥ 50 ms apart |
| Long-press | 500 ms |
| Edge margin | 24 px |
| Pointer slots | 4 |
