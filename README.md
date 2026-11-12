# gestures

Pointer gesture recognition for Bats: drags with axis lock, long-press
and pinch. It is written for a WASM app in an Android WebView, over
Pointer Events. The library is pure and safe (`unsafe = false`).

When input is ambiguous, the library does nothing. Scrolling, snap-paging
and taps stay native (`click`); the library never emits a tap.

## Proven (compile time)

In `src/classify.bats`, with the integer classifier
(horizontal iff `5·|dy| < 2·|dx|`, vertical iff `5·|dx| < 2·|dy|`):

* `class_unique` and `class_exclusive`: no displacement is both
  horizontal and vertical.
* `class_flip_x` and `class_flip_y`: the class does not change when dx
  or dy changes sign.
* `deadzone_diagonal`: the dead zone is not empty (every `(d, d)` is in it).

In `src/pointer.bats`, a pointer's phase is in its type, and each step's
type says which phase can follow which (`MOVE`, `RELEASE`, `ABORT`;
`LOCKED`, `UNLOCKED`, `DECIDING` and `STAYING` say which phases are
which):

* `lock_stable`: once an axis is locked it does not change before release
  (`SAME_PHASE`).
* `locked_ends`: a locked gesture ends in exactly one of commit or cancel.
* `unlocked_silent`: a gesture that never locked ends in neither.
* `abort_never_commits`: a cancel never commits.
* Only a locked pointer pans (`panned(q)`), and a long-press takes only
  a pointer still within slop (`pointer(WithinSlop)`), so it cannot follow a
  drag.

In `src/tracker.bats`, the entry point `gestures_step` is total. A move,
up or cancel for an unknown pointer is ignored, as is a down past the
4 slots (the slot count is in the pointer list's type). Each pointer is a
linear value, made at its down and consumed at its up or cancel. The
host's bytes are checked once, in `src/decode.bats`: positions are
clamped to ±8192 px, so no product overflows, and a record of an unknown
kind (or a down of an unknown pointer kind) is ignored.

`tests/static` holds programs that must be rejected: a diagonal claimed
horizontal, a locked pointer turning, a locked drag ending silently, a
cancel committing, an unlocked pointer panning, a locked pointer
long-pressing, a phase or a region given as a number, and a match that
leaves out a phase, a pointer kind or a class. Its runner also fails on
any `case` without `+`.
