(* classify -- the axis of a displacement, with integer arithmetic only.
   Horizontal iff DZ_MINOR*|dy| < DZ_MAJOR*|dx|, vertical iff
   DZ_MINOR*|dx| < DZ_MAJOR*|dy|, otherwise ambiguous. Proven here, for
   every displacement: no displacement is both; the class does not
   change when dx or dy changes sign; the dead zone is not empty. *)

#include "share/atspre_staload.hats"

staload "./consts.sats"

(* ABS(x, a): a is |x| *)
#pub dataprop ABS(int, int) =
  | {x:nat} ABSpos(x, x)
  | {x:int | x < 0} ABSneg(x, ~x)

(* |x|, with its proof *)
#pub fn absv {x:int} (x: int x): [a:nat] (ABS(x, a) | int a)
implement absv {x} (x) =
  if x >= 0 then (ABSpos() | x) else (ABSneg() | ~x)

(* |x| exists for every x *)
#pub prfn abs_total {x:int} (): [a:nat] ABS(x, a)

primplement abs_total {x} () = sif x >= 0 then ABSpos() else ABSneg()

(* |x| is one number *)
#pub prfn abs_unique {x:int}{a,b:int} (p: ABS(x, a), q: ABS(x, b)): [a == b] void

primplement abs_unique {x}{a,b} (p, q) =
  case+ (p, q) of
  | (ABSpos(), ABSpos()) => ()
  | (ABSneg(), ABSneg()) => ()
  | (ABSpos(), ABSneg()) =/=> ()
  | (ABSneg(), ABSpos()) =/=> ()

(* |-x| = |x| *)
#pub prfn abs_neg {x:int}{a:int} (p: ABS(x, a)): ABS(~x, a)

primplement abs_neg {x}{a} (p) =
  case+ p of
  | ABSpos() => sif x == 0 then ABSpos() else ABSneg()
  | ABSneg() => ABSpos()

(* The classes: a choice of three, a datasort *)
#pub datasort axis_class =
  | Ambiguous
  | Horizontal
  | Vertical

(* CLASS(dx, dy, c): the displacement (dx, dy) is of class c *)
#pub dataprop CLASS(int, int, axis_class) =
  | {dx,dy:int}{ax,ay:nat | DZ_MINOR * ay < DZ_MAJOR * ax}
    CLh(dx, dy, Horizontal) of (ABS(dx, ax), ABS(dy, ay))
  | {dx,dy:int}{ax,ay:nat | DZ_MINOR * ax < DZ_MAJOR * ay}
    CLv(dx, dy, Vertical) of (ABS(dx, ax), ABS(dy, ay))
  | {dx,dy:int}{ax,ay:nat | DZ_MINOR * ay >= DZ_MAJOR * ax; DZ_MINOR * ax >= DZ_MAJOR * ay}
    CLa(dx, dy, Ambiguous) of (ABS(dx, ax), ABS(dy, ay))

(* A class, as a value: what classify answers, matched with case+ *)
#pub datatype class_is(axis_class) =
  | IsAmbiguous(Ambiguous)
  | IsHorizontal(Horizontal)
  | IsVertical(Vertical)

(* SAME_CLASS(c, d): c and d are one class *)
#pub dataprop SAME_CLASS(axis_class, axis_class) =
  | {c:axis_class} SameClass(c, c)

(* The class of (dx, dy) *)
#pub fn classify {dx,dy:int} (dx: int dx, dy: int dy): [c:axis_class] (CLASS(dx, dy, c) | class_is(c))
implement classify {dx,dy} (dx, dy) = let
  val (px | ax) = absv(dx)
  val (py | ay) = absv(dy)
in
  if dz_minor() * ay < dz_major() * ax then (CLh(px, py) | IsHorizontal())
  else if dz_minor() * ax < dz_major() * ay then (CLv(px, py) | IsVertical())
  else (CLa(px, py) | IsAmbiguous())
end

(* A displacement has one class: two classes of it are the same one
   (two different ones would need |dx| and |dy| to be two numbers each) *)
#pub prfn class_unique {dx,dy:int}{c,d:axis_class}
  (p: CLASS(dx, dy, c), q: CLASS(dx, dy, d)): SAME_CLASS(c, d)

primplement class_unique {dx,dy}{c,d} (p, q) =
  case+ (p, q) of
  | (CLh(_, _), CLh(_, _)) => SameClass()
  | (CLv(_, _), CLv(_, _)) => SameClass()
  | (CLa(_, _), CLa(_, _)) => SameClass()
  | (CLh(a, b), CLv(e, f)) =/=> let prval () = abs_unique(a, e) prval () = abs_unique(b, f) in () end
  | (CLh(a, b), CLa(e, f)) =/=> let prval () = abs_unique(a, e) prval () = abs_unique(b, f) in () end
  | (CLv(a, b), CLh(e, f)) =/=> let prval () = abs_unique(a, e) prval () = abs_unique(b, f) in () end
  | (CLv(a, b), CLa(e, f)) =/=> let prval () = abs_unique(a, e) prval () = abs_unique(b, f) in () end
  | (CLa(a, b), CLh(e, f)) =/=> let prval () = abs_unique(a, e) prval () = abs_unique(b, f) in () end
  | (CLa(a, b), CLv(e, f)) =/=> let prval () = abs_unique(a, e) prval () = abs_unique(b, f) in () end

(* No displacement is both horizontal and vertical *)
#pub prfn class_exclusive {dx,dy:int}
  (h: CLASS(dx, dy, Horizontal), v: CLASS(dx, dy, Vertical)): [false] void

primplement class_exclusive {dx,dy} (h, v) = let
  prval same = class_unique(h, v)
in case+ same of SameClass() =/=> () end

(* The class does not change when dx changes sign ... *)
#pub prfn class_flip_x {dx,dy:int}{c:axis_class} (p: CLASS(dx, dy, c)): CLASS(~dx, dy, c)

primplement class_flip_x {dx,dy}{c} (p) =
  case+ p of
  | CLh(a, b) => CLh(abs_neg(a), b)
  | CLv(a, b) => CLv(abs_neg(a), b)
  | CLa(a, b) => CLa(abs_neg(a), b)

(* ... nor when dy does *)
#pub prfn class_flip_y {dx,dy:int}{c:axis_class} (p: CLASS(dx, dy, c)): CLASS(dx, ~dy, c)

primplement class_flip_y {dx,dy}{c} (p) =
  case+ p of
  | CLh(a, b) => CLh(a, abs_neg(b))
  | CLv(a, b) => CLv(a, abs_neg(b))
  | CLa(a, b) => CLa(a, abs_neg(b))

(* The dead zone is not empty: every diagonal displacement (d, d) is in
   it, for the constants chosen *)
#pub prfn deadzone_diagonal {d:int} (): CLASS(d, d, Ambiguous)

primplement deadzone_diagonal {d} () = let
  prval a = abs_total{d}()
in CLa(a, a) end
