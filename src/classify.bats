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

(* The classes *)
#pub stadef AMBIG = 0
#pub stadef HORIZ = 1
#pub stadef VERT = 2

(* CLASS(dx, dy, c): the displacement (dx, dy) is of class c *)
#pub dataprop CLASS(int, int, int) =
  | {dx,dy:int}{ax,ay:nat | DZ_MINOR * ay < DZ_MAJOR * ax}
    CLh(dx, dy, HORIZ) of (ABS(dx, ax), ABS(dy, ay))
  | {dx,dy:int}{ax,ay:nat | DZ_MINOR * ax < DZ_MAJOR * ay}
    CLv(dx, dy, VERT) of (ABS(dx, ax), ABS(dy, ay))
  | {dx,dy:int}{ax,ay:nat | DZ_MINOR * ay >= DZ_MAJOR * ax; DZ_MINOR * ax >= DZ_MAJOR * ay}
    CLa(dx, dy, AMBIG) of (ABS(dx, ax), ABS(dy, ay))

(* The class of (dx, dy) *)
#pub fn classify {dx,dy:int} (dx: int dx, dy: int dy): [c:int | c >= AMBIG; c <= VERT] (CLASS(dx, dy, c) | int c)
implement classify {dx,dy} (dx, dy) = let
  val (px | ax) = absv(dx)
  val (py | ay) = absv(dy)
in
  if dz_minor() * ay < dz_major() * ax then (CLh(px, py) | 1)
  else if dz_minor() * ax < dz_major() * ay then (CLv(px, py) | 2)
  else (CLa(px, py) | 0)
end

(* A displacement has one class *)
#pub prfn class_unique {dx,dy:int}{c,d:int}
  (p: CLASS(dx, dy, c), q: CLASS(dx, dy, d)): [c == d] void

primplement class_unique {dx,dy}{c,d} (p, q) = let
  prfn parts {c:int} (p: CLASS(dx, dy, c)):
    [ax,ay:nat | (c == HORIZ && DZ_MINOR * ay < DZ_MAJOR * ax) ||
                 (c == VERT && DZ_MINOR * ax < DZ_MAJOR * ay) ||
                 (c == AMBIG && DZ_MINOR * ay >= DZ_MAJOR * ax && DZ_MINOR * ax >= DZ_MAJOR * ay)]
    (ABS(dx, ax), ABS(dy, ay)) =
    case+ p of
    | CLh(a, b) => (a, b)
    | CLv(a, b) => (a, b)
    | CLa(a, b) => (a, b)
  prval (pa, pb) = parts(p)
  prval (qa, qb) = parts(q)
  prval () = abs_unique(pa, qa)
  prval () = abs_unique(pb, qb)
in end

(* No displacement is both horizontal and vertical *)
#pub prfn class_exclusive {dx,dy:int}
  (h: CLASS(dx, dy, HORIZ), v: CLASS(dx, dy, VERT)): [false] void

primplement class_exclusive {dx,dy} (h, v) = class_unique(h, v)

(* The class does not change when dx changes sign ... *)
#pub prfn class_flip_x {dx,dy:int}{c:int} (p: CLASS(dx, dy, c)): CLASS(~dx, dy, c)

primplement class_flip_x {dx,dy}{c} (p) =
  case+ p of
  | CLh(a, b) => CLh(abs_neg(a), b)
  | CLv(a, b) => CLv(abs_neg(a), b)
  | CLa(a, b) => CLa(abs_neg(a), b)

(* ... nor when dy does *)
#pub prfn class_flip_y {dx,dy:int}{c:int} (p: CLASS(dx, dy, c)): CLASS(dx, ~dy, c)

primplement class_flip_y {dx,dy}{c} (p) =
  case+ p of
  | CLh(a, b) => CLh(a, abs_neg(b))
  | CLv(a, b) => CLv(a, abs_neg(b))
  | CLa(a, b) => CLa(a, abs_neg(b))

(* The dead zone is not empty: every diagonal displacement (d, d) is in
   it, for the constants chosen *)
#pub prfn deadzone_diagonal {d:int} (): CLASS(d, d, AMBIG)

primplement deadzone_diagonal {d} () = let
  prval a = abs_total{d}()
in CLa(a, a) end
