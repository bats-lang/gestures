(* consts -- every threshold of the gesture library, in one place.
   Positions are in 1/16 CSS px (SCALE), times in ms. Thresholds are in
   CSS px, never multiplied by devicePixelRatio. *)

#include "share/atspre_staload.hats"

(* Positions: 1/16 CSS px *)
#pub stadef SCALE = 16
(* A position is within +-8192 CSS px, so every product the recognizers
   form (a squared distance in px, a displacement times 1000) fits in
   32 bits; the input decoder clamps to it *)
#pub stadef COORD_MAX = 131072
#pub typedef coord = [x:int | ~COORD_MAX <= x; x <= COORD_MAX] int x
(* Timestamps: ms, never negative *)
#pub typedef stamp = [t:nat] int t

(* Slop: no classification until the movement exceeds 8 px *)
#pub stadef SLOP = 128
(* Dead zone: an axis when DZ_MINOR * minor < DZ_MAJOR * major (about 22
   degrees each side of the axis); otherwise ambiguous *)
#pub stadef DZ_MINOR = 5
#pub stadef DZ_MAJOR = 2
(* A drag commits at 100 px along its axis *)
#pub stadef COMMIT_DIST = 1600
(* ... or at 300 px/s in the direction of its displacement *)
#pub stadef COMMIT_VEL = 300
(* Velocity is taken over the last 100 ms, from samples at least 50 ms
   apart *)
#pub stadef VEL_WINDOW = 100
#pub stadef VEL_SPAN = 50
(* The samples kept at most (a burst of moves in one window) *)
#pub stadef VEL_SAMPLES = 32
(* A long-press: 500 ms within slop *)
#pub stadef LONG_PRESS = 500
(* A horizontal drag cannot start within 24 px of the left or right
   edge, which the Android back gesture uses *)
#pub stadef EDGE = 384
(* Pointer slots *)
#pub stadef SLOTS = 4
(* A pinch's scale is in 1/1024 *)
#pub stadef SCALE_ONE = 1024

#pub fn slop (): int SLOP
implement slop () = 128
#pub fn dz_minor (): int DZ_MINOR
implement dz_minor () = 5
#pub fn dz_major (): int DZ_MAJOR
implement dz_major () = 2
#pub fn commit_dist (): int COMMIT_DIST
implement commit_dist () = 1600
#pub fn commit_vel (): int COMMIT_VEL
implement commit_vel () = 300
#pub fn vel_window (): int VEL_WINDOW
implement vel_window () = 100
#pub fn vel_span (): int VEL_SPAN
implement vel_span () = 50
#pub fn vel_samples (): int VEL_SAMPLES
implement vel_samples () = 32
#pub fn long_press (): int LONG_PRESS
implement long_press () = 500
#pub fn edge (): int EDGE
implement edge () = 384
#pub fn scale_one (): int SCALE_ONE
implement scale_one () = 1024
#pub fn coord_max (): int COORD_MAX
implement coord_max () = 131072
