#;#;
#<<END
TR opt: float-complex-float-mul.rkt 32:0 (* 1.0 2.0+4.0i) -- unboxed float complex multiplication
TR opt: float-complex-float-mul.rkt 32:7 2.0+4.0i -- unboxed literal
TR opt: float-complex-float-mul.rkt 33:0 (* 1.0+2.0i 2.0) -- unboxed float complex multiplication
TR opt: float-complex-float-mul.rkt 33:3 1.0+2.0i -- unboxed literal
TR opt: float-complex-float-mul.rkt 34:0 (* 1.0 2.0+4.0i 3.0+6.0i) -- unboxed float complex multiplication
TR opt: float-complex-float-mul.rkt 34:16 3.0+6.0i -- unboxed literal
TR opt: float-complex-float-mul.rkt 34:7 2.0+4.0i -- unboxed literal
TR opt: float-complex-float-mul.rkt 35:0 (* 1.0+2.0i 2.0 3.0+6.0i) -- unboxed float complex multiplication
TR opt: float-complex-float-mul.rkt 35:16 3.0+6.0i -- unboxed literal
TR opt: float-complex-float-mul.rkt 35:3 1.0+2.0i -- unboxed literal
TR opt: float-complex-float-mul.rkt 36:0 (* 1.0+2.0i 2.0+4.0i 3.0) -- unboxed float complex multiplication
TR opt: float-complex-float-mul.rkt 36:12 2.0+4.0i -- unboxed literal
TR opt: float-complex-float-mul.rkt 36:3 1.0+2.0i -- unboxed literal
TR opt: float-complex-float-mul.rkt 37:0 (* 1.0+2.0i 2.0 3.0) -- unboxed float complex multiplication
TR opt: float-complex-float-mul.rkt 37:3 1.0+2.0i -- unboxed literal
END
#<<END
2.0+4.0i
2.0+4.0i
-18.0+24.0i
-18.0+24.0i
-18.0+24.0i
6.0+12.0i

END

#lang typed/scheme
#:optimize

(* 1.0 2.0+4.0i)
(* 1.0+2.0i 2.0)
(* 1.0 2.0+4.0i 3.0+6.0i)
(* 1.0+2.0i 2.0 3.0+6.0i)
(* 1.0+2.0i 2.0+4.0i 3.0)
(* 1.0+2.0i 2.0 3.0)
