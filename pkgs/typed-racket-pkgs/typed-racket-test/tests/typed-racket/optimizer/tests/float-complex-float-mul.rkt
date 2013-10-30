#;#;
#<<END
TR missed opt: float-complex-float-mul.rkt 46:3 1.0 -- Non complex value in complex arithmetic
TR missed opt: float-complex-float-mul.rkt 47:12 2.0 -- Non complex value in complex arithmetic
TR missed opt: float-complex-float-mul.rkt 48:3 1.0 -- Non complex value in complex arithmetic
TR missed opt: float-complex-float-mul.rkt 49:12 2.0 -- Non complex value in complex arithmetic
TR missed opt: float-complex-float-mul.rkt 50:21 3.0 -- Non complex value in complex arithmetic
TR missed opt: float-complex-float-mul.rkt 51:12 2.0 -- Non complex value in complex arithmetic
TR missed opt: float-complex-float-mul.rkt 51:16 3.0 -- Non complex value in complex arithmetic
TR opt: float-complex-float-mul.rkt 46:0 (* 1.0 2.0+4.0i) -- unboxed binary float complex
TR opt: float-complex-float-mul.rkt 46:3 1.0 -- float in complex ops
TR opt: float-complex-float-mul.rkt 46:7 2.0+4.0i -- unboxed literal
TR opt: float-complex-float-mul.rkt 47:0 (* 1.0+2.0i 2.0) -- unboxed binary float complex
TR opt: float-complex-float-mul.rkt 47:12 2.0 -- float in complex ops
TR opt: float-complex-float-mul.rkt 47:3 1.0+2.0i -- unboxed literal
TR opt: float-complex-float-mul.rkt 48:0 (* 1.0 2.0+4.0i 3.0+6.0i) -- unboxed binary float complex
TR opt: float-complex-float-mul.rkt 48:16 3.0+6.0i -- unboxed literal
TR opt: float-complex-float-mul.rkt 48:3 1.0 -- float in complex ops
TR opt: float-complex-float-mul.rkt 48:7 2.0+4.0i -- unboxed literal
TR opt: float-complex-float-mul.rkt 49:0 (* 1.0+2.0i 2.0 3.0+6.0i) -- unboxed binary float complex
TR opt: float-complex-float-mul.rkt 49:12 2.0 -- float in complex ops
TR opt: float-complex-float-mul.rkt 49:16 3.0+6.0i -- unboxed literal
TR opt: float-complex-float-mul.rkt 49:3 1.0+2.0i -- unboxed literal
TR opt: float-complex-float-mul.rkt 50:0 (* 1.0+2.0i 2.0+4.0i 3.0) -- unboxed binary float complex
TR opt: float-complex-float-mul.rkt 50:12 2.0+4.0i -- unboxed literal
TR opt: float-complex-float-mul.rkt 50:21 3.0 -- float in complex ops
TR opt: float-complex-float-mul.rkt 50:3 1.0+2.0i -- unboxed literal
TR opt: float-complex-float-mul.rkt 51:0 (* 1.0+2.0i 2.0 3.0) -- unboxed binary float complex
TR opt: float-complex-float-mul.rkt 51:12 2.0 -- float in complex ops
TR opt: float-complex-float-mul.rkt 51:16 3.0 -- float in complex ops
TR opt: float-complex-float-mul.rkt 51:3 1.0+2.0i -- unboxed literal
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
