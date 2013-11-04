#;#;
#<<END
TR missed opt: float-complex-float.rkt 34:3 1.0 -- non-complex value in complex arithmetic
TR missed opt: float-complex-float.rkt 35:12 2.0 -- non-complex value in complex arithmetic
TR missed opt: float-complex-float.rkt 36:21 3.0 -- non-complex value in complex arithmetic
TR opt: float-complex-float.rkt 33:0 (+ 1.0+2.0i 2.0 3.0+6.0i) -- unboxed float complex addition
TR opt: float-complex-float.rkt 33:16 3.0+6.0i -- unboxed literal
TR opt: float-complex-float.rkt 33:3 1.0+2.0i -- unboxed literal
TR opt: float-complex-float.rkt 34:0 (- 1.0 2.0+4.0i 3.0+6.0i) -- unboxed binary float complex
TR opt: float-complex-float.rkt 34:16 3.0+6.0i -- unboxed literal
TR opt: float-complex-float.rkt 34:3 1.0 -- float in complex ops
TR opt: float-complex-float.rkt 34:7 2.0+4.0i -- unboxed literal
TR opt: float-complex-float.rkt 35:0 (- 1.0+2.0i 2.0 3.0+6.0i) -- unboxed binary float complex
TR opt: float-complex-float.rkt 35:12 2.0 -- float in complex ops
TR opt: float-complex-float.rkt 35:16 3.0+6.0i -- unboxed literal
TR opt: float-complex-float.rkt 35:3 1.0+2.0i -- unboxed literal
TR opt: float-complex-float.rkt 36:0 (- 1.0+2.0i 2.0+4.0i 3.0) -- unboxed binary float complex
TR opt: float-complex-float.rkt 36:12 2.0+4.0i -- unboxed literal
TR opt: float-complex-float.rkt 36:21 3.0 -- float in complex ops
TR opt: float-complex-float.rkt 36:3 1.0+2.0i -- unboxed literal
END
#<<END
6.0+8.0i
-4.0-10.0i
-4.0-4.0i
-4.0-2.0i

END

#lang typed/scheme
#:optimize

(+ 1.0+2.0i 2.0 3.0+6.0i)
(- 1.0 2.0+4.0i 3.0+6.0i)
(- 1.0+2.0i 2.0 3.0+6.0i)
(- 1.0+2.0i 2.0+4.0i 3.0)
