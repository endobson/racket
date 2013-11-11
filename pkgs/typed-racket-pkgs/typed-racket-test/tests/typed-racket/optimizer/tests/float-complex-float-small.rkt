#;#;
#<<END
TR opt: float-complex-float-small.rkt 27:0 (+ 1.0+2.0i 3.0) -- unboxed float complex: addition
TR opt: float-complex-float-small.rkt 27:3 1.0+2.0i -- unbox float-complex
TR opt: float-complex-float-small.rkt 28:0 (+ 1.0 2.0+4.0i) -- unboxed float complex: addition
TR opt: float-complex-float-small.rkt 28:7 2.0+4.0i -- unbox float-complex
TR opt: float-complex-float-small.rkt 29:0 (- 1.0+2.0i 3.0) -- unboxed float complex: subtraction
TR opt: float-complex-float-small.rkt 29:3 1.0+2.0i -- unbox float-complex
TR opt: float-complex-float-small.rkt 30:0 (- 1.0 2.0+4.0i) -- unboxed float complex: subtraction
TR opt: float-complex-float-small.rkt 30:7 2.0+4.0i -- unbox float-complex
TR opt: float-complex-float-small.rkt 31:0 (+ 1.0+2.0i (+ 1.0 2.0)) -- unboxed float complex: addition
TR opt: float-complex-float-small.rkt 31:12 (+ 1.0 2.0) -- binary float
TR opt: float-complex-float-small.rkt 31:3 1.0+2.0i -- unbox float-complex
END
#<<END
4.0+2.0i
3.0+4.0i
-2.0+2.0i
-1.0-4.0i
4.0+2.0i

END

#lang typed/scheme
#:optimize

(+ 1.0+2.0i 3.0)
(+ 1.0 2.0+4.0i)
(- 1.0+2.0i 3.0)
(- 1.0 2.0+4.0i)
(+ 1.0+2.0i (+ 1.0 2.0))
