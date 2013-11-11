#;#;
#<<END
TR opt: float-complex-float.rkt 27:0 (+ 1.0+2.0i 2.0 3.0+6.0i) -- unboxed float complex addition
TR opt: float-complex-float.rkt 27:16 3.0+6.0i -- unboxed literal
TR opt: float-complex-float.rkt 27:3 1.0+2.0i -- unboxed literal
TR opt: float-complex-float.rkt 28:0 (- 1.0 2.0+4.0i 3.0+6.0i) -- unboxed float complex subtraction
TR opt: float-complex-float.rkt 28:16 3.0+6.0i -- unboxed literal
TR opt: float-complex-float.rkt 28:7 2.0+4.0i -- unboxed literal
TR opt: float-complex-float.rkt 29:0 (- 1.0+2.0i 2.0 3.0+6.0i) -- unboxed float complex subtraction
TR opt: float-complex-float.rkt 29:16 3.0+6.0i -- unboxed literal
TR opt: float-complex-float.rkt 29:3 1.0+2.0i -- unboxed literal
TR opt: float-complex-float.rkt 30:0 (- 1.0+2.0i 2.0+4.0i 3.0) -- unboxed float complex subtraction
TR opt: float-complex-float.rkt 30:12 2.0+4.0i -- unboxed literal
TR opt: float-complex-float.rkt 30:3 1.0+2.0i -- unboxed literal
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
