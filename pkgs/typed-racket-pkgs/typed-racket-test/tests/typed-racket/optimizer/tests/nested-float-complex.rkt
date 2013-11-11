#;#;
#<<END
TR opt: nested-float-complex.rkt 17:0 (+ 1.0+2.0i (- 2.0+4.0i 3.0+6.0i)) -- unboxed float complex: addition
TR opt: nested-float-complex.rkt 17:12 (- 2.0+4.0i 3.0+6.0i) -- unboxed float complex: subtraction
TR opt: nested-float-complex.rkt 17:15 2.0+4.0i -- unbox float-complex
TR opt: nested-float-complex.rkt 17:24 3.0+6.0i -- unbox float-complex
TR opt: nested-float-complex.rkt 17:3 1.0+2.0i -- unbox float-complex
END
#<<END
0.0+0.0i

END

#lang typed/scheme
#:optimize

(+ 1.0+2.0i (- 2.0+4.0i 3.0+6.0i))
