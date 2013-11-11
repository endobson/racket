#;#;
#<<END
TR missed opt: float-complex-i.rkt 16:12 (* 0+1.0i 2.0+4.0i) -- Non float complex value in complex arithmetic
TR opt: float-complex-i.rkt 16:0 (+ 1.0+2.0i (* 0+1.0i 2.0+4.0i)) -- unboxed float complex: addition
TR opt: float-complex-i.rkt 16:12 (* 0+1.0i 2.0+4.0i) -- non float complex in complex ops
TR opt: float-complex-i.rkt 16:3 1.0+2.0i -- unboxed literal
END
#<<END
-3.0+4.0i

END

#lang typed/scheme
#:optimize

(+ 1.0+2.0i (* +1.0i 2.0+4.0i))
