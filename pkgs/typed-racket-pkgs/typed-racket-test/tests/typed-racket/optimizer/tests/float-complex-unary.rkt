#;#;
#<<END
TR opt: float-complex-unary.rkt 28:0 (real-part (+ 1.0+2.0i (+ (* 3.0+4.0i 5.0+6.0i)))) -- complex accessor elimination
TR opt: float-complex-unary.rkt 28:11 (+ 1.0+2.0i (+ (* 3.0+4.0i 5.0+6.0i))) -- unboxed float complex addition
TR opt: float-complex-unary.rkt 28:14 1.0+2.0i -- unboxed literal
TR opt: float-complex-unary.rkt 28:23 (+ (* 3.0+4.0i 5.0+6.0i)) -- unboxed float complex addition
TR opt: float-complex-unary.rkt 28:26 (* 3.0+4.0i 5.0+6.0i) -- unboxed binary float complex
TR opt: float-complex-unary.rkt 28:29 3.0+4.0i -- unboxed literal
TR opt: float-complex-unary.rkt 28:38 5.0+6.0i -- unboxed literal
TR opt: float-complex-unary.rkt 29:0 (real-part (+ 7.0+8.0i (* (+ 9.0+10.0i 11.0+12.0i)))) -- complex accessor elimination
TR opt: float-complex-unary.rkt 29:11 (+ 7.0+8.0i (* (+ 9.0+10.0i 11.0+12.0i))) -- unboxed float complex addition
TR opt: float-complex-unary.rkt 29:14 7.0+8.0i -- unboxed literal
TR opt: float-complex-unary.rkt 29:23 (* (+ 9.0+10.0i 11.0+12.0i)) -- unboxed unary float complex
TR opt: float-complex-unary.rkt 29:26 (+ 9.0+10.0i 11.0+12.0i) -- unboxed float complex addition
TR opt: float-complex-unary.rkt 29:26 (+ 9.0+10.0i 11.0+12.0i) -- unboxed float complex addition
TR opt: float-complex-unary.rkt 29:29 9.0+10.0i -- unboxed literal
TR opt: float-complex-unary.rkt 29:29 9.0+10.0i -- unboxed literal
TR opt: float-complex-unary.rkt 29:39 11.0+12.0i -- unboxed literal
TR opt: float-complex-unary.rkt 29:39 11.0+12.0i -- unboxed literal
END
#<<END
-8.0
27.0

END
#lang typed/racket

(real-part (+ 1.0+2.0i (+ (* 3.0+4.0i 5.0+6.0i))))
(real-part (+ 7.0+8.0i (* (+ 9.0+10.0i 11.0+12.0i))))

