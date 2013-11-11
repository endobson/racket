#;#;
#<<END
TR opt: float-complex-unary.rkt 25:0 (real-part (+ 1.0+2.0i (+ (* 3.0+4.0i 5.0+6.0i)))) -- complex accessor elimination
TR opt: float-complex-unary.rkt 25:11 (+ 1.0+2.0i (+ (* 3.0+4.0i 5.0+6.0i))) -- unboxed float complex: addition
TR opt: float-complex-unary.rkt 25:14 1.0+2.0i -- unbox float-complex
TR opt: float-complex-unary.rkt 25:23 (+ (* 3.0+4.0i 5.0+6.0i)) -- unboxed float complex: addition
TR opt: float-complex-unary.rkt 25:26 (* 3.0+4.0i 5.0+6.0i) -- unboxed float complex: multiplication
TR opt: float-complex-unary.rkt 25:29 3.0+4.0i -- unbox float-complex
TR opt: float-complex-unary.rkt 25:38 5.0+6.0i -- unbox float-complex
TR opt: float-complex-unary.rkt 26:0 (real-part (+ 7.0+8.0i (* (+ 9.0+10.0i 11.0+12.0i)))) -- complex accessor elimination
TR opt: float-complex-unary.rkt 26:11 (+ 7.0+8.0i (* (+ 9.0+10.0i 11.0+12.0i))) -- unboxed float complex: addition
TR opt: float-complex-unary.rkt 26:14 7.0+8.0i -- unbox float-complex
TR opt: float-complex-unary.rkt 26:23 (* (+ 9.0+10.0i 11.0+12.0i)) -- unboxed float complex: multiplication
TR opt: float-complex-unary.rkt 26:26 (+ 9.0+10.0i 11.0+12.0i) -- unboxed float complex: addition
TR opt: float-complex-unary.rkt 26:29 9.0+10.0i -- unbox float-complex
TR opt: float-complex-unary.rkt 26:39 11.0+12.0i -- unbox float-complex
END
#<<END
-8.0
27.0

END
#lang typed/racket

(real-part (+ 1.0+2.0i (+ (* 3.0+4.0i 5.0+6.0i))))
(real-part (+ 7.0+8.0i (* (+ 9.0+10.0i 11.0+12.0i))))

