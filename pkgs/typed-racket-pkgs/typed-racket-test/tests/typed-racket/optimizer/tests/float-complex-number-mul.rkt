#;#;
#<<END
TR missed opt: float-complex-number-mul.rkt 15:23 (* 1 2) -- non-complex value in complex arithmetic
TR opt: float-complex-number-mul.rkt 15:0 (imag-part (* 0.0+1.0i (* 1 2))) -- complex accessor elimination
TR opt: float-complex-number-mul.rkt 15:11 (* 0.0+1.0i (* 1 2)) -- unboxed binary float complex
TR opt: float-complex-number-mul.rkt 15:14 0.0+1.0i -- unboxed literal
TR opt: float-complex-number-mul.rkt 15:23 (* 1 2) -- fixnum bounded expr
TR opt: float-complex-number-mul.rkt 15:23 (* 1 2) -- non float complex in complex ops
END
#<<END
2.0

END
#lang typed/racket
(imag-part (* 0.0+1.0i (* 1 2)))
