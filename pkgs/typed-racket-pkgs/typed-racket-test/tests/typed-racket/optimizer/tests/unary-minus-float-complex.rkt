#;#;
#<<END
TR opt: unary-minus-float-complex.rkt 12:0 (imag-part (- 0.0+0.0i)) -- complex accessor elimination
TR opt: unary-minus-float-complex.rkt 12:11 (- 0.0+0.0i) -- unboxed float complex: subtraction
TR opt: unary-minus-float-complex.rkt 12:14 0.0+0.0i -- unboxed literal
END
#<<END
-0.0

END
#lang typed/racket
(imag-part (- 0.0+0.0i))
