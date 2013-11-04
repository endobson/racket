#;#;
#<<END
TR missed opt: complex-exp.rkt 15:3 (exp 0) -- non-complex value in complex arithmetic
TR opt: complex-exp.rkt 15:0 (- (exp 0) 1.0+2.0i) -- unboxed binary float complex
TR opt: complex-exp.rkt 15:11 1.0+2.0i -- unboxed literal
TR opt: complex-exp.rkt 15:3 (exp 0) -- non float complex in complex ops
END
#<<END
0.0-2.0i

END

#lang typed/racket

(- (exp 0) 1.0+2.0i)
