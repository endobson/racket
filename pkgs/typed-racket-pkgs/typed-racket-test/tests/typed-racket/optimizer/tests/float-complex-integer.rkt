#;#;
#<<END
TR missed opt: float-complex-integer.rkt 16:3 (expt 2 100) -- Non float complex value in complex arithmetic
TR opt: float-complex-integer.rkt 16:0 (+ (expt 2 100) 1.0+2.0i) -- unboxed float complex: addition
TR opt: float-complex-integer.rkt 16:16 1.0+2.0i -- unbox float-complex
TR opt: float-complex-integer.rkt 16:3 (expt 2 100) -- non float real in complex ops
END
#<<END
1.2676506002282294e+30+2.0i

END

#lang typed/scheme
#:optimize

(+ (expt 2 100) 1.0+2.0i)
