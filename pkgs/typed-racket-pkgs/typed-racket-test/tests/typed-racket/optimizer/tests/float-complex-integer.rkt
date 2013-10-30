#;#;
#<<END
TR missed opt: float-complex-integer.rkt 16:3 (expt 2 100) -- Non complex value in complex arithmetic
TR opt: float-complex-integer.rkt 16:0 (+ (expt 2 100) 1.0+2.0i) -- unboxed binary float complex
TR opt: float-complex-integer.rkt 16:16 1.0+2.0i -- unboxed literal
TR opt: float-complex-integer.rkt 16:3 (expt 2 100) -- non float complex in complex ops
END
#<<END
1.2676506002282294e+30+2.0i

END

#lang typed/scheme
#:optimize

(+ (expt 2 100) 1.0+2.0i)
