#;#;
#<<END
TR missed opt: maybe-exact-complex.rkt 18:12 2+4i -- Non float complex value in complex arithmetic
TR opt: maybe-exact-complex.rkt 18:0 (+ 1.0+2.0i 2+4i) -- unboxed float complex: addition
TR opt: maybe-exact-complex.rkt 18:12 2+4i -- non float complex in complex ops
TR opt: maybe-exact-complex.rkt 18:3 1.0+2.0i -- unbox float-complex
END
#<<END
3.0+6.0i

END

#lang typed/scheme
#:optimize



(+ 1.0+2.0i 2+4i)
