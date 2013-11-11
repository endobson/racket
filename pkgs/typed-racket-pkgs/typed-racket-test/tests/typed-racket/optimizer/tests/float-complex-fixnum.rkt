#;#;
#<<END
TR missed opt: float-complex-fixnum.rkt 18:3 (modulo 2 1) -- Non float complex value in complex arithmetic
TR opt: float-complex-fixnum.rkt 18:0 (+ (modulo 2 1) 1.0+2.0i 3.0+6.0i) -- unboxed float complex: addition
TR opt: float-complex-fixnum.rkt 18:16 1.0+2.0i -- unbox float-complex
TR opt: float-complex-fixnum.rkt 18:25 3.0+6.0i -- unbox float-complex
TR opt: float-complex-fixnum.rkt 18:3 (modulo 2 1) -- binary nonzero fixnum
TR opt: float-complex-fixnum.rkt 18:3 (modulo 2 1) -- non float real in complex ops
END
#<<END
4.0+8.0i

END

#lang typed/scheme
#:optimize

(+ (modulo 2 1) 1.0+2.0i 3.0+6.0i)
