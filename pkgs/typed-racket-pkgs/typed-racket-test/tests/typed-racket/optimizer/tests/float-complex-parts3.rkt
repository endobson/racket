#;#;
#<<END
TR missed opt: float-complex-parts3.rkt 49:12 (real-part (+ 2.0+4.0i 3.0+6.0i)) -- Non complex value in complex arithmetic
TR missed opt: float-complex-parts3.rkt 50:12 (unsafe-flreal-part (+ 2.0+4.0i 3.0+6.0i)) -- Non complex value in complex arithmetic
TR missed opt: float-complex-parts3.rkt 51:12 (imag-part (+ 2.0+4.0i 3.0+6.0i)) -- Non complex value in complex arithmetic
TR missed opt: float-complex-parts3.rkt 52:12 (unsafe-flimag-part (+ 2.0+4.0i 3.0+6.0i)) -- Non complex value in complex arithmetic
TR opt: float-complex-parts3.rkt 49:0 (+ 1.0+2.0i (real-part (+ 2.0+4.0i 3.0+6.0i))) -- unboxed binary float complex
TR opt: float-complex-parts3.rkt 49:12 (real-part (+ 2.0+4.0i 3.0+6.0i)) -- complex accessor elimination
TR opt: float-complex-parts3.rkt 49:12 (real-part (+ 2.0+4.0i 3.0+6.0i)) -- float in complex ops
TR opt: float-complex-parts3.rkt 49:23 (+ 2.0+4.0i 3.0+6.0i) -- unboxed binary float complex
TR opt: float-complex-parts3.rkt 49:26 2.0+4.0i -- unboxed literal
TR opt: float-complex-parts3.rkt 49:3 1.0+2.0i -- unboxed literal
TR opt: float-complex-parts3.rkt 49:35 3.0+6.0i -- unboxed literal
TR opt: float-complex-parts3.rkt 50:0 (+ 1.0+2.0i (unsafe-flreal-part (+ 2.0+4.0i 3.0+6.0i))) -- unboxed binary float complex
TR opt: float-complex-parts3.rkt 50:12 (unsafe-flreal-part (+ 2.0+4.0i 3.0+6.0i)) -- complex accessor elimination
TR opt: float-complex-parts3.rkt 50:12 (unsafe-flreal-part (+ 2.0+4.0i 3.0+6.0i)) -- float in complex ops
TR opt: float-complex-parts3.rkt 50:3 1.0+2.0i -- unboxed literal
TR opt: float-complex-parts3.rkt 50:32 (+ 2.0+4.0i 3.0+6.0i) -- unboxed binary float complex
TR opt: float-complex-parts3.rkt 50:35 2.0+4.0i -- unboxed literal
TR opt: float-complex-parts3.rkt 50:44 3.0+6.0i -- unboxed literal
TR opt: float-complex-parts3.rkt 51:0 (+ 1.0+2.0i (imag-part (+ 2.0+4.0i 3.0+6.0i))) -- unboxed binary float complex
TR opt: float-complex-parts3.rkt 51:12 (imag-part (+ 2.0+4.0i 3.0+6.0i)) -- complex accessor elimination
TR opt: float-complex-parts3.rkt 51:12 (imag-part (+ 2.0+4.0i 3.0+6.0i)) -- float in complex ops
TR opt: float-complex-parts3.rkt 51:23 (+ 2.0+4.0i 3.0+6.0i) -- unboxed binary float complex
TR opt: float-complex-parts3.rkt 51:26 2.0+4.0i -- unboxed literal
TR opt: float-complex-parts3.rkt 51:3 1.0+2.0i -- unboxed literal
TR opt: float-complex-parts3.rkt 51:35 3.0+6.0i -- unboxed literal
TR opt: float-complex-parts3.rkt 52:0 (+ 1.0+2.0i (unsafe-flimag-part (+ 2.0+4.0i 3.0+6.0i))) -- unboxed binary float complex
TR opt: float-complex-parts3.rkt 52:12 (unsafe-flimag-part (+ 2.0+4.0i 3.0+6.0i)) -- complex accessor elimination
TR opt: float-complex-parts3.rkt 52:12 (unsafe-flimag-part (+ 2.0+4.0i 3.0+6.0i)) -- float in complex ops
TR opt: float-complex-parts3.rkt 52:3 1.0+2.0i -- unboxed literal
TR opt: float-complex-parts3.rkt 52:32 (+ 2.0+4.0i 3.0+6.0i) -- unboxed binary float complex
TR opt: float-complex-parts3.rkt 52:35 2.0+4.0i -- unboxed literal
TR opt: float-complex-parts3.rkt 52:44 3.0+6.0i -- unboxed literal
END
#<<END
6.0+2.0i
6.0+2.0i
11.0+2.0i
11.0+2.0i

END

#lang typed/scheme
#:optimize

(require racket/unsafe/ops)

(+ 1.0+2.0i (real-part (+ 2.0+4.0i 3.0+6.0i)))
(+ 1.0+2.0i (unsafe-flreal-part (+ 2.0+4.0i 3.0+6.0i)))
(+ 1.0+2.0i (imag-part (+ 2.0+4.0i 3.0+6.0i)))
(+ 1.0+2.0i (unsafe-flimag-part (+ 2.0+4.0i 3.0+6.0i)))
