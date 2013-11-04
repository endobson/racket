#;#;
#<<END
TR missed opt: float-complex-float-div.rkt 55:9 1.0 -- non-complex value in complex arithmetic
TR missed opt: float-complex-float-div.rkt 56:18 2.0 -- non-complex value in complex arithmetic
TR missed opt: float-complex-float-div.rkt 57:9 1.0 -- non-complex value in complex arithmetic
TR missed opt: float-complex-float-div.rkt 58:18 2.0 -- non-complex value in complex arithmetic
TR missed opt: float-complex-float-div.rkt 59:27 3.0 -- non-complex value in complex arithmetic
TR missed opt: float-complex-float-div.rkt 60:18 2.0 -- non-complex value in complex arithmetic
TR missed opt: float-complex-float-div.rkt 60:22 3.0 -- non-complex value in complex arithmetic
TR missed opt: float-complex-float-div.rkt 61:13 2.0 -- non-complex value in complex arithmetic
TR missed opt: float-complex-float-div.rkt 61:9 1.0 -- non-complex value in complex arithmetic
TR opt: float-complex-float-div.rkt 52:51 (real-part x) -- complex accessor elimination
TR opt: float-complex-float-div.rkt 52:62 x -- unbox float-complex
TR opt: float-complex-float-div.rkt 53:51 (imag-part x) -- complex accessor elimination
TR opt: float-complex-float-div.rkt 53:62 x -- unbox float-complex
TR opt: float-complex-float-div.rkt 55:13 2.0+4.0i -- unboxed literal
TR opt: float-complex-float-div.rkt 55:6 (/ 1.0 2.0+4.0i) -- unboxed binary float complex
TR opt: float-complex-float-div.rkt 55:9 1.0 -- float in complex ops
TR opt: float-complex-float-div.rkt 56:18 2.0 -- float in complex ops
TR opt: float-complex-float-div.rkt 56:6 (/ 1.0+2.0i 2.0) -- unboxed binary float complex
TR opt: float-complex-float-div.rkt 56:9 1.0+2.0i -- unboxed literal
TR opt: float-complex-float-div.rkt 57:13 2.0+4.0i -- unboxed literal
TR opt: float-complex-float-div.rkt 57:22 3.0+6.0i -- unboxed literal
TR opt: float-complex-float-div.rkt 57:6 (/ 1.0 2.0+4.0i 3.0+6.0i) -- unboxed binary float complex
TR opt: float-complex-float-div.rkt 57:9 1.0 -- float in complex ops
TR opt: float-complex-float-div.rkt 58:18 2.0 -- float in complex ops
TR opt: float-complex-float-div.rkt 58:22 3.0+6.0i -- unboxed literal
TR opt: float-complex-float-div.rkt 58:6 (/ 1.0+2.0i 2.0 3.0+6.0i) -- unboxed binary float complex
TR opt: float-complex-float-div.rkt 58:9 1.0+2.0i -- unboxed literal
TR opt: float-complex-float-div.rkt 59:18 2.0+4.0i -- unboxed literal
TR opt: float-complex-float-div.rkt 59:27 3.0 -- float in complex ops
TR opt: float-complex-float-div.rkt 59:6 (/ 1.0+2.0i 2.0+4.0i 3.0) -- unboxed binary float complex
TR opt: float-complex-float-div.rkt 59:9 1.0+2.0i -- unboxed literal
TR opt: float-complex-float-div.rkt 60:18 2.0 -- float in complex ops
TR opt: float-complex-float-div.rkt 60:22 3.0 -- float in complex ops
TR opt: float-complex-float-div.rkt 60:6 (/ 1.0+2.0i 2.0 3.0) -- unboxed binary float complex
TR opt: float-complex-float-div.rkt 60:9 1.0+2.0i -- unboxed literal
TR opt: float-complex-float-div.rkt 61:13 2.0 -- float in complex ops
TR opt: float-complex-float-div.rkt 61:17 3.0+6.0i -- unboxed literal
TR opt: float-complex-float-div.rkt 61:6 (/ 1.0 2.0 3.0+6.0i) -- unboxed binary float complex
TR opt: float-complex-float-div.rkt 61:9 1.0 -- float in complex ops
END
#<<END
'("0.1000000000-0.2000000000" "0.50000000001.0000000000" "-0.0200000000-0.0266666667" "0.16666666670.0000000000" "0.16666666670.0000000000" "0.16666666670.3333333333" "0.0333333333-0.0666666667")

END

#lang typed/scheme
#:optimize

(map (lambda: ((x : Float-Complex))
              (string-append (real->decimal-string (real-part x) 10)
                             (real->decimal-string (imag-part x) 10)))
     (list
      (/ 1.0 2.0+4.0i)
      (/ 1.0+2.0i 2.0)
      (/ 1.0 2.0+4.0i 3.0+6.0i)
      (/ 1.0+2.0i 2.0 3.0+6.0i)
      (/ 1.0+2.0i 2.0+4.0i 3.0)
      (/ 1.0+2.0i 2.0 3.0)
      (/ 1.0 2.0 3.0+6.0i)))
