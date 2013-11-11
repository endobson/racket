#;#;
#<<END
TR opt: float-complex-float-div.rkt 34:51 (real-part x) -- complex accessor elimination
TR opt: float-complex-float-div.rkt 34:62 x -- unbox float-complex
TR opt: float-complex-float-div.rkt 35:51 (imag-part x) -- complex accessor elimination
TR opt: float-complex-float-div.rkt 35:62 x -- unbox float-complex
TR opt: float-complex-float-div.rkt 37:13 2.0+4.0i -- unboxed literal
TR opt: float-complex-float-div.rkt 37:6 (/ 1.0 2.0+4.0i) -- unboxed float complex division
TR opt: float-complex-float-div.rkt 38:6 (/ 1.0+2.0i 2.0) -- unboxed float complex division
TR opt: float-complex-float-div.rkt 38:9 1.0+2.0i -- unboxed literal
TR opt: float-complex-float-div.rkt 39:13 2.0+4.0i -- unboxed literal
TR opt: float-complex-float-div.rkt 39:22 3.0+6.0i -- unboxed literal
TR opt: float-complex-float-div.rkt 39:6 (/ 1.0 2.0+4.0i 3.0+6.0i) -- unboxed float complex division
TR opt: float-complex-float-div.rkt 40:22 3.0+6.0i -- unboxed literal
TR opt: float-complex-float-div.rkt 40:6 (/ 1.0+2.0i 2.0 3.0+6.0i) -- unboxed float complex division
TR opt: float-complex-float-div.rkt 40:9 1.0+2.0i -- unboxed literal
TR opt: float-complex-float-div.rkt 41:18 2.0+4.0i -- unboxed literal
TR opt: float-complex-float-div.rkt 41:6 (/ 1.0+2.0i 2.0+4.0i 3.0) -- unboxed float complex division
TR opt: float-complex-float-div.rkt 41:9 1.0+2.0i -- unboxed literal
TR opt: float-complex-float-div.rkt 42:6 (/ 1.0+2.0i 2.0 3.0) -- unboxed float complex division
TR opt: float-complex-float-div.rkt 42:9 1.0+2.0i -- unboxed literal
TR opt: float-complex-float-div.rkt 43:17 3.0+6.0i -- unboxed literal
TR opt: float-complex-float-div.rkt 43:6 (/ 1.0 2.0 3.0+6.0i) -- unboxed float complex division
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
