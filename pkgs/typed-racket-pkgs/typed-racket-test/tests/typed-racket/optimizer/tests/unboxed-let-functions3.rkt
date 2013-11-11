#;#;
#<<END
TR opt: unboxed-let-functions3.rkt 22:20 x -- unboxed var -> table
TR opt: unboxed-let-functions3.rkt 22:7 f -- fun -> unboxed fun
TR opt: unboxed-let-functions3.rkt 23:18 (+ x y) -- unboxed float complex: addition
TR opt: unboxed-let-functions3.rkt 23:21 x -- leave var unboxed
TR opt: unboxed-let-functions3.rkt 24:17 2.0+4.0i -- unbox float-complex
TR opt: unboxed-let-functions3.rkt 24:2 (f (+ 1.0+2.0i 2.0+4.0i) 3.0) -- call to fun with unboxed args
TR opt: unboxed-let-functions3.rkt 24:2 (f (+ 1.0+2.0i 2.0+4.0i) 3.0) -- unboxed call site
TR opt: unboxed-let-functions3.rkt 24:5 (+ 1.0+2.0i 2.0+4.0i) -- unboxed float complex: addition
TR opt: unboxed-let-functions3.rkt 24:8 1.0+2.0i -- unbox float-complex
END
#<<END
6.0+6.0i

END

#lang typed/scheme
#:optimize

;; function with a mix of complex and non-complex args
(let ((f (lambda: ((x : Float-Complex) (y : Float))
                  (+ x y))))
  (f (+ 1.0+2.0i 2.0+4.0i)
     3.0))
