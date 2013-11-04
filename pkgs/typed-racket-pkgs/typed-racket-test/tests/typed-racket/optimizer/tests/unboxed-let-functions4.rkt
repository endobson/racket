#;#;
#<<END
TR opt: unboxed-let-functions4.rkt 22:32 x -- unboxed var -> table
TR opt: unboxed-let-functions4.rkt 22:7 f -- fun -> unboxed fun
TR opt: unboxed-let-functions4.rkt 23:18 (+ x y) -- unboxed float complex addition
TR opt: unboxed-let-functions4.rkt 23:21 x -- leave var unboxed
TR opt: unboxed-let-functions4.rkt 24:2 (f 3.0 (+ 1.0+2.0i 2.0+4.0i)) -- call to fun with unboxed args
TR opt: unboxed-let-functions4.rkt 24:2 (f 3.0 (+ 1.0+2.0i 2.0+4.0i)) -- unboxed call site
TR opt: unboxed-let-functions4.rkt 25:17 2.0+4.0i -- unboxed literal
TR opt: unboxed-let-functions4.rkt 25:5 (+ 1.0+2.0i 2.0+4.0i) -- unboxed float complex addition
TR opt: unboxed-let-functions4.rkt 25:8 1.0+2.0i -- unboxed literal
END
#<<END
6.0+6.0i

END

#lang typed/scheme
#:optimize

;; function with a mix of complex and non-complex args, non-complex first
(let ((f (lambda: ((y : Float) (x : Float-Complex))
                  (+ x y))))
  (f 3.0
     (+ 1.0+2.0i 2.0+4.0i)))
