#;#;
#<<END
TR info: float-complex-sin.rkt 22:13 (sin (* t 6.28)) -- exact real arith
TR info: float-complex-sin.rkt 22:18 (* t 6.28) -- exact real arith
TR missed opt: float-complex-sin.rkt 22:13 (sin (* t 6.28)) -- Non float complex value in complex arithmetic
TR missed opt: float-complex-sin.rkt 22:13 (sin (* t 6.28)) -- all args float-arg-expr, result not Float -- caused by: 22:18 (* t 6.28)
TR missed opt: float-complex-sin.rkt 22:18 (* t 6.28) -- all args float-arg-expr, result not Float -- caused by: 22:21 t
TR opt: float-complex-sin.rkt 22:10 (+ (sin (* t 6.28)) 0.0+0.0i) -- unboxed float complex: addition
TR opt: float-complex-sin.rkt 22:13 (sin (* t 6.28)) -- non float real in complex ops
TR opt: float-complex-sin.rkt 22:30 0.0+0.0i -- unbox float-complex
END
#<<END
-0.0031853017931379904+0.0i

END



#lang typed/scheme
#:optimize
((lambda: ((t : Integer))
          (+ (sin (* t 6.28)) 0.0+0.0i))
 1)
