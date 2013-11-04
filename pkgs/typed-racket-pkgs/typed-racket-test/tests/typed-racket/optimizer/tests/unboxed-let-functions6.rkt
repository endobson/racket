#;#;
#<<END
TR missed opt: unboxed-let-functions6.rkt 32:21 (car l) -- non-complex value in complex arithmetic
TR opt: unboxed-let-functions6.rkt 28:0 (let: loop : Float-Complex ((z : Float-Complex 0.0+0.0i) (l : (Listof Integer) (quote (1 2 3)))) (if (null? l) (+ z 0.0+1.0i) (loop (+ z (car l)) (cdr l)))) -- unboxed call site
TR opt: unboxed-let-functions6.rkt 28:31 z -- unboxed var -> table
TR opt: unboxed-let-functions6.rkt 28:51 0.0+0.0i -- unboxed literal
TR opt: unboxed-let-functions6.rkt 28:6 loop -- fun -> unboxed fun
TR opt: unboxed-let-functions6.rkt 28:6 loop -- unboxed let loop
TR opt: unboxed-let-functions6.rkt 31:10 (+ z 0.0+1.0i) -- unboxed float complex addition
TR opt: unboxed-let-functions6.rkt 31:13 z -- leave var unboxed
TR opt: unboxed-let-functions6.rkt 31:15 0.0+1.0i -- unboxed literal
TR opt: unboxed-let-functions6.rkt 32:10 (loop (+ z (car l)) (cdr l)) -- call to fun with unboxed args
TR opt: unboxed-let-functions6.rkt 32:10 (loop (+ z (car l)) (cdr l)) -- unboxed call site
TR opt: unboxed-let-functions6.rkt 32:16 (+ z (car l)) -- unboxed float complex addition
TR opt: unboxed-let-functions6.rkt 32:19 z -- leave var unboxed
TR opt: unboxed-let-functions6.rkt 32:21 (car l) -- non float real in complex ops
TR opt: unboxed-let-functions6.rkt 32:21 (car l) -- pair
TR opt: unboxed-let-functions6.rkt 33:16 (cdr l) -- pair
END
#<<END
6.0+1.0i

END

#lang typed/scheme
#:optimize

(let: loop :   Float-Complex ((z : Float-Complex   0.0+0.0i)
                              (l : (Listof Integer) '(1 2 3)))
      (if (null? l)
          (+ z 0.0+1.0i)
          (loop (+ z (car l))
                (cdr l))))
