#;#;
#<<END
TR opt: real-part-loop.rkt 28:1 (let loop ((v 0.0+1.0i)) (if (> (real-part v) 70000.2) 0 (loop (+ v 3.6)))) -- unboxed call site
TR opt: real-part-loop.rkt 28:13 v -- unboxed var -> table
TR opt: real-part-loop.rkt 28:15 0.0+1.0i -- unboxed literal
TR opt: real-part-loop.rkt 28:6 loop -- fun -> unboxed fun
TR opt: real-part-loop.rkt 28:6 loop -- unboxed let loop
TR opt: real-part-loop.rkt 29:20 v -- leave var unboxed
TR opt: real-part-loop.rkt 29:6 (> (real-part v) 70000.2) -- binary float comp
TR opt: real-part-loop.rkt 29:9 (real-part v) -- complex accessor elimination
TR opt: real-part-loop.rkt 31:12 (+ v 3.6) -- unboxed float complex addition
TR opt: real-part-loop.rkt 31:15 v -- leave var unboxed
TR opt: real-part-loop.rkt 31:6 (loop (+ v 3.6)) -- call to fun with unboxed args
TR opt: real-part-loop.rkt 31:6 (loop (+ v 3.6)) -- unboxed call site
END
#<<END
0

END


#lang typed/racket/base
#:optimize



(ann
 (let loop ([v 0.0+1.0i])
  (if (> (real-part v) 70000.2)
      0
      (loop (+ v 3.6))))
 Integer)
