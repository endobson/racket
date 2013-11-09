#;#;
#<<END
TR opt: float-complex-div3.rkt 32:0 (/ 1.0 0.0+1.0i) -- unboxed binary float complex
TR opt: float-complex-div3.rkt 32:7 0.0+1.0i -- unboxed literal
TR opt: float-complex-div3.rkt 33:0 (/ 1.0 1.0+0.0i) -- unboxed binary float complex
TR opt: float-complex-div3.rkt 33:7 1.0+0.0i -- unboxed literal
TR opt: float-complex-div3.rkt 36:0 (/ 1.0+0.0i 0.0+1.0i) -- unboxed binary float complex
TR opt: float-complex-div3.rkt 36:12 0.0+1.0i -- unboxed literal
TR opt: float-complex-div3.rkt 36:3 1.0+0.0i -- unboxed literal
TR opt: float-complex-div3.rkt 37:0 (/ 1.0+0.0i 1.0+0.0i) -- unboxed binary float complex
TR opt: float-complex-div3.rkt 37:12 1.0+0.0i -- unboxed literal
TR opt: float-complex-div3.rkt 37:3 1.0+0.0i -- unboxed literal
TR opt: float-complex-div3.rkt 38:0 (/ 0.0+1.0i 0.0+1.0i) -- unboxed binary float complex
TR opt: float-complex-div3.rkt 38:12 0.0+1.0i -- unboxed literal
TR opt: float-complex-div3.rkt 38:3 0.0+1.0i -- unboxed literal
TR opt: float-complex-div3.rkt 39:0 (/ 0.0+1.0i 1.0+0.0i) -- unboxed binary float complex
TR opt: float-complex-div3.rkt 39:12 1.0+0.0i -- unboxed literal
TR opt: float-complex-div3.rkt 39:3 0.0+1.0i -- unboxed literal
END
#<<END
0.0-1.0i
1.0-0.0i
1.0+0.0i
0.0+1.0i
0.0-1.0i
1.0+0.0i
1.0+0.0i
0.0+1.0i

END
#lang typed/racket
(/ 1.0 0.0+1.0i)
(/ 1.0 1.0+0.0i)
(/ 0+1.0i 0.0+1.0i)
(/ 0+1.0i 1.0+0.0i)
(/ 1.0+0.0i 0.0+1.0i)
(/ 1.0+0.0i 1.0+0.0i)
(/ 0.0+1.0i 0.0+1.0i)
(/ 0.0+1.0i 1.0+0.0i)

