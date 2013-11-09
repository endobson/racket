#;#;
#<<END
TR opt: float-complex-div3.rkt 22:0 (/ 1.0 0.0+1.0i) -- unboxed binary float complex
TR opt: float-complex-div3.rkt 22:7 0.0+1.0i -- unboxed literal
TR opt: float-complex-div3.rkt 23:0 (/ 1.0 1.0+0.0i) -- unboxed binary float complex
TR opt: float-complex-div3.rkt 23:7 1.0+0.0i -- unboxed literal
TR opt: float-complex-div3.rkt 24:0 (/ 1.0+0.0i 0.0+1.0i) -- unboxed binary float complex
TR opt: float-complex-div3.rkt 24:12 0.0+1.0i -- unboxed literal
TR opt: float-complex-div3.rkt 24:3 1.0+0.0i -- unboxed literal
TR opt: float-complex-div3.rkt 25:0 (/ 1.0+0.0i 1.0+0.0i) -- unboxed binary float complex
TR opt: float-complex-div3.rkt 25:12 1.0+0.0i -- unboxed literal
TR opt: float-complex-div3.rkt 25:3 1.0+0.0i -- unboxed literal
END
#<<END
0.0-1.0i
1.0-0.0i
0.0-1.0i
1.0+0.0i

END
#lang typed/racket
(/ 1.0 0.0+1.0i)
(/ 1.0 1.0+0.0i)
(/ 1.0+0.0i 0.0+1.0i)
(/ 1.0+0.0i 1.0+0.0i)

