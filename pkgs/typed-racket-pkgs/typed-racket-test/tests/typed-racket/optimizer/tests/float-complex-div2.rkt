#;#;
#<<END
TR missed opt: float-complex-div2.rkt 114:3 1 -- Non float complex value in complex arithmetic
TR missed opt: float-complex-div2.rkt 115:12 1 -- Non float complex value in complex arithmetic
TR missed opt: float-complex-div2.rkt 116:3 1 -- Non float complex value in complex arithmetic
TR missed opt: float-complex-div2.rkt 117:12 1 -- Non float complex value in complex arithmetic
TR missed opt: float-complex-div2.rkt 118:21 1 -- Non float complex value in complex arithmetic
TR missed opt: float-complex-div2.rkt 119:3 1 -- Non float complex value in complex arithmetic
TR missed opt: float-complex-div2.rkt 119:5 1 -- Non float complex value in complex arithmetic
TR missed opt: float-complex-div2.rkt 120:14 1 -- Non float complex value in complex arithmetic
TR missed opt: float-complex-div2.rkt 120:3 1 -- Non float complex value in complex arithmetic
TR missed opt: float-complex-div2.rkt 121:12 1 -- Non float complex value in complex arithmetic
TR missed opt: float-complex-div2.rkt 121:14 1 -- Non float complex value in complex arithmetic
TR opt: float-complex-div2.rkt 100:0 (/ 1.0+1.0i 1.0+1.0i) -- unboxed binary float complex
TR opt: float-complex-div2.rkt 100:12 1.0+1.0i -- unboxed literal
TR opt: float-complex-div2.rkt 100:3 1.0+1.0i -- unboxed literal
TR opt: float-complex-div2.rkt 101:0 (/ 1.0+1.0i 1.0+1.0i 1.0+1.0i) -- unboxed binary float complex
TR opt: float-complex-div2.rkt 101:12 1.0+1.0i -- unboxed literal
TR opt: float-complex-div2.rkt 101:21 1.0+1.0i -- unboxed literal
TR opt: float-complex-div2.rkt 101:3 1.0+1.0i -- unboxed literal
TR opt: float-complex-div2.rkt 104:0 (/ 1.0 1.0+1.0i) -- unboxed binary float complex
TR opt: float-complex-div2.rkt 104:7 1.0+1.0i -- unboxed literal
TR opt: float-complex-div2.rkt 105:0 (/ 1.0+1.0i 1.0) -- unboxed binary float complex
TR opt: float-complex-div2.rkt 105:3 1.0+1.0i -- unboxed literal
TR opt: float-complex-div2.rkt 106:0 (/ 1.0 1.0+1.0i 1.0+1.0i) -- unboxed binary float complex
TR opt: float-complex-div2.rkt 106:16 1.0+1.0i -- unboxed literal
TR opt: float-complex-div2.rkt 106:7 1.0+1.0i -- unboxed literal
TR opt: float-complex-div2.rkt 107:0 (/ 1.0+1.0i 1.0 1.0+1.0i) -- unboxed binary float complex
TR opt: float-complex-div2.rkt 107:16 1.0+1.0i -- unboxed literal
TR opt: float-complex-div2.rkt 107:3 1.0+1.0i -- unboxed literal
TR opt: float-complex-div2.rkt 108:0 (/ 1.0+1.0i 1.0+1.0i 1.0) -- unboxed binary float complex
TR opt: float-complex-div2.rkt 108:12 1.0+1.0i -- unboxed literal
TR opt: float-complex-div2.rkt 108:3 1.0+1.0i -- unboxed literal
TR opt: float-complex-div2.rkt 109:0 (/ 1.0 1.0 1.0+1.0i) -- unboxed binary float complex
TR opt: float-complex-div2.rkt 109:11 1.0+1.0i -- unboxed literal
TR opt: float-complex-div2.rkt 110:0 (/ 1.0 1.0+1.0i 1.0) -- unboxed binary float complex
TR opt: float-complex-div2.rkt 110:7 1.0+1.0i -- unboxed literal
TR opt: float-complex-div2.rkt 111:0 (/ 1.0+1.0i 1.0 1.0) -- unboxed binary float complex
TR opt: float-complex-div2.rkt 111:3 1.0+1.0i -- unboxed literal
TR opt: float-complex-div2.rkt 114:0 (/ 1 1.0+1.0i) -- unboxed binary float complex
TR opt: float-complex-div2.rkt 114:3 1 -- non float real in complex ops
TR opt: float-complex-div2.rkt 114:5 1.0+1.0i -- unboxed literal
TR opt: float-complex-div2.rkt 115:0 (/ 1.0+1.0i 1) -- unboxed binary float complex
TR opt: float-complex-div2.rkt 115:12 1 -- non float real in complex ops
TR opt: float-complex-div2.rkt 115:3 1.0+1.0i -- unboxed literal
TR opt: float-complex-div2.rkt 116:0 (/ 1 1.0+1.0i 1.0+1.0i) -- unboxed binary float complex
TR opt: float-complex-div2.rkt 116:14 1.0+1.0i -- unboxed literal
TR opt: float-complex-div2.rkt 116:3 1 -- non float real in complex ops
TR opt: float-complex-div2.rkt 116:5 1.0+1.0i -- unboxed literal
TR opt: float-complex-div2.rkt 117:0 (/ 1.0+1.0i 1 1.0+1.0i) -- unboxed binary float complex
TR opt: float-complex-div2.rkt 117:12 1 -- non float real in complex ops
TR opt: float-complex-div2.rkt 117:14 1.0+1.0i -- unboxed literal
TR opt: float-complex-div2.rkt 117:3 1.0+1.0i -- unboxed literal
TR opt: float-complex-div2.rkt 118:0 (/ 1.0+1.0i 1.0+1.0i 1) -- unboxed binary float complex
TR opt: float-complex-div2.rkt 118:12 1.0+1.0i -- unboxed literal
TR opt: float-complex-div2.rkt 118:21 1 -- non float real in complex ops
TR opt: float-complex-div2.rkt 118:3 1.0+1.0i -- unboxed literal
TR opt: float-complex-div2.rkt 119:0 (/ 1 1 1.0+1.0i) -- unboxed binary float complex
TR opt: float-complex-div2.rkt 119:3 1 -- non float real in complex ops
TR opt: float-complex-div2.rkt 119:5 1 -- non float real in complex ops
TR opt: float-complex-div2.rkt 119:7 1.0+1.0i -- unboxed literal
TR opt: float-complex-div2.rkt 120:0 (/ 1 1.0+1.0i 1) -- unboxed binary float complex
TR opt: float-complex-div2.rkt 120:14 1 -- non float real in complex ops
TR opt: float-complex-div2.rkt 120:3 1 -- non float real in complex ops
TR opt: float-complex-div2.rkt 120:5 1.0+1.0i -- unboxed literal
TR opt: float-complex-div2.rkt 121:0 (/ 1.0+1.0i 1 1) -- unboxed binary float complex
TR opt: float-complex-div2.rkt 121:12 1 -- non float real in complex ops
TR opt: float-complex-div2.rkt 121:14 1 -- non float real in complex ops
TR opt: float-complex-div2.rkt 121:3 1.0+1.0i -- unboxed literal
TR opt: float-complex-div2.rkt 99:0 (/ 1.0+1.0i) -- unboxed unary float complex
TR opt: float-complex-div2.rkt 99:3 1.0+1.0i -- unboxed literal
TR opt: float-complex-div2.rkt 99:3 1.0+1.0i -- unboxed literal
END
#<<END
0.5-0.5i
1.0+0.0i
0.5-0.5i
0.5-0.5i
1.0+1.0i
0.0-0.5i
1.0+0.0i
1.0+0.0i
0.5-0.5i
0.5-0.5i
1.0+1.0i
0.5-0.5i
1.0+1.0i
0.0-0.5i
1.0+0.0i
1.0+0.0i
0.5-0.5i
0.5-0.5i
1.0+1.0i

END
#lang typed/racket

;;Complex Floats
(/ 1.0+1.0i)
(/ 1.0+1.0i 1.0+1.0i)
(/ 1.0+1.0i 1.0+1.0i 1.0+1.0i)

;;Floats
(/ 1.0 1.0+1.0i)
(/ 1.0+1.0i 1.0)
(/ 1.0 1.0+1.0i 1.0+1.0i)
(/ 1.0+1.0i 1.0 1.0+1.0i)
(/ 1.0+1.0i 1.0+1.0i 1.0)
(/ 1.0 1.0 1.0+1.0i)
(/ 1.0 1.0+1.0i 1.0)
(/ 1.0+1.0i 1.0 1.0)

;;Reals
(/ 1 1.0+1.0i)
(/ 1.0+1.0i 1)
(/ 1 1.0+1.0i 1.0+1.0i)
(/ 1.0+1.0i 1 1.0+1.0i)
(/ 1.0+1.0i 1.0+1.0i 1)
(/ 1 1 1.0+1.0i)
(/ 1 1.0+1.0i 1)
(/ 1.0+1.0i 1 1)



