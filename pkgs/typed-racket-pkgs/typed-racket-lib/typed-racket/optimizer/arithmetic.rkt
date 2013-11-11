#lang racket/base

(require
  "real-arithmetic.rkt"
  racket/list
  racket/syntax
  racket/match
  (for-syntax racket/base racket/syntax syntax/parse unstable/syntax racket/set)
  (for-template racket/base racket/unsafe/ops))

(provide
  add-cs
  sub-cs
  mult-cs
  div-cs
  (rename-out
    [c* complex]
    [real-c* real-complex])
  complex->bindings
  non-zero-real
  real
  flonum)

;; Stx objects are side effect free, but may be expensive
;; so shouldn't be duplicated but reordering is fine
(struct c (bindings real imag) #:transparent)

(define (c* real imag) (c empty real imag))
(define (real-c* real) (c empty real 0-))

(define 0c (real-c* 0-))
(define 1c (real-c* (non-zero-real #'1)))

;;;;;;;;;;;;;;;
;;; Helpers ;;;
;;;;;;;;;;;;;;;

(define (if-c cond t f)
  (match* (t f)
    [((c t-binds t-r t-i) (c f-binds f-r f-i))
     (match (if-r/values cond
                         (values-r t-binds t-r t-i)
                         (values-r f-binds f-r f-i))
       [(values-r: binds (list v-r v-i))
        (c binds v-r v-i)])]))

(define-syntax with-bindings
  (syntax-parser
    [(_ bindings . body)
     #'(let ((binds1 bindings))
         (match (let () . body)
           [(c binds2 r i)
            (c (append binds1 binds2) r i)]))]))

(define-syntax let*-c
  (syntax-parser
    [(_ ([names:id bound:expr] ...) . body:expr)
     (define/with-syntax (bindings ...) (generate-temporaries #'(names ...)))
     #'(let*-values ([(bindings names) (save bound 'names)] ...)
         (with-bindings (append bindings ...)
           . body))]))


(define-syntax cond-c
  (syntax-parser
    [(_ [(~literal else) body:expr])
     #'body]
    [(_ [cond:expr body:expr] clause:expr ...)
     #'(if-c cond body (cond-c clause ...))]))

(define (fold-c f base vs)
  (for/fold ([acc base])
            ([v (in-list vs)])
      (f acc v)))

(define (complex->bindings v)
  (match v
    [(c bindings r i)
     (define-values (r-binds r*) (save r 'unboxed-real-))
     (define-values (i-binds i*) (save i 'unboxed-imag-))
     (list
       (append bindings r-binds i-binds)
       (safe-stx r*)
       (safe-stx i*))]))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Binary implementations ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (add-c v1 v2)
  (match* (v1 v2)
    [((c binds1 r1 i1) (c binds2 r2 i2))
     (with-bindings (append binds1 binds2)
       (c* (add-r r1 r2)
           (add-r i1 i2)))]))

(define (sub-c v1 v2)
  (match* (v1 v2)
    [((c binds1 r1 i1) (c binds2 r2 i2))
     (with-bindings (append binds1 binds2)
     (c* (sub-r r1 r2)
         (sub-r i1 i2)))]))

(define (mult-c v1 v2)
  (match* (v1 v2)
    [((c binds1 r1* i1*) (c binds2 r2* i2*))
     (with-bindings (append binds2 binds2)
       (let*-c ([r1 r1*] [i1 i1*] [r2 r2*] [i2 i2*])
         (c* (sub-r (mult-r r1 r2)
                    (mult-r i1 i2))
             (add-r (mult-r i1 r2)
                    (mult-r i2 r1)))))]))

(define (div-c v1 v2)
  (match* (v1 v2)
    [((c binds1 r1 i1) (c binds2 r2 i2))
     (with-bindings (append binds1 binds2)
       (let*-c ([a r1] [b i1] [c r2] [d i2])
         (define d=0-case
           (c* (add-r (div-r a c) (mult-r d b))
               (sub-r (div-r b c) (mult-r d a))))
         (define c=0-case
           (c* (add-r (div-r b d) (mult-r c a))
               (sub-r (mult-r c b) (div-r a d))))

         (define general-case
           (let*-c ([r (div-r c d)]
                    [den (add-r d (mult-r c r))]
                    [i (div-r (sub-r (mult-r b r) a) den)])
             (c* (div-r (add-r b (mult-r a r)) den) i)))
         (define general-case-swapped
           (let*-c ([r (div-r d c)]
                    [den (add-r c (mult-r d r))]
                    [i (div-r (sub-r b (mult-r a r)) den)])
             (c* (div-r (add-r a (mult-r b r)) den) i)))

         (cond-c
           [(zero?-r d) d=0-case]
           [(zero?-r c) c=0-case]
           [(<-r (abs-r c) (abs-r d))
            general-case-swapped]
           [else general-case])))]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Variable Arity Implementations ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (add-cs vs)
  (fold-c add-c 0c vs))

(define (sub-cs vs)
  (cond
    [(empty? vs) (error 'sub-cs "subtraction cannot handle 0 args")]
    [(empty? (rest vs)) (sub-c 0c (first vs))]
    [else (fold-c sub-c (first vs) (rest vs))]))

(define (mult-cs vs)
  (cond
    [(empty? vs) 1c]
    [else (fold-c mult-c (first vs) (rest vs))]))

(define (div-cs vs)
  (cond
    [(empty? vs) (error 'div-cs "division cannot handle 0 args")]
    [(empty? (rest vs)) (div-c 1c (first vs))]
    [else (fold-c div-c (first vs) (rest vs))]))
