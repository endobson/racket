#lang racket/base

(provide (all-defined-out) (all-from-out "real-arithmetic.rkt"))
(require
  "real-arithmetic.rkt"
  racket/list
  racket/syntax
  racket/match
  (for-syntax racket/base racket/syntax syntax/parse unstable/syntax racket/set)
  (for-template racket/base racket/unsafe/ops))
;; Stx objects are side effect free, but may be expensive
;; so shouldn't be duplicated but reordering is fine
(struct c (bindings real imag) #:transparent)

(define complex c)
(define (c* real imag) (c empty real imag))


(define (if-c cond t f)
  (match* (t f)
    [((c t-binds t-r t-i) (c f-binds f-r f-i))
     (match (if-r/values cond
                         (values-r t-binds t-r t-i)
                         (values-r f-binds f-r f-i))
       [(values-r: binds (list v-r v-i))
        (c binds v-r v-i)])]))

(define (with-bindings bindings v)
  (match v
    [(c binds r i)
     (c (append bindings binds) r i)]))




(define (add-c v1 v2)
  (match* (v1 v2)
    [((c binds1 r1 i1) (c binds2 r2 i2))
     (c (append binds1 binds2)
       (add-r r1 r2)
       (add-r i1 i2))]))

(define (sub-c v1 v2)
  (match* (v1 v2)
    [((c binds1 r1 i1) (c binds2 r2 i2))
     (c (append binds1 binds2)
        (sub-r r1 r2)
        (sub-r i1 i2))]))

(define-syntax let*-c
  (syntax-parser
    [(_ ([names:id bound:expr] ...) body:expr)
     (define/with-syntax (bindings ...) (generate-temporaries #'(names ...)))
     #'(let*-values ([(bindings names) (save bound)] ...)
         (with-bindings (append bindings ...)
           body))]))



(define (mult-c v1 v2)
  (match* (v1 v2)
    [((c binds1 r1 i1) (c binds2 r2 i2))
     (with-bindings (append binds2 binds2)
       (let*-c ([r1* r1] [i1* i1] [r2* r2] [i2* i2])
         (c* (sub-r (mult-r r1* r2*)
                    (mult-r i1* i2*))
             (add-r (mult-r i1* r2*)
                    (mult-r i2* r1*)))))]))

(define-syntax cond-c
  (syntax-parser
    [(_ [(~literal else) body:expr])
     #'body]
    [(_ [cond:expr body:expr] clause:expr ...)
     #'(if-c cond body (cond-c clause ...))]))

;; a+bi / c+di -> syntax 
;; a,b,c,d are floats (!= exact 0)
(define (complex-complex-/ a b c d)
  ;; we have the same cases as the Racket `/' primitive (except for the non-float ones)
  (define d=0-case
    (c* (add-r (div-r a c) (mult-r d b))
        (sub-r (div-r b c) (mult-r d a))))
  (define c=0-case
    (c* (add-r (div-r b d) (mult-r c a))
        (sub-r (mult-r c b) (div-r a d))))

  (define general-case
    (let* ([r (div-r c d)]
           [den (add-r d (mult-r c r))]
           [i (div-r (sub-r (mult-r b r) a) den)])
      (c* (div-r (add-r b (mult-r a r)) den) i)))
  (define general-case-swapped
    (let* ([r (div-r d c)]
           [den (add-r c (mult-r d r))]
           [i (div-r (sub-r b (mult-r a r)) den)])
      (c* (div-r (add-r a (mult-r b r)) den) i)))

  (cond-c
    [(zero?-r d) d=0-case]
    [(zero?-r c) c=0-case]
    [(<-r (abs-r c) (abs-r d))
     general-case-swapped]
    [else general-case]))


(define (div-c v1 v2)
  (match* (v1 v2)
    [((c binds1 r1 i1) (c binds2 r2 i2))
     (with-bindings (append binds1 binds2)
       (complex-complex-/ r1 i1 r2 i2))]))

(define (unary-div-c v)
  (div-c (c empty (non-zero-real #'1.0) 0-) v))


(define (sum-c vs)
  (for/fold ([acc (c* 0- 0-)])
            ([v (in-list vs)])
    (add-c acc v)))

(define (sub-cs v vs)
  (for/fold ([acc v])
            ([v (in-list vs)])
    (sub-c acc v)))

(define (mult-cs vs)
  (if (empty? vs)
      (c* (non-zero-real #'1) 0-)
      (for/fold ([acc (first vs)])
                ([v (in-list (rest vs))])
        (mult-c acc v))))

(define (div-cs v vs)
  (for/fold ([acc v])
            ([v (in-list vs)])
    (div-c acc v)))

