#lang racket/base

(provide (all-defined-out) (all-from-out "real-arithmetic.rkt"))
(require
  "real-arithmetic.rkt"
  racket/list
  racket/syntax
  racket/match
  (for-syntax racket/base syntax/parse unstable/syntax racket/set)
  (for-template racket/base racket/unsafe/ops))
;; Stx objects are side effect free, but may be expensive
;; so shouldn't be duplicated but reordering is fine
(struct c (bindings real imag) #:transparent)

(define complex c)


(define (if-c cond t f)
  (match* (t f)
    [((c t-binds t-r t-i) (c f-binds f-r f-i))
     (match (if-r/values cond
                         (values-r t-binds t-r t-i)
                         (values-r f-binds f-r f-i))
       [(values-r: binds (list v-r v-i))
        (c binds v-r v-i)])]))




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



(define (mult-c v1 v2)
  (match* (v1 v2)
    [((c binds1 r1 i1) (c binds2 r2 i2))
     (define-values (r1-binds r1*) (save r1))
     (define-values (i1-binds i1*) (save i1))
     (define-values (r2-binds r2*) (save r2))
     (define-values (i2-binds i2*) (save i2))
     (values
       (append r1-binds i1-binds r2-binds i2-binds)
       (c (append binds1 binds2)
        (sub-r (mult-r r1* r2*)
               (mult-r i1* i2*))
        (add-r (mult-r i1* r2*)
               (mult-r i2* r1*))))]))

;; a+bi / c+di -> syntax 
;; a,b,c,d are floats (!= exact 0)
(define (complex-complex-/ binds a b c d)
  ;; we have the same cases as the Racket `/' primitive (except for the non-float ones)
  (define d=0-case
    (complex binds
             (add-r (div-r a c) (mult-r d b))
             (sub-r (div-r b c) (mult-r d a))))
  (define c=0-case
    (complex binds
             (add-r (div-r b d) (mult-r c a))
             (sub-r (mult-r c b) (div-r a d))))

  (define general-body
    (let* ([r (div-r c d)]
           [den (add-r d (mult-r c r))]
           [i (div-r (sub-r (mult-r b r) a) den)])
      (complex binds (div-r (add-r b (mult-r a r)) den) i)))
  (define general-body-swapped
    (let* ([r (div-r d c)]
           [den (add-r c (mult-r d r))]
           [i (div-r (sub-r b (mult-r a r)) den)])
      (complex binds (div-r (add-r a (mult-r b r)) den) i)))

  (define general-case
    (if-c (<-r (abs-r c) (abs-r d))
          general-body-swapped
          general-body))
  (values
    empty
    (if-c (zero?-r d) d=0-case
          (if-c (zero?-r c) c=0-case
                general-case))))


(define (div-c v1 v2)
    (match* (v1 v2)
      [((c binds1 r1 (0:)) (c binds2 r2 (0:)))
       (complex-complex-/ (append binds1 binds2) r1 0- r2 0-) ]
      [((c binds1 (or (? flonum? r1) (? non-zero-real? r1)) (0:))
        (c binds2 (? flonum? r2) (? flonum? i2)))
       (complex-complex-/ (append binds1 binds2) r1 0- r2 i2)]
      [((c binds1 (? flonum? r1) (? flonum? i1))
        (c binds2 (or (? flonum? r2) (? non-zero-real? r2)) (0:)))
       (complex-complex-/ (append binds1 binds2) r1 i1 r2 0-)]
      [((c binds1 (? flonum? r1) (? flonum? i1))
        (c binds2
           (or (? flonum? r2) (? non-zero-real? r2))
           (or (? flonum? i2) (? non-zero-real? i2))))
       (complex-complex-/ (append binds1 binds2) r1 i1 r2 i2)]
      [((c binds1
           (or (? flonum? r1) (? non-zero-real? r1))
           (or (? flonum? i1) (? non-zero-real? i1)))
        (c binds2
           (? flonum? r2) (? flonum? i2)))
       (complex-complex-/ (append binds1 binds2) r1 i1 r2 i2)]))

(define (unary-div-c v)
  (div-c (c empty (non-zero-real #'1.0) 0-) v))


(define (sum-c vs)
  (for/fold ([acc (c empty 0- 0-)])
            ([v (in-list vs)])
    (add-c acc v)))

(define (sub-cs v vs)
  (for/fold ([acc v])
            ([v (in-list vs)])
    (sub-c acc v)))

(define (mult-cs vs)
  (if (empty? vs)
      (values empty (c empty (non-zero-real #'1) 0-))
      (for/fold ([bindings empty]
                 [acc (first vs)])
                ([v (in-list (rest vs))])
        (define-values (new-bindings new-val) (mult-c acc v))
        (values (append bindings new-bindings)
                new-val))))

(define (div-cs v vs)
  (for/fold ([bindings empty]
             [acc v])
            ([v (in-list vs)])
    (define-values (new-bindings new-val) (div-c acc v))
    (values (append bindings new-bindings)
            new-val)))

