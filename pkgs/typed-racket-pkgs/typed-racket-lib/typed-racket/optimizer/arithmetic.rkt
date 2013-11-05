#lang racket/base

(provide (all-defined-out))
(require
  racket/match
  (for-template racket/base racket/unsafe/ops))
;; Stx objects are side effect free, but may be expensive
;; so shouldn't be duplicated but reordering is fine
(struct n-complex (real imag))
(struct n-zero ())
(struct n-real (stx))
(struct n-flonum (stx))

(define (add-r v1 v2)
  (match* (v1 v2)
    [((n-zero) _) v2]
    [(_ (n-zero)) v1]
    [((n-flonum s1) (n-flonum s2))
     (n-flonum #`(unsafe-fl+ #,s1 #,s2))]
    [((n-flonum s1) (n-real s2))
     (n-flonum #`(+ #,s1 #,s2))]
    [((n-real s1) (n-flonum s2))
     (n-flonum #`(+ #,s1 #,s2))]
    [((n-real s1) (n-real s2))
     (n-real #`(+ #,s1 #,s2))]))

(define (sub-r v1 v2)
  (match* (v1 v2)
    [(_ (n-zero)) v1]
    [((n-zero) (n-flonum s2))
     (n-flonum #`(unsafe-fl* -1.0 #,s2))]
    [((n-zero) (n-real s2))
     (n-real #`(- #,s2))]
    [((n-flonum s1) (n-flonum s2))
     (n-flonum #`(unsafe-fl- #,s1 #,s2))]
    [((n-flonum s1) (n-real s2))
     (n-flonum #`(- #,s1 #,s2))]
    [((n-real s1) (n-flonum s2))
     (n-flonum #`(- #,s1 #,s2))]
    [((n-real s1) (n-real s2))
     (n-real #`(- #,s1 #,s2))]))


(define (add-c v1 v2)
  (match* (v1 v2)
    [((n-complex r1 i1)
      (n-complex r2 i2))
     (n-complex
       (add-r r1 r2)
       (add-r i1 i2))]))

(define (sub-c v1 v2)
  (match* (v1 v2)
    [((n-complex r1 i1)
      (n-complex r2 i2))
     (n-complex
       (sub-r r1 r2)
       (sub-r i1 i2))]))



(define (sum-c vs)
  (for/fold ([acc (n-complex (n-zero) (n-zero))])
            ([v (in-list vs)])
    (add-c acc v)))

(define (sum-r vs)
  (for/fold ([acc (n-zero)])
            ([v (in-list vs)])
    (add-r acc v)))

(define (sub-cs v vs)
  (for/fold ([acc v])
            ([v (in-list vs)])
    (sub-c acc v)))

(define (sub-rs v vs)
  (for/fold ([acc v])
            ([v (in-list vs)])
    (sub-r acc v)))
