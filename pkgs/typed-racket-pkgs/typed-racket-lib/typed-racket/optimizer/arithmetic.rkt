#lang racket/base

(provide (all-defined-out))
(require
  racket/list
  racket/syntax
  racket/match
  (for-syntax racket/base syntax/parse)
  (for-template racket/base racket/unsafe/ops))
;; Stx objects are side effect free, but may be expensive
;; so shouldn't be duplicated but reordering is fine
(struct n-complex (real imag) #:transparent)
(struct n-zero () #:transparent)
(struct n-real (stx) #:transparent)
(struct n-non-zero-real (stx) #:transparent)
(struct n-flonum (stx) #:transparent)

(define-match-expander n-real:
  (syntax-parser
    [(_ expr)
     #'(or (n-real expr) (n-non-zero-real expr))]))

(define (add-r v1 v2)
  (match* (v1 v2)
    [((n-zero) _) v2]
    [(_ (n-zero)) v1]
    [((n-flonum s1) (n-flonum s2))
     (n-flonum #`(unsafe-fl+ #,s1 #,s2))]
    [((n-flonum s1) (n-real: s2))
     (n-flonum #`(+ #,s1 #,s2))]
    [((n-real: s1) (n-flonum s2))
     (n-flonum #`(+ #,s1 #,s2))]
    [((n-real: s1) (n-real: s2))
     (n-real #`(+ #,s1 #,s2))]))

(define (sub-r v1 v2)
  (match* (v1 v2)
    [(_ (n-zero)) v1]
    [((n-zero) (n-flonum s2))
     (n-flonum #`(unsafe-fl* -1.0 #,s2))]
    [((n-zero) (n-real: s2))
     (n-real #`(- #,s2))]
    [((n-flonum s1) (n-flonum s2))
     (n-flonum #`(unsafe-fl- #,s1 #,s2))]
    [((n-flonum s1) (n-real: s2))
     (n-flonum #`(- #,s1 #,s2))]
    [((n-real: s1) (n-flonum s2))
     (n-flonum #`(- #,s1 #,s2))]
    [((n-real: s1) (n-real: s2))
     (n-real #`(- #,s1 #,s2))]))

(define (mult-r v1 v2)
  (match* (v1 v2)
    [((n-zero) _) (n-zero)]
    [(_ (n-zero)) (n-zero)]
    [((n-flonum s1) (n-flonum s2))
     (n-flonum #`(unsafe-fl* #,s1 #,s2))]
    [((n-flonum s1) (n-non-zero-real s2))
     (n-flonum #`(* #,s1 #,s2))]
    [((n-non-zero-real s1) (n-flonum s2))
     (n-flonum #`(* #,s1 #,s2))]
    [((n-flonum s1) (n-real s2))
     (n-real #`(* #,s1 #,s2))]
    [((n-real s1) (n-flonum s2))
     (n-real #`(* #,s1 #,s2))]
    [((n-non-zero-real s1) (n-non-zero-real s2))
     (n-non-zero-real #`(* #,s1 #,s2))]
    [((n-real: s1) (n-real: s2))
     (n-real #`(* #,s1 #,s2))]))


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

(define (save r)
  (define binding (generate-temporary))
  (match r
    [(n-zero)
     (values empty (n-zero))]
    [(n-flonum stx)
     (if (identifier? stx)
         (values empty r)
         (values (list #`[(#,binding) #,stx]) (n-flonum binding)))]
    [(n-non-zero-real stx)
     (if (identifier? stx)
         (values empty r)
         (values (list #`[(#,binding) #,stx]) (n-non-zero-real binding)))]
    [(n-real stx)
     (if (identifier? stx)
         (values empty r)
         (values (list #`[(#,binding) #,stx]) (n-real binding)))]))


(define (mult-c v1 v2)
  (match* (v1 v2)
    [((n-complex r1 i1)
      (n-complex r2 i2))
     (define-values (r1-binds r1*) (save r1))
     (define-values (i1-binds i1*) (save i1))
     (define-values (r2-binds r2*) (save r2))
     (define-values (i2-binds i2*) (save i2))
     (values
       (append r1-binds i1-binds r2-binds i2-binds)
       (n-complex
        (sub-r (mult-r r1* r2*)
               (mult-r i1* i2*))
        (add-r (mult-r i1* r2*)
               (mult-r i2* r1*))))]))



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

(define (mult-rs vs)
  (if (empty? vs)
      (n-real #'1)
      (for/fold ([acc (first vs)])
                ([v (in-list (rest vs))])
        (sub-r acc v))))


(define (mult-cs vs)
  (if (empty? vs)
      (values empty (n-complex (n-real #'1) (n-zero)))
      (for/fold ([bindings empty]
                 [acc (first vs)])
                ([v (in-list (rest vs))])
        (define-values (new-bindings new-val) (mult-c acc v))
        (values (append bindings new-bindings)
                new-val))))



