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

(define-match-expander n-flonum:
  (syntax-parser
    [(_ expr)
     #'(or (n-flonum expr)
           (n-non-zero-real (app (lambda (stx) #`(real->double-flonum #,stx)) expr)))]))



;; TODO figure out if pre-conversion in flonum+real cases is ok

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

(define (div-r v1 v2)
  (match* (v1 v2)
    [((n-zero) _) (n-zero)]
    [((n-flonum s1) (n-flonum s2))
     (n-flonum #`(unsafe-fl/ #,s1 #,s2))]
    [((n-flonum s1) (n-non-zero-real s2))
     (n-flonum #`(/ #,s1 #,s2))]
    [((n-non-zero-real s1) (n-flonum s2))
     (n-flonum #`(/ #,s1 #,s2))]
    [((n-non-zero-real s1) (n-non-zero-real s2))
     (n-non-zero-real #`(/ #,s1 #,s2))]
    [((n-real s1) (n-flonum s2))
     (n-real #`(/ #,s1 #,s2))]
    [((n-real s1) (n-non-zero-real s2))
     (n-real #`(/ #,s1 #,s2))]))

(define (negate-r v)
  (sub-r (n-zero) v))



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


;; a+bi / c+di, names for real and imag parts of result -> one let-values binding clause
(define (complex-complex-/ a b c d)
  ;; we have the same cases as the Racket `/' primitive (except for the non-float ones)
  (define d=0-case
    #`(values (unsafe-fl+ (unsafe-fl/ #,a #,c)
                          (unsafe-fl* #,d #,b))
              (unsafe-fl- (unsafe-fl/ #,b #,c)
                          (unsafe-fl* #,d #,a))))
  (define c=0-case
    #`(values (unsafe-fl+ (unsafe-fl/ #,b #,d)
                          (unsafe-fl* #,c #,a))
              (unsafe-fl- (unsafe-fl* #,c #,b)
                          (unsafe-fl/ #,a #,d))))

  (define general-case
    #`(let* ([cm    (unsafe-flabs #,c)]
             [dm    (unsafe-flabs #,d)]
             [swap? (unsafe-fl< cm dm)]
             [a     (if swap? #,b #,a)]
             [b     (if swap? #,a #,b)]
             [c     (if swap? #,d #,c)]
             [d     (if swap? #,c #,d)]
             [r     (unsafe-fl/ c d)]
             [den   (unsafe-fl+ d (unsafe-fl* c r))]
             [i     (if swap?
                        (unsafe-fl/ (unsafe-fl- a (unsafe-fl* b r)) den)
                        (unsafe-fl/ (unsafe-fl- (unsafe-fl* b r) a) den))])
        (values (unsafe-fl/ (unsafe-fl+ b (unsafe-fl* a r)) den)
                i)))
  #`(cond [(unsafe-fl= #,d 0.0) #,d=0-case]
          [(unsafe-fl= #,c 0.0) #,c=0-case]
          [else                 #,general-case]))

;; a+bi / c+di, names for real and imag parts of result -> one let-values binding clause
;; b = exact 0
;; a,c,d are floats (!= exact 0)
(define (float-complex-/ a c d)
  ;; TODO: In what cases is the negation in the d=0 case useful
  (define d=0-case
    #`(values (unsafe-fl/ #,a #,c)
              (unsafe-fl* -1.0 (unsafe-fl* #,d #,a))))
  (define c=0-case
    #`(values (unsafe-fl* #,c #,a)
              (unsafe-fl* -1.0 (unsafe-fl/ #,a #,d))))


  (define general-case
    #`(let* ([cm    (unsafe-flabs #,c)]
             [dm    (unsafe-flabs #,d)]
             [swap? (unsafe-fl< cm dm)]
             [a     #,a]
             [c     (if swap? #,d #,c)]
             [d     (if swap? #,c #,d)]
             [r     (unsafe-fl/ c d)]
             [den   (unsafe-fl+ d (unsafe-fl* c r))]
             [i     (if swap?
                        (unsafe-fl/ (unsafe-fl* -1.0 (unsafe-fl* a r)) den)
                        (unsafe-fl/ (unsafe-fl* -1.0 a) den))]
             [j     (if swap? a (unsafe-fl* a r))])
          (values (unsafe-fl/ j den) i)))
  #`(cond [(unsafe-fl= #,d 0.0) #,d=0-case]
          [(unsafe-fl= #,c 0.0) #,c=0-case]
          [else                 #,general-case]))

;; a+bi / c+di, names for real and imag parts of result -> one let-values binding clause
;; d = exact 0
;; a,b,c are floats (!= exact 0)
(define (complex-float-/ a b c)
  #`(let ([a #,a]
          [b #,b]
          [c #,c])
        (values (unsafe-fl/ a c) (unsafe-fl/ b c))))


(define (div-c v1 v2)


  (define (wrap v)
    (define/with-syntax (real imag) (generate-temporaries (list 'real 'imag)))
    (values
      (list #`[(real imag) #,v])
      (n-complex (n-flonum #'real) (n-flonum #'imag))))

    (match* (v1 v2)
      [((n-complex r1 (n-zero))
        (n-complex r2 (n-zero)))
       (values empty (n-complex (div-r r1 r2) (n-zero)))]
      [((n-complex (n-flonum: r1) (n-zero))
        (n-complex (n-flonum r2) (n-flonum i2)))
       (wrap (float-complex-/ r1 r2 i2))]
      [((n-complex (n-flonum r1) (n-flonum i1))
        (n-complex (n-flonum: r2) (n-zero)))
       (wrap (complex-float-/ r1 i1 r2))]
      [((n-complex (n-flonum r1) (n-flonum i1))
        (n-complex (n-flonum r2) (n-flonum i2)))
       (wrap (complex-complex-/ r1 i1 r2 i2))]))


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
      (values empty (n-complex (n-non-zero-real #'1) (n-zero)))
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



