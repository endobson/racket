#lang racket/base

(provide (all-defined-out))
(require
  racket/list
  racket/syntax
  racket/match
  (for-syntax racket/base syntax/parse unstable/syntax racket/set)
  (for-template racket/base racket/unsafe/ops))
;; Stx objects are side effect free, but may be expensive
;; so shouldn't be duplicated but reordering is fine
(struct c (real imag) #:transparent)
(struct zero () #:transparent)
(struct real (stx) #:transparent)
(struct non-zero-real (stx) #:transparent)
(struct flonum (stx) #:transparent)

(struct true ())
(struct false ())
(struct bool (stx))

(define 0- (zero))
(define complex c)

(define-match-expander 0:
  (syntax-parser [(_) #'(zero)]))

(define-match-expander flonum/coerce:
  (syntax-parser
    [(_ expr)
     #'(or (flonum expr)
           (non-zero-real (app (lambda (stx) #`(real->double-flonum #,stx)) expr)))]))

(define-match-expander real/flonum:
  (syntax-parser
    [(_ expr)
     #'(or (real expr) (non-zero-real expr) (flonum expr))]))

(define (unsafe-stx v)
  (match v
    [(flonum stx) stx]
    [(non-zero-real stx) #`(real->double-flonum #,stx)]))

(define (safe-stx v)
  (match v
    [(0:) #'0]
    [(real/flonum: stx) stx]))

(define (safe-bool-stx v)
  (match v
    [(true) #'#t]
    [(false) #'#f]
    [(bool stx) stx]))

(begin-for-syntax

  ;; Possible numbers
  (define-syntax-class number-type
    (pattern (~datum zero)
      #:with pattern #'(0:))
    (pattern (~datum flonum)
      #:with pattern #'(flonum _))
    (pattern (~datum real-not-zero)
      #:with pattern #'(non-zero-real _))
    (pattern (~datum real)
      #:with pattern #'(real _)))

  (define-syntax-class op-arg
    (pattern (t:number-type ...+)
      #:with pattern #'(or t.pattern ...))
    (pattern ((~datum any))
      #:with pattern #'_)
    (pattern pattern:id))

  (define-splicing-syntax-class symmetry
    (pattern (~seq) #:attr sym? #f)
    (pattern #:sym #:attr sym? #t)
    (pattern #:asym #:attr sym? #f))


  (define-splicing-syntax-class constructor
    #:attributes (constr)
    (pattern (~seq) #:with constr #'real)
    (pattern (~seq #:constructor constr:id)))

  (define-splicing-syntax-class (result safe-id unsafe-id)
    #:attributes (f)
    (pattern (~seq e:expr)
      #:with f #'(lambda (v1 v2) e))
    (pattern (~seq #:unsafe)
      #:with unsafe unsafe-id
      #:with f #'(lambda (v1 v2) (flonum #`(unsafe #,(unsafe-stx v1) #,(unsafe-stx v2) ))))
    (pattern (~seq #:safe c:constructor)
      #:with safe safe-id
      #:with f #'(lambda (v1 v2) (c.constr #`(safe #,(safe-stx v1) #,(safe-stx v2))))))


  (define-syntax-class (real-op-clause safe-id unsafe-id)
    #:attributes (match-clause)
    (pattern [term1:op-arg term2:op-arg sym:symmetry (~datum =>) (~var res (result safe-id unsafe-id))]
      #:with list-pat (if (attribute sym.sym?) #'list-no-order #'list)
      #:with (a1 a2) (generate-temporaries '(arg arg))
      #:with match-clause
        #'[(and (list a1 a2) (list-pat term1.pattern term2.pattern))
           (res.f a1 a2)])))


(define-syntax define-real-op
  (syntax-parser
    [(_ name:id #:safe safe-op:id #:unsafe unsafe-op:id
        (~var clauses (real-op-clause #'safe-op #'unsafe-op)) ...)
     #'(define name (match-lambda* clauses.match-clause ...))]))




;; zero, flonum, real, real-not-zero
(define-real-op add-r
  #:safe + #:unsafe unsafe-fl+
  [(zero) x => x]
  [x (zero) => x]
  [(flonum) (flonum real-not-zero) #:sym => #:unsafe]
  [(any) (any) #:sym => #:safe #:constructor flonum])

(define-real-op sub-r
  #:safe - #:unsafe unsafe-fl-
  [(zero) x => (negate-r x)]
  [x (zero) => x]
  [(flonum) (flonum real-not-zero) #:sym => #:unsafe]
  [(any) (any) #:sym => #:safe #:constructor flonum])

(define-real-op mult-r
  #:safe * #:unsafe unsafe-fl*
  [(zero) x => 0-]
  [x (zero) => 0-]
  [(flonum) (flonum real-not-zero) #:sym => #:unsafe]
  [(real-not-zero) (real-not-zero) #:sym => #:safe #:constructor non-zero-real]
  [(any) (any) #:sym => #:safe])

(define-real-op div-r
  #:safe / #:unsafe unsafe-fl/
  [(zero) x => 0-]
  [(flonum) (flonum real-not-zero) #:sym => #:unsafe]
  [(real-not-zero) (real-not-zero) #:sym => #:safe #:constructor non-zero-real]
  [(any) (any) #:sym => #:safe])


(define (negate-r v)
  (match v
    [(zero) 0-]
    [(flonum s)
     (flonum #`(unsafe-fl* -1.0 #,s))]
    [(non-zero-real s)
     (non-zero-real #`(- #,s))]
    [(real s)
     (real #`(- #,s))]))

(define (abs-r v)
  (match v
    [(zero) (zero)]
    [(flonum s)
     (flonum #`(unsafe-flabs #,s))]
    [(non-zero-real s)
     (non-zero-real #`(abs #,s))]
    [(real s)
     (real #`(abs #,s))]))

(define (<-r v1 v2)
  (match* (v1 v2)
    [((flonum s1) (flonum s2))
     #`(unsafe-fl< #,s1 #,s2)]
    [(_ _)
     #`(< #,(safe-stx v1) #,(safe-stx v2))]))

(define (values-r . vs)
  #`(values #,@(map safe-stx vs)))

(define (zero?-r v)
  (match v
    [(0:) (true)]
    [(flonum stx) (bool #`(unsafe-fl= 0.0 #,stx))]
    [_ (bool #`(zero? #,(safe-stx v)))]))

#|
(define (if0-r c t f)
  (match c
    [(0:) t]
    [else
      (define check
        (match c
          [(flonum stx) #`(unsafe-fl= 0.0 #,stx)]
          [_ #`(zero? #,(safe-stx v))]))

      (match* (t f)
        [
|#






(define (add-c v1 v2)
  (match* (v1 v2)
    [((c r1 i1) (c r2 i2))
     (c
       (add-r r1 r2)
       (add-r i1 i2))]))

(define (sub-c v1 v2)
  (match* (v1 v2)
    [((c r1 i1) (c r2 i2))
     (c (sub-r r1 r2) (sub-r i1 i2))]))


(define (save r)
  (match r
    [(0:)
     (values empty 0-)]
    [(real/flonum: stx)
     (define/with-syntax binding (generate-temporary))
     (define constructor
       (match r
         [(flonum _) flonum]
         [(non-zero-real _) non-zero-real]
         [(real _) real]))
     (if (identifier? stx)
         (values empty r)
         (values (list #`[(binding) #,stx]) (constructor #'binding)))]))

(define (mult-c v1 v2)
  (match* (v1 v2)
    [((c r1 i1) (c r2 i2))
     (define-values (r1-binds r1*) (save r1))
     (define-values (i1-binds i1*) (save i1))
     (define-values (r2-binds r2*) (save r2))
     (define-values (i2-binds i2*) (save i2))
     (values
       (append r1-binds i1-binds r2-binds i2-binds)
       (c
        (sub-r (mult-r r1* r2*)
               (mult-r i1* i2*))
        (add-r (mult-r i1* r2*)
               (mult-r i2* r1*))))]))

;; a+bi / c+di -> syntax 
;; a,b,c,d are floats (!= exact 0)
(define (complex-complex-/ a b c d)
  ;; we have the same cases as the Racket `/' primitive (except for the non-float ones)
  (define d=0-case
    (values-r (add-r (div-r a c) (mult-r d b))
              (sub-r (div-r b c) (mult-r d a))))
  (define c=0-case
    (values-r (add-r (div-r b d) (mult-r c a))
              (sub-r (mult-r c b) (div-r a d))))

  (define general-body
    (let* ([r (div-r c d)]
           [den (add-r d (mult-r c r))]
           [i (div-r (sub-r (mult-r b r) a) den)])
      (values-r (div-r (add-r b (mult-r a r)) den) i)))
  (define general-body-swapped
    (let* ([r (div-r d c)]
           [den (add-r c (mult-r d r))]
           [i (div-r (sub-r b (mult-r a r)) den)])
      (values-r (div-r (add-r a (mult-r b r)) den) i)))

  (define general-case
    #`(if #,(<-r (abs-r c) (abs-r d))
          #,general-body-swapped
          #,general-body))

  (wrap
    #`(cond [#,(safe-bool-stx (zero?-r d)) #,d=0-case]
            [#,(safe-bool-stx (zero?-r c)) #,c=0-case]
            [else                          #,general-case])))

(define (wrap v)
  (define/with-syntax (real imag) (generate-temporaries (list 'real 'imag)))
  (values
    (list #`[(real imag) #,v])
    (c (flonum #'real) (flonum #'imag))))


(define (div-c v1 v2)
    (match* (v1 v2)
      [((c r1 (0:)) (c r2 (0:)))
       (values empty (c (div-r r1 r2) 0-))]
      [((c (or (? flonum? r1) (? non-zero-real? r1)) (0:)) (c (? flonum? r2) (? flonum? i2)))
       (complex-complex-/ r1 0- r2 i2)]
      [((c (? flonum? r1) (? flonum? i1)) (c (or (? flonum? r2) (? non-zero-real? r2)) (0:)))
       (complex-complex-/ r1 i1 r2 0-)]
      ;; Buggy on single flonum reals
      [((c (? flonum? r1) (? flonum? i1))
        (c (or (? flonum? r2) (? non-zero-real? r2))
           (or (? flonum? i2) (? non-zero-real? i2))))
       (complex-complex-/ r1 i1 r2 i2)]
      [((c (or (? flonum? r1) (? non-zero-real? r1))
           (or (? flonum? i1) (? non-zero-real? i1)))
        (c (? flonum? r2) (? flonum? i2)))
       (complex-complex-/ r1 i1 r2 i2)]))

(define (unary-div-c v)
  (div-c (c (non-zero-real #'1.0) 0-) v))


(define (sum-c vs)
  (for/fold ([acc (c 0- 0-)])
            ([v (in-list vs)])
    (add-c acc v)))

(define (sum-r vs)
  (for/fold ([acc 0-])
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
      (real #'1)
      (for/fold ([acc (first vs)])
                ([v (in-list (rest vs))])
        (sub-r acc v))))


(define (mult-cs vs)
  (if (empty? vs)
      (values empty (c (non-zero-real #'1) 0-))
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



