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
    [(real/flonum: stx) stx]))

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

;; a+bi / c+di -> syntax
;; b = exact 0
;; a,c,d are floats (!= exact 0)
(define (float-complex-/ a-r c-r d-r)
  (define a (unsafe-stx a-r))
  (define c (unsafe-stx c-r))
  (define d (unsafe-stx d-r))
  ;; TODO: In what cases is the negation in the d=0 case useful
  (define d=0-case
    #`(values #,(flonum-stx (div-r a-r c-r))
              #,(flonum-stx (negate-r (mult-r d-r a-r)))))
  (define c=0-case
    #`(values #,(flonum-stx (mult-r c-r a-r))
              #,(flonum-stx (negate-r (div-r a-r d-r)))))



  (define general-case
    #`(let* ([swap? #,(<-r (abs-r c-r) (abs-r d-r))]
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
  (wrap
    #`(cond [(unsafe-fl= #,d 0.0) #,d=0-case]
            [(unsafe-fl= #,c 0.0) #,c=0-case]
            [else                 #,general-case])))

;; a+bi / c+di -> syntax
;; d = exact 0
;; a,b,c are inexact (!= exact 0)
(define (complex-float-/ a b c)
  (define-values (c-binds c*) (save c))
  (values c-binds
          (complex (div-r a c*) (div-r b c*))))

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
       (float-complex-/ r1 r2 i2)]
      [((c (? flonum? r1) (? flonum? i1)) (c (or (? flonum? r2) (? non-zero-real? r2)) (0:)))
       (complex-float-/ r1 i1 r2)]
      ;; Buggy on single flonum reals
      [((c (flonum r1) (flonum i1)) (c (flonum/coerce: r2) (flonum/coerce: i2)))
       (wrap (complex-complex-/ r1 i1 r2 i2))]
      [((c (flonum/coerce: r1) (flonum/coerce: i1)) (c (flonum r2) (flonum i2)))
       (wrap (complex-complex-/ r1 i1 r2 i2))]))

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



