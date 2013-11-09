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
(struct n-complex (real imag) #:transparent)
(struct n-zero () #:transparent)
(struct n-real (stx) #:transparent)
(struct n-non-zero-real (stx) #:transparent)
(struct n-flonum (stx) #:transparent)

(define-match-expander c:
  (syntax-parser
    [(_ real imag)
     #'(n-complex real imag)]))

(define-match-expander 0:
  (syntax-parser
    [(_)
     #'(n-zero)]))

(define-match-expander n-real:
  (syntax-parser
    [(_ expr)
     #'(or (n-real expr) (n-non-zero-real expr))]))

(define-match-expander n-flonum:
  (syntax-parser
    [(_ expr)
     #'(or (n-flonum expr)
           (n-non-zero-real (app (lambda (stx) #`(real->double-flonum #,stx)) expr)))]))

(define-match-expander n-real/flonum:
  (syntax-parser
    [(_ expr)
     #'(or (n-real expr) (n-non-zero-real expr) (n-flonum expr))]))

(define (unsafe-stx v)
  (match v
    [(n-flonum stx) stx]
    [(n-non-zero-real stx) #`(real->double-flonum #,stx)]))

(define (safe-stx v)
  (match v
    [(n-flonum stx) stx]
    [(n-non-zero-real stx) stx]
    [(n-real stx) stx]))

(begin-for-syntax

  ;; Possible numbers
  (define-syntax-class number-type
    (pattern (~datum zero)
      #:with pattern #'(n-zero))
    (pattern (~datum flonum)
      #:with pattern #'(n-flonum _))
    (pattern (~datum real-not-zero)
      #:with pattern #'(n-non-zero-real _))
    (pattern (~datum real)
      #:with pattern #'(n-real _)))

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
    (pattern (~seq) #:with constr #'n-real)
    (pattern (~seq #:constructor constr:id)))

  (define-splicing-syntax-class (result safe-id unsafe-id)
    #:attributes (f)
    (pattern (~seq e:expr)
      #:with f #'(lambda (v1 v2) e))
    (pattern (~seq #:unsafe)
      #:with unsafe unsafe-id
      #:with f #'(lambda (v1 v2) (n-flonum #`(unsafe #,(unsafe-stx v1) #,(unsafe-stx v2) ))))
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
  [(any) (any) #:sym => #:safe #:constructor n-flonum])

(define-real-op sub-r
  #:safe - #:unsafe unsafe-fl-
  [(zero) x => (negate-r x)]
  [x (zero) => x]
  [(flonum) (flonum real-not-zero) #:sym => #:unsafe]
  [(any) (any) #:sym => #:safe #:constructor n-flonum])

(define-real-op mult-r
  #:safe * #:unsafe unsafe-fl*
  [(zero) x => (n-zero)]
  [x (zero) => (n-zero)]
  [(flonum) (flonum real-not-zero) #:sym => #:unsafe]
  [(real-not-zero) (real-not-zero) #:sym => #:safe #:constructor n-non-zero-real]
  [(any) (any) #:sym => #:safe])

(define-real-op div-r
  #:safe / #:unsafe unsafe-fl/
  [(zero) x => (n-zero)]
  [(flonum) (flonum real-not-zero) #:sym => #:unsafe]
  [(real-not-zero) (real-not-zero) #:sym => #:safe #:constructor n-non-zero-real]
  [(any) (any) #:sym => #:safe])


(define (negate-r v)
  (match v
    [(0:) (n-zero)]
    [(n-flonum s)
     (n-flonum #`(unsafe-fl* -1.0 #,s))]
    [(n-non-zero-real s)
     (n-non-zero-real #`(- #,s))]
    [(n-real s)
     (n-real #`(- #,s))]))



(define (add-c v1 v2)
  (match* (v1 v2)
    [((c: r1 i1) (c: r2 i2))
     (n-complex
       (add-r r1 r2)
       (add-r i1 i2))]))

(define (sub-c v1 v2)
  (match* (v1 v2)
    [((c: r1 i1)
      (c: r2 i2))
     (n-complex
       (sub-r r1 r2)
       (sub-r i1 i2))]))


(define (save r)
  (define binding (generate-temporary))
  (define constructor
    (match r
      [(n-zero) n-zero]
      [(n-flonum _) n-flonum]
      [(n-non-zero-real _) n-non-zero-real]
      [(n-real _) n-real]))

  (match r
    [(n-zero)
     (values empty (n-zero))]
    [(n-real/flonum: stx)
     (if (identifier? stx)
         (values empty r)
         (values (list #`[(#,binding) #,stx]) (constructor binding)))]))

(define (mult-c v1 v2)
  (match* (v1 v2)
    [((c: r1 i1) (c: r2 i2))
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

;; a+bi / c+di -> syntax
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
      [((c: r1 (0:)) (c: r2 (0:)))
       (values empty (n-complex (div-r r1 r2) (n-zero)))]
      [((c: (n-flonum: r1) (0:)) (c: (n-flonum r2) (n-flonum i2)))
       (wrap (float-complex-/ r1 r2 i2))]
      [((c: (n-flonum r1) (n-flonum i1)) (c: (n-flonum: r2) (0:)))
       (wrap (complex-float-/ r1 i1 r2))]
      [((c: (n-flonum r1) (n-flonum i1)) (c: (n-flonum r2) (n-flonum i2)))
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



