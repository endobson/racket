#lang racket/base

(provide 
  0r
  1r
  add-r
  sub-r
  mult-r
  div-r
  negate-r
  exp-r
  cos-r
  sin-r
  if-r
  <-r
  abs-r
  zero?-r
  save-r

  if-r/values
  (rename-out [values-r* values-r])
  values-r:
  
  (rename-out [safe-stx real-stx])
  real
  non-zero-real
  flonum)

(require
  racket/list
  racket/syntax
  syntax/stx
  racket/match
  (for-syntax racket/base syntax/parse unstable/syntax racket/set)
  (for-template racket/base racket/unsafe/ops))
;; Stx objects are side effect free, but may be expensive
;; so shouldn't be duplicated but reordering is fine
(struct zero () #:transparent)
(struct one () #:transparent)
(struct real (stx) #:transparent)
(struct non-zero-real (stx) #:transparent)
(struct flonum (stx) #:transparent)

(struct true ())
(struct false ())
(struct bool (stx))

(define 0r (zero))
(define 1r (one))

(struct values-r (bindings vals))

(define-match-expander 0:
  (syntax-parser [(_) #'(zero)]))

(define-match-expander 1:
  (syntax-parser [(_) #'(one)]))

(define-match-expander values-r:
  (syntax-parser [(_ binds args) #'(values-r binds args)]))

(define-match-expander real/flonum:
  (syntax-parser
    [(_ expr)
     #'(or (real expr) (non-zero-real expr) (flonum expr))]))

(define-match-expander any-r:
  (syntax-parser
    [(_ expr)
     #'(or (real expr) (non-zero-real expr) (flonum expr) 
           (and (1:) (app safe-stx expr))
           (and (0:) (app safe-stx expr)))]))


(define (unsafe-stx v)
  (match v
    [(1:) #'1.0]
    [(flonum stx) stx]
    [(non-zero-real stx) #`(real->double-flonum #,stx)]))

(define (safe-stx v)
  (match v
    [(0:) #'0]
    [(1:) #'1]
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
    (pattern (~datum one)
      #:with pattern #'(1:))
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
  [(flonum) (flonum real-not-zero one) #:sym => #:unsafe]
  [(any) (any) #:sym => #:safe #:constructor flonum])

(define-real-op sub-r
  #:safe - #:unsafe unsafe-fl-
  [(zero) x => (negate-r x)]
  [x (zero) => x]
  [(flonum) (flonum real-not-zero one) #:sym => #:unsafe]
  [(any) (any) #:sym => #:safe #:constructor flonum])

(define-real-op mult-r
  #:safe * #:unsafe unsafe-fl*
  [x (zero) #:sym => 0r]
  [x (one) #:sym => x]
  [(flonum) (flonum real-not-zero one) #:sym => #:unsafe]
  [(real-not-zero) (real-not-zero one) #:sym => #:safe #:constructor non-zero-real]
  [(any) (any) #:sym => #:safe])

(define-real-op div-r
  #:safe / #:unsafe unsafe-fl/
  [(zero) x => 0r]
  [x (one) => 1r]
  [(flonum) (flonum real-not-zero one) #:sym => #:unsafe]
  [(real-not-zero) (real-not-zero one) #:sym => #:safe #:constructor non-zero-real]
  [(any) (any) #:sym => #:safe])


(define (negate-r v)
  (match v
    [(0:) 0r]
    [(1:) (non-zero-real #'-1)]
    [(flonum s)
     (flonum #`(unsafe-fl* -1.0 #,s))]
    [(non-zero-real s)
     (non-zero-real #`(- #,s))]
    [(real s)
     (real #`(- #,s))]))

(define (abs-r v)
  (match v
    [(0:) 0r]
    [(1:) 1r]
    [(flonum s)
     (flonum #`(unsafe-flabs #,s))]
    [(non-zero-real s)
     (non-zero-real #`(abs #,s))]
    [(real s)
     (real #`(abs #,s))]))

(define (exp-r v)
  (match v
    [(0:) 1r]
    [(1:) (flonum #'(exp 1))]
    [(flonum s)
     (flonum #`(unsafe-flexp #,s))]
    [(non-zero-real s)
     (non-zero-real #`(exp #,s))]
    [(real s)
     (non-zero-real #`(exp #,s))]))

(define (cos-r v)
  (match v
    [(0:) 1r]
    [(1:) (flonum #'(cos 1))]
    [(flonum s)
     (flonum #`(unsafe-flcos #,s))]
    [(non-zero-real s)
     (real #`(cos #,s))]
    [(real s)
     (real #`(cos #,s))]))

(define (sin-r v)
  (match v
    [(0:) 0r]
    [(1:) (flonum #'(sin 1))]
    [(flonum s)
     (flonum #`(unsafe-flsin #,s))]
    [(non-zero-real s)
     (real #`(sin #,s))]
    [(real s)
     (real #`(sin #,s))]))


(define (<-r v1 v2)
  (match* (v1 v2)
    [((0:) (1:))
     (true)]
    [((flonum s1) (flonum s2))
     (bool #`(unsafe-fl< #,s1 #,s2))]
    [(_ _)
     (bool #`(< #,(safe-stx v1) #,(safe-stx v2)))]))

(define (zero?-r v)
  (match v
    [(0:) (true)]
    [(1:) (false)]
    [(flonum stx) (bool #`(unsafe-fl= 0.0 #,stx))]
    [_ (bool #`(zero? #,(safe-stx v)))]))

(define (if-stx c t f)
  #`(if #,c #,t #,f))


;; Stx must represent one or both of the two values in v1 and v2
(define (merge-r v1 v2 stx)
  (match* (v1 v2)
    [((0:) (0:)) 0r]
    [((1:) (1:)) 1r]
    [((flonum t-stx) (flonum f-stx)) (flonum stx)]
    [((non-zero-real t-stx) (flonum f-stx)) (non-zero-real stx)]
    [((flonum t-stx) (non-zero-real f-stx)) (non-zero-real stx)]
    [((1:) (flonum f-stx)) (non-zero-real stx)]
    [((flonum t-stx) (1:)) (non-zero-real stx)]
    [((non-zero-real t-stx) (non-zero-real f-stx)) (non-zero-real stx)]
    [((1:) (non-zero-real f-stx)) (non-zero-real stx)]
    [((non-zero-real t-stx) (1:)) (non-zero-real stx)]
    [((any-r: t-stx) (any-r: f-stx)) (real stx)]))


(define (if-r c t f)
  (match c
    [(true) t]
    [(false) f]
    [(bool c-stx)
     (match* (t f)
       [((any-r: t-stx) (any-r: f-stx)) (merge-r t f (if-stx c-stx t-stx f-stx))])]))

(define (if-r/values c t f)
  (match c
    [(true) t]
    [(false) f]
    [(bool c-stx)
     (match* (t f)
       [((values-r t-binds t-args) (values-r f-binds f-args))
        (unless (equal? (length t-args) (length f-args))
          (error 'if-r/values "Branches have different number of values."))
        (define/with-syntax (vs ...)
          (generate-temporaries (map (lambda (x) 'merged) t-args)))
        (values-r
          (list
            #`[(vs ...)
               (if #,c-stx
                   (let*-values (#,@t-binds) (values #,@(map safe-stx t-args)))
                   (let*-values (#,@f-binds) (values #,@(map safe-stx f-args))))])
          (stx-map merge-r t-args f-args #'(vs ...)))])]))

(define (save-r r [name 'saved])
  (match r
    [(0:)
     (values empty 0r)]
    [(1:)
     (values empty 1r)]
    [(real/flonum: stx)
     (define/with-syntax binding (generate-temporary name))
     (define constructor
       (match r
         [(flonum _) flonum]
         [(non-zero-real _) non-zero-real]
         [(real _) real]))
     (if (identifier? stx)
         (values empty r)
         (values (list #`[(binding) #,stx]) (constructor #'binding)))]))

(define (values-r* bindings . args)
  (values-r bindings args))
