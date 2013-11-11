#lang racket/base

(require syntax/parse syntax/stx racket/dict racket/promise racket/list
         racket/syntax racket/match syntax/parse/experimental/specialize
         "../utils/utils.rkt" racket/unsafe/ops unstable/sequence
         (for-template racket/base racket/math racket/flonum racket/unsafe/ops)
         (utils tc-utils)
         (types numeric-tower subtype type-table utils)
         (optimizer utils numeric-utils logging float unboxed-tables arithmetic))

(provide float-complex-opt-expr
         float-complex-expr
         binding-names
         float-complex-arith-expr
         unboxed-float-complex-opt-expr
         float-complex-call-site-opt-expr arity-raising-opt-msg)

(define-literal-syntax-class +)
(define-literal-syntax-class -)
(define-literal-syntax-class *)
(define-literal-syntax-class /)
(define-literal-syntax-class conjugate)
(define-literal-syntax-class magnitude)
(define-literal-syntax-class make-polar)
(define-literal-syntax-class exp)

(define-literal-syntax-class make-rectangular^ (make-rectangular unsafe-make-flrectangular))
(define-literal-syntax-class real-part^ (real-part flreal-part unsafe-flreal-part))
(define-literal-syntax-class imag-part^ (imag-part flimag-part unsafe-flimag-part))
(define-syntax-class projection^
  #:attributes (op)
  (pattern :real-part^ #:attr op real-part-c)
  (pattern :imag-part^ #:attr op imag-part-c))

(define-merged-syntax-class float-complex-op (+^ -^ *^ /^ conjugate^ exp^))

(define-syntax-class/specialize number-expr (subtyped-expr -Number))
(define-syntax-class/specialize real-expr (subtyped-expr -Real))
(define-syntax-class/specialize float-expr (subtyped-expr -Flonum))
(define-syntax-class/specialize float-complex-expr (subtyped-expr -FloatComplex))

(define (binding-names)
  (generate-temporaries (list "unboxed-real-" "unboxed-imag-")))

(define arity-raising-opt-msg "Complex number arity raising.")
(define-syntax-rule (log-unboxing-opt opt-label)
  (log-opt opt-label "Complex number unboxing."))
(define-syntax-rule (log-arity-raising-opt opt-label)
  (log-opt opt-label arity-raising-opt-msg))
(define-syntax-rule (log-missed-complex-expr)
  (log-missed-optimization
    "Non float complex value in complex arithmetic"
    (string-append
      "This expression has a non-float Complex number type. "
      "The optimizer could optimize it better if it had type Float-Complex.")
    this-syntax))
(define-syntax-rule (log-missed-float-expr)
  (log-missed-optimization
    "Non float value in float arithmetic"
    (string-append
      "This expression has a non Float type and thus cannot "
      "be promoted to unboxed arithmetic.")
    this-syntax))






(define-syntax-class lifted-real
  #:attributes ([bindings 1] value)
  (pattern (~and e:float-expr)
    #:with e* (generate-temporary)
    #:with (bindings ...) #'([(e*) e.opt])
    #:attr value (flonum #'e*))
  (pattern (~and e:real-expr)
    #:do [(log-missed-float-expr)
          (log-unboxing-opt "non float in float ops")
          (define constr
            (if (possibly-contains-zero? #'e)
                real
                non-zero-real))]
    #:with e* (generate-temporary 'real)
    #:with (bindings ...) #'([(e*) e.opt])
    #:attr value (constr #'e*)))


(define-syntax-class lifted-complex
  #:attributes ([bindings 1] value)
  (pattern :unboxed-float-complex-opt-expr
    #:attr value (complex (flonum #'real-binding) (flonum #'imag-binding)))
  (pattern (~and e:float-expr)
    #:with e* (generate-temporary)
    #:with (bindings ...) #'([(e*) e.opt])
    #:attr value (real-complex (flonum #'e*)))
  (pattern (~and e:real-expr)
    #:do [(log-missed-complex-expr)
          (log-unboxing-opt "non float real in complex ops")
          (define constr
            (if (possibly-contains-zero? #'e)
                real
                non-zero-real))]
    #:with e* (generate-temporary 'real)
    #:with (bindings ...) #'([(e*) e.opt])
    #:attr value (real-complex (constr #'e*)))
  (pattern (~and e:number-expr)
    #:do [(log-missed-complex-expr)
          (log-unboxing-opt "non float complex in complex ops")
          (define constr
            (if (subtypeof? #'e -InexactComplex)
                non-zero-real
                real))]
    #:with (real-binding imag-binding) (binding-names)
    #:with e* (generate-temporary 'complex)
    #:with (bindings ...)
           #'([(e*) e.opt]
              [(real-binding) (real-part e*)]
              [(imag-binding) (imag-part e*)])
    #:attr value (complex (constr #'real-binding) (constr #'imag-binding))))

(define-syntax-class variable-arity-math-op
  #:attributes (op name)
  (pattern _:+^ #:attr op add-cs  #:attr name "addition")
  (pattern _:-^ #:attr op sub-cs  #:attr name "subtraction")
  (pattern _:*^ #:attr op mult-cs #:attr name "multiplication")
  (pattern _:/^ #:attr op div-cs  #:attr name "division"))

(define-syntax-class unary-math-op
  #:attributes (op name)
  (pattern _:conjugate^ #:attr op conjugate-c  #:attr name "conjugation")
  (pattern _:exp^ #:attr op exp-c  #:attr name "exponentiation"))

;; we can eliminate boxing that was introduced by the user
(define-syntax-class binary-real-math-op
  #:attributes (op name)
  (pattern _:make-rectangular^ #:attr op make-rectangular-c  #:attr name "make-rectangular elimination")
  (pattern _:make-polar^ #:attr op make-polar-c  #:attr name "make-polar elimination"))

(define-splicing-syntax-class static-math-op
  #:attributes (name (bindings 1) value)
  (pattern (~seq :variable-arity-math-op cs:lifted-complex ...)
    #:attr value ((attribute op) (attribute cs.value))
    #:with (bindings ...) #`(cs.bindings ... ...))
  (pattern (~seq :unary-math-op c:lifted-complex)
    #:attr value ((attribute op) (attribute c.value))
    #:with (bindings ...) #'(c.bindings ...))
  (pattern (~seq :binary-real-math-op v1:lifted-real v2:lifted-real)
    #:attr value ((attribute op) (attribute v1.value) (attribute v2.value))
    #:with (bindings ...) #'(v1.bindings ... v2.bindings ...)))


;; it's faster to take apart a complex number and use unsafe operations on
;; its parts than it is to use generic operations
;; we keep the real and imaginary parts unboxed as long as we stay within
;; complex operations
(define-syntax-class unboxed-float-complex-opt-expr
  #:commit
  #:attributes (real-binding imag-binding (bindings 1))
  (pattern (~and :float-complex-expr :unchecked-unboxed-float-complex-opt-expr)))

(define-syntax-class unchecked-unboxed-float-complex-opt-expr
  #:commit
  #:attributes (real-binding imag-binding (bindings 1))

  (pattern (#%plain-app expr:static-math-op)
    #:do [(log-unboxing-opt (string-append "unboxed float complex: " (attribute expr.name)))]
    #:with ((math-bindings ...) real-binding imag-binding) (complex->bindings (attribute expr.value))
    #:with (bindings ...) #`(expr.bindings ... math-bindings ...))

  ;; if we see a variable that's already unboxed, use the unboxed bindings
  (pattern :unboxed-var
    #:do [(log-unboxing-opt "leave var unboxed")]
    #:with (bindings ...) #'())

  ;; else, do the unboxing here
  (pattern e:opt-expr
    #:with e* (generate-temporary)
    #:with (real-binding imag-binding) (binding-names)
    #:do [(log-unboxing-opt "unbox float-complex")]
    #:with (bindings ...)
      #`(((e*) e.opt)
         ((real-binding) (unsafe-flreal-part e*))
         ((imag-binding) (unsafe-flimag-part e*)))))



(define-syntax-class float-complex-opt-expr
  #:commit
  #:attributes (opt)
  ;; Dummy pattern that can't actually match.
  ;; We just want to detect "unexpected" Complex _types_ that come up.
  ;; (not necessarily complex _values_, in fact, most of the time this
  ;; case would come up, no actual complex values will be generated,
  ;; but the type system has to play it safe, and must assume that it
  ;; could happen. ex: (sqrt Integer), if the type system can't prove
  ;; that the argument is non-negative, it must assume that complex
  ;; results can happen, even if it never does in the user's program.
  ;; This is exactly what makes complex types like this "unexpected")
  ;; We define unexpected as: the whole expression has a Complex type,
  ;; but none of its subexpressions do. Since our definition of
  ;; arithmetic expression (see the arith-expr syntax class) exclude
  ;; constructors (like make-rectangular) and coercions, this is a
  ;; reasonable definition.
  (pattern e:arith-expr
           #:when (when (and (in-complex-layer? #'e)
                             (for/and ([subexpr (in-syntax #'(e.args ...))])
                               (subtypeof? subexpr -Real)))
                    (log-missed-optimization
                     "unexpected complex type"
                     (string-append
                      "This expression has a Complex type, despite all its "
                      "arguments being reals. If you do not want or expect "
                      "complex numbers as results, you may want to restrict "
                      "the type of the arguments or use float-specific "
                      "operations (e.g. flsqrt), which may have a beneficial "
                      "impact on performance.")
                     this-syntax))
           ;; We don't actually want to match.
           #:when #f
           ;; required, otherwise syntax/parse is not happy
           #:with opt #'#f)


  (pattern (#%plain-app op:make-polar^ r theta)
    #:when (subtypeof? this-syntax -FloatComplex)
    #:with exp:unboxed-float-complex-opt-expr this-syntax
    #:do [(log-unboxing-opt "make-polar")]
    #:with opt #`(let*-values (exp.bindings ...)
                   (unsafe-make-flrectangular exp.real-binding exp.imag-binding)))

  (pattern (#%plain-app op:unboxed-fun .
             (~var call (float-complex-call-site-opt-expr #'op.unboxed-info)))
    #:do [(log-unboxing-opt "unboxed call site")
          (log-arity-raising-opt "call to fun with unboxed args")]
    #:with opt ((attribute call.opt-app) #'op))

  (pattern :float-complex-arith-opt-expr))

;; Supports not optimizing in order to support using it to check for optimizable expressions.
;; Thus side effects are hidden behind the optimizing argument and referencing the opt attribute.
(define-syntax-class (float-complex-arith-expr* optimizing)
  #:commit
  #:attributes (opt)

  ;; we can optimize taking the real of imag part of an unboxed complex
  ;; hopefully, the compiler can eliminate unused bindings for the other part if it's not used
  (pattern (#%plain-app _:projection^ _:float-complex-expr)
    #:attr opt
      (delay
        (syntax-parse this-syntax
          [(#%plain-app op:projection^ c:lifted-complex)
           (log-unboxing-opt "complex accessor elimination")
           (define/with-syntax ((math-bindings ...) real-binding imag-binding)
             (complex->bindings ((attribute op.op) (attribute c.value))))
           #`(let*-values (c.bindings ... math-bindings ...)
               real-binding)])))

  (pattern (#%plain-app _:magnitude^ _:float-complex-expr)
    #:attr opt
      (delay
        (syntax-parse this-syntax
          [(#%plain-app op:magnitude^ c:unboxed-float-complex-opt-expr)
           (log-unboxing-opt "unboxed unary float complex")
           (define/with-syntax (abs-real abs-imag small large div)
             (generate-temporaries '(abs-real abs-imag small large div)))
           #`(let*-values (c.bindings ...)
               (let*-values ([(abs-real) (unsafe-flabs c.real-binding)]
                             [(abs-imag) (unsafe-flabs c.imag-binding)]
                             [(small large)
                              (if (unsafe-fl< abs-imag abs-real)
                                  (values abs-imag abs-real)
                                  (values abs-real abs-imag))])
                 (cond
                   [(unsafe-fl= 0.0 small) large]
                   [(eq? +inf.0 large)
                    (if (eq? +nan.0 small)
                        +nan.0
                        +inf.0)]
                   [else
                     (let ([div (unsafe-fl/ small large)])
                       (unsafe-fl* large (unsafe-flsqrt (unsafe-fl+ 1.0 (unsafe-fl* div div)))))])))])))

  (pattern (#%plain-app op:float-complex-op e:expr ...)
    #:when (subtypeof? this-syntax -FloatComplex)
    #:attr opt
      (delay
        (syntax-parse this-syntax
          (exp:unboxed-float-complex-opt-expr
           #'(let*-values (exp.bindings ...)
               (unsafe-make-flrectangular exp.real-binding exp.imag-binding))))))

  (pattern (~and :unboxed-var v:float-complex-expr)
    ;; unboxed variable used in a boxed fashion, we have to box
    #:attr opt
      (delay
       (log-unboxing-opt "unboxed complex variable")
       #'(unsafe-make-flrectangular real-binding imag-binding))))


(define-syntax-class possibly-unboxed
  #:attributes ([bindings 1] [real-binding 1] [imag-binding 1] [boxed-binding 1])
  (pattern (#t arg:unboxed-float-complex-opt-expr)
    #:with (bindings ...) #'(arg.bindings ...)
    #:with (real-binding ...) #'(arg.real-binding)
    #:with (imag-binding ...) #'(arg.imag-binding)
    #:with (boxed-binding ...) #'())
  (pattern (#f arg:opt-expr)
    #:with binding-name (generate-temporary 'boxed-binding)
    #:with (bindings ...) #'(((binding-name) arg.opt))
    #:with (real-binding ...) #'()
    #:with (imag-binding ...) #'()
    #:with (boxed-binding ...) #'(binding-name)))

;; takes as argument a structure describing which arguments will be unboxed
;; We cannot log opt here because this doesn't see the full original syntax
(define-syntax-class (float-complex-call-site-opt-expr unboxed-info)
  #:commit
  #:attributes (opt-app)
  ;; call site of a function with unboxed parameters
  ;; the calling convention is: real parts of unboxed, imag parts, boxed
  (pattern (orig-args:expr ...)
    #:with (unboxed-args ...) unboxed-info
    #:attr opt-app
      (λ (op)
        (syntax-parse #'((unboxed-args orig-args) ...)
          [(e:possibly-unboxed ...)
           #`(let*-values (e.bindings ... ...)
               (#,op e.real-binding ... ...
                     e.imag-binding ... ...
                     e.boxed-binding ... ...))]))))


(define-syntax-class/specialize float-complex-arith-opt-expr (float-complex-arith-expr* #t))
(define-syntax-class/specialize float-complex-arith-expr (float-complex-arith-expr* #f))
