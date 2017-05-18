#lang racket
(require turnstile
         (for-syntax syntax/parse
                     ))


(define-base-type Int)
(define-base-type Bool)
(define-type-constructor × #:arity = 2)
(define-type-constructor Lin #:arity = 1)

(define-type-constructor L* #:arity = 2)
(define-type-constructor L& #:arity = 2)
(define-type-constructor LVar  #:arity = 1)
(define-type-constructor LVarI #:arity = 1)
(define-base-type L1)
(define-base-type L0)

(begin-for-syntax
  (require syntax/parse)


  #|

  . ⊢ (a + ~b) * (b + ~a)    => OK

  |#


  (define l-eval (current-type-eval))

  (define (verify l ctx)
    (printf "verify: ~a ⊢ ~a\n" (map type->str ctx) (type->str l))
    (syntax-parse l
      ; * (then)
      [(~L* ~L1 b)
       (verify #'b ctx)]
      [(~L* x b) #:when (or (LVar? #'x)
                            (LVarI? #'x))
       (verify #'b (cons #'x ctx))]
      [(~L* (~L& a b) c)
       (and (verify (l-eval #'(L* a c)) ctx)
            (verify (l-eval #'(L* b c)) ctx))]
      [(~L* (~L* a b) c)
       (verify (l-eval #'(L* a (L* b c))) ctx)]

      ; * (and)
      [(~L& a b)
       (and (verify #'a ctx)
            (verify #'b ctx))]

      ; var
      [(~or (~LVar _) (~LVarI _))
       (verify (l-eval #'L1) (cons l ctx))]

      ; unit
      [~L1 (balanced? ctx)]
      [~L0 #f]

      [_ (error (format "bad form: ~a"
                        (type->str l)))]))

  (define (balanced? ctx)
    (cond
      [(null? ctx) #t]
      [else
       (let ([p (lambda (l)
                  (syntax-parse (list (car ctx) l)
                    [(~or ((~LVar x) (~LVarI y))
                          ((~LVarI x) (~LVar y)))
                     (printf "~a vs ~a\n" #'x #'y)
                     (free-identifier=? #'x #'y)]
                    [_ #f]))])
         (and (ormap p (cdr ctx))
              (balanced? (remf p ctx))))]))

  )


(define-typed-syntax ty/datum
  [(_ . k:integer) ≫
   --------
   [⊢ 'k (⇒ : Int) (⇒ lin L1)]]
  [(_ . k:boolean) ≫
   --------
   [⊢ 'k (⇒ : Bool) (⇒ lin L1)]]
  )


(define-typed-syntax ty/let
  [(_ (x:id rhs) e) ≫
   [⊢ rhs ≫ rhs- (⇒ : τ) (⇒ lin l1)]
   #:with x* (mk-type #'x)
   [[x ≫ x-
         : τ
         lin (LVar x*)]
    ⊢ e ≫ e- (⇒ : σ) (⇒ lin l2)]
   #:with x-* (mk-type #'x-)
   --------
   [⊢ (let- ([x- rhs-]) e-)
      (⇒ : σ)
      (⇒ lin (L* (L* l1 l2)
                 (LVarI x-*)))]])


(define-typed-syntax (top-interact . e) ≫
  [⊢ e ≫ e-
         (⇒ : τ)
         (⇒ lin l)]
  #:with t->s (type->str #'τ)
  #:with l->s (type->str #'l)
  --------
  [≻ (#%app- printf "~a : ~a\nlinear: ~a => ~a\n"
             e-
             't->s
             'l->s
             '#,(if (verify #'l '())
                    "ok"
                    "FAILED"))])


(define-syntax mod-begin
  (syntax-parser
    [_ #'(#%module-begin)]))


(provide (type-out Int Bool ×)
         (rename-out [ty/datum #%datum]
                     [ty/let let]
                     #;[ty/tup tup]
                     [top-interact #%top-interaction]
                     [mod-begin #%module-begin]))
