#lang racket
(require turnstile
         (for-syntax syntax/parse
                     ))


(define-base-type Unit)
(define-base-type Int)
(define-base-type Bool)
(define-type-constructor × #:arity = 2)
(define-type-constructor Lin #:arity = 1)
(define-type-constructor → #:arity = 2)

(define-base-type LUnit)
(define-type-constructor L× #:arity > 0)
(define-type-constructor L& #:arity > 0)
(define-type-constructor LVar #:arity = 1)
(define-base-type UP)
(define-base-type DOWN)

(begin-for-syntax
  (require syntax/parse)

  (define (put-props s . args)
    (let trav ([s s] [l args])
      (match l
        [(list* k v args-) (trav (syntax-property s k v) args-)]
        ['() s]
        [_ (error "wrong # of arguments")])))

  (define (get-prop s k)
    (syntax-property s k))




  (define (mk-lvar orig)
    (put-props ((current-type-eval) #'(LVar UP))
               '#%lin-orig (syntax-local-introduce orig)
               '#%lin-uniq (gensym)))

  (define (invol x)
    (syntax-parse x
      [(~LVar ~UP)
       (put-props ((current-type-eval) (syntax/loc x (LVar DOWN)))
                  '#%lin-orig (get-prop x '#%lin-orig)
                  '#%lin-uniq (get-prop x '#%lin-uniq))]
      [(~LVar ~DOWN)
       (put-props ((current-type-eval) (syntax/loc x (LVar UP)))
                  '#%lin-orig (get-prop x '#%lin-orig)
                  '#%lin-uniq (get-prop x '#%lin-uniq))]
      [_ x]))



  (define (check-well-formed l)
    (define (balance var ctx)
      ; assuming that UP gets added first, followed by successive DOWNs
      (syntax-parse var
        [(~LVar ~UP)
         (hash-set ctx (get-prop var '#%lin-uniq) var)]
        [(~LVar ~DOWN)
         (unless (hash-has-key? ctx (get-prop var '#%lin-uniq))
           (raise-syntax-error #f (format "linear variable ~a may be used more than once"
                                          (syntax-e (get-prop var '#%lin-orig)))
                               (get-prop var '#%lin-orig)))
         (hash-remove ctx (get-prop var '#%lin-uniq))]))

    (define (finished ctx)
      (for ([var (in-hash-values ctx)])
        (raise-syntax-error #f (format "linear variable ~a may be unused"
                                       (syntax-e (get-prop var '#%lin-orig)))
                            (get-prop var '#%lin-orig))))

    (let loop ([ctx (hash)]
               [exprs (list l)])
      (match exprs
        ['() (finished ctx)]
        [(cons e exprs-)
         (syntax-parse e
           [(~LVar _)
            (loop (balance e ctx) exprs-)]

           [(~L× l ...)
            (loop ctx (append (syntax-e #'(l ...)) exprs-))]

           [(~L& l ...)
            (for ([l (in-list (syntax-e #'(l ...)))])
              (loop ctx (cons l exprs-)))]

           [~LUnit (loop ctx exprs-)])])))


  )


(define-typed-syntax ty/datum
  [(_ . k:integer) ≫
   --------
   [⊢ 'k (⇒ : Int) (⇒ ~ LUnit)]]
  [(_ . k:boolean) ≫
   --------
   [⊢ 'k (⇒ : Bool) (⇒ ~ LUnit)]]
  )


(define-typed-syntax ty/begin
  [(_ e ... ef) ≫
   [⊢ e ≫ e- (⇒ : _) (⇒ ~ l)] ...
   [⊢ ef ≫ ef- (⇒ : σ) (⇒ ~ lf)]
   --------
   [⊢ (begin- e- ... ef-)
      (⇒ : σ)
      (⇒ ~ (L× l ... lf))]])


(define-typed-syntax ty/box
  [(_ e) ≫
   [⊢ e ≫ e- (⇒ : τ) (⇒ ~ l)]
   --------
   [⊢ (#%app- box e-) (⇒ : (Lin τ)) (⇒ ~ l)]])


(define-typed-syntax ty/let
  [(_ (x rhs) e) ≫
   [⊢ rhs ≫ rhs- (⇒ : τ) (⇒ ~ l1)]
   #:with l/x (if (Lin? #'τ)
                  (mk-lvar #'x)
                  #'LUnit)
   #:with l/x/inv (invol #'l/x)
   [[x ≫ x- : τ ~ l/x/inv] ⊢ e ≫ e- (⇒ : σ) (⇒ ~ l2)]
   --------
   [⊢ (let- ([x- rhs-]) e-)
      (⇒ : σ)
      (⇒ ~ (L× l/x l1 l2))]])


(define-typed-syntax ty/if
  [(_ c e1 e2) ≫
   [⊢ c ≫ c- (⇒ : τ) (⇒ ~ l1)]
   #:fail-unless (Bool? #'τ) "condition to 'if' expression must be Bool type"
   [⊢ e1 ≫ e1- (⇒ : σ1) (⇒ ~ l2)]
   [⊢ e2 ≫ e2- (⇒ : σ2) (⇒ ~ l3)]
   #:fail-unless (type=? #'σ1 #'σ2)
   (format "branches in condition have conflicting types ~a and ~a"
           (type->str #'σ1)
           (type->str #'σ2))
   --------
   [⊢ (if- c- e1- e2-)
      (⇒ : σ1)
      (⇒ ~ (L× l1 (L& l2 l3)))]])


(define-typed-syntax ty/tup
  [(_ e1 e2) ≫
   [⊢ e1 ≫ e1- (⇒ : τ) (⇒ ~ l1)]
   [⊢ e2 ≫ e2- (⇒ : σ) (⇒ ~ l2)]
   --------
   [⊢ (#%app- list e1- e2-)
      (⇒ : #,(if (or (Lin? #'τ) (Lin? #'σ))
                 #'(Lin (× τ σ))
                 #'(× τ σ)))
      (⇒ ~ (L× l1 l2))]])


(define-typed-syntax ty/lambda
  [(_ (x (~datum :) t:type) e) ≫
   #:with τ #'t.norm
   #:with l/x (if (Lin? #'τ)
                  (mk-lvar #'x)
                  #'LUnit)
   #:with l/x/inv (invol #'l/x)
   [[x ≫ x- : τ ~ l/x/inv] ⊢ e ≫ e- (⇒ : σ) (⇒ ~ l)]
   --------
   [⊢ (lambda- (x-) e-)
      (⇒ : (→ τ σ))
      (⇒ ~ (L& LUnit
               (L× l/x l)))]])

(define-typed-syntax ty/lambda-once
  [(_ (x (~datum :) t:type) e) ≫
   #:with τ #'t.norm
   #:with l/x (if (Lin? #'τ)
                  (mk-lvar #'x)
                  #'LUnit)
   #:with l/x/inv (invol #'l/x)
   [[x ≫ x- : τ ~ l/x/inv] ⊢ e ≫ e- (⇒ : σ) (⇒ ~ l)]
   --------
   [⊢ (lambda- (x-) e-)
      (⇒ : (Lin (→ τ σ)))
      (⇒ ~ (L× l/x l))]])



(define-typed-syntax (top-interact . e) ≫
  [⊢ e ≫ e-
         (⇒ : τ)
         (⇒ ~ l)]
  #:do [(check-well-formed #'l)]
  #:with t->s (type->str #'τ)
  ;#:with l->s (type->str #'l)
  --------
  [≻ (#%app- printf "~a : ~a\n"
             e-
             't->s)])



(define-syntax mod-begin
  (syntax-parser
    [_ #'(#%module-begin)]))


(provide (type-out Int Bool Unit Lin × →)
         (rename-out [ty/datum #%datum]
                     [ty/box box]
                     [ty/let let]
                     [ty/if if]
                     [ty/begin begin]
                     [ty/tup tup]
                     [ty/lambda lambda]
                     [ty/lambda λ]
                     [ty/lambda-once lambda-once]
                     [ty/lambda-once λ-once]
                     [top-interact #%top-interaction]
                     [mod-begin #%module-begin]))
