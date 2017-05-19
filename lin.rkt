#lang racket
(require turnstile
         (for-syntax syntax/parse
                     ))


;; types ;;
; basic types
(define-base-type Unit)
(define-base-type Int)
(define-base-type Bool)
; pair type and function type
(define-type-constructor × #:arity = 2)
(define-type-constructor → #:arity = 2)
; linear type, which surrounds any type which is linear.
; e.g. (box 2)              : (Lin Int)
;      (tup (box 2) #t)     : (Lin (× (Lin Int) Bool))
;      (λ-once (x : Int) x) : (Lin (→ Int Int))
(define-type-constructor Lin #:arity = 1)


;; linear expressions, which are collected & evaluated after inference. ;;
; every L⇑ that occurs in a linear expression must have exactly one corresponding
; L⇓ to be considered "well-formed". bound identifiers translate to L⇑ and L⇓
; expressions by syntax-properties '#%lin-uniq (for differentiating them), and
; property '#%lin-orig (for producing errors messages).
(define-base-types L⇑ L⇓)

; LUnit is well-formed as long as the rest of the expression is
(define-base-type LUnit)

; (L× A B) is well formed if the combination of A and B is well formed
; e.g. (L× {L⇑ X} {L⇓ X}) is well formed
;      (L× {L⇑ X} LUnit)  is not
(define-type-constructor L× #:arity > 0)

; (L& A B) is well formed if the compound expression could be replaced
; by either A or B
; e.g. (L& (L× {L⇑ X} {L⇓ X})
;          LUnit)             is well formed
;      (L& {L⇑ X} {L⇓ X})     is not
(define-type-constructor L& #:arity > 0)

; this is rougly based on linear logic from https://en.wikipedia.org/wiki/Linear_logic
; where L× is multiplicative disjunction (⅋)
;       L& is additive conjunction (&)
;       LUnit is the multiplicative unit (1)
;       L⇑ is the dual (superscript ⊥) of L⇓
; as an example, binding a linear variable forms a linear implication, e.g.
; (let (x (box 3)) x)   ~>   (L× {L⇑ x} {L⇓ x}) ≡ x⊥ ⅋ x ≡ x -o x  (is well formed)




(begin-for-syntax
  (require syntax/parse)

  ; (put-props s 'k1 v1 'k2 v2) =
  ; (syntax-property (syntax-property s 'k1 v1) 'k2 v2)
  (define (put-props s . args)
    (let trav ([s s] [l args])
      (match l
        [(list* k v args-) (trav (syntax-property s k v) args-)]
        ['() s]
        [_ (error "wrong # of arguments")])))

  (define (get-prop s k)
    (syntax-property s k))



  ; create a unique L⇑ with the given syntax as its origin
  (define (mk-⇑ orig)
    (put-props ((current-type-eval) (syntax/loc orig L⇑))
               '#%lin-orig orig
               '#%lin-uniq (gensym)))

  ; flip L⇑ to L⇓ and vice-versa, or don't flip if the given expression isn't an arrow
  (define (lin-flip x)
    (syntax-parse x
      [~L⇑
       (put-props ((current-type-eval) (syntax/loc x L⇓))
                  '#%lin-orig (get-prop x '#%lin-orig)
                  '#%lin-uniq (get-prop x '#%lin-uniq))]
      [~L⇓
       (put-props ((current-type-eval) (syntax/loc x L⇑))
                  '#%lin-orig (get-prop x '#%lin-orig)
                  '#%lin-uniq (get-prop x '#%lin-uniq))]
      [_ x]))



  ; checks if a linear expression (L_) is wellformed (if all
  ; paths lead to ⇑ being matched with ⇓). raises a syntax error
  ; if not well formed, returns (void) if it is.
  (define (check-well-formed l)
    ; add the variable to the context, balancing ⇑ and ⇓ occurences. note
    ; that we assume here that (for each var) a single ⇑ will be added, followed by some ⇓'s
    (define (add/balance var ctx)
      (syntax-parse var
        [~L⇑
         (hash-set ctx (get-prop var '#%lin-uniq) var)]
        [~L⇓
         (unless (hash-has-key? ctx (get-prop var '#%lin-uniq))
           (raise-syntax-error #f (format "linear variable ~a may be used more than once"
                                          (syntax-e (get-prop var '#%lin-orig)))
                               (get-prop var '#%lin-orig)))
         (hash-remove ctx (get-prop var '#%lin-uniq))]))

    (define (finished ctx)
      ; any remaining ⇑'s will cause an error due to the lack of matching ⇓
      ; i think that if this function was empty we would have an "affine" type system
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
           [~LUnit
            (loop ctx exprs-)]
           [(~L× l ...)
            (loop ctx (append (syntax-e #'(l ...)) exprs-))]
           [(~L& l ...)
            (for ([l (in-list (syntax-e #'(l ...)))])
              (loop ctx (cons l exprs-)))]
           [_
            (loop (add/balance e ctx) exprs-)])])))

  )



; these first rules are straightforward. datum do not expend any
; resource. box, begin and tup use the resources of all their sub expressions.

(define-typed-syntax ty/datum
  [(_ . k:integer) ≫
   --------
   [⊢ 'k (⇒ : Int) (⇒ ~> LUnit)]]
  [(_ . k:boolean) ≫
   --------
   [⊢ 'k (⇒ : Bool) (⇒ ~> LUnit)]])


(define-typed-syntax (ty/box e) ≫
  [⊢ e ≫ e- (⇒ : τ) (⇒ ~> l)]
  --------
  [⊢ (#%app- box e-)
     (⇒ : (Lin τ))
     (⇒ ~> l)])


(define-typed-syntax (ty/begin e ...+) ≫
  [⊢ e ≫ e- (⇒ : τ) (⇒ ~> l)] ...
  #:with σ (last (syntax-e #'(τ ...)))
  --------
  [⊢ (begin- e- ...)
     (⇒ : σ)
     (⇒ ~> (L× l ...))])


(define-typed-syntax (ty/tup e1 e2) ≫
  [⊢ e1 ≫ e1- (⇒ : τ1) (⇒ ~> l1)]
  [⊢ e2 ≫ e2- (⇒ : τ2) (⇒ ~> l2)]
  #:with σ #'(× τ1 τ2)
  --------
  [⊢ (#%app- list e1- e2-)
     ; make the pair linear if any of the elements are
     (⇒ : #,(if (or (Lin? #'τ1) (Lin? #'τ2))
                #'(Lin σ)
                #'σ))
     (⇒ ~> (L× l1 l2))])


; the if rule combines the result of both branches
; with L& to ensure that they have the same effects

(define-typed-syntax (ty/if c e1 e2) ≫
  [⊢ c ≫ c- (⇒ : τ) (⇒ ~> l1)]
  #:fail-unless (Bool? #'τ) "condition to 'if' expression must be Bool type"
  [⊢ e1 ≫ e1- (⇒ : σ1) (⇒ ~> l2)]
  [⊢ e2 ≫ e2- (⇒ : σ2) (⇒ ~> l3)]
  #:fail-unless (type=? #'σ1 #'σ2)
  (format "branches in condition have conflicting types ~a and ~a"
          (type->str #'σ1)
          (type->str #'σ2))
  --------
  [⊢ (if- c- e1- e2-)
     (⇒ : σ1)
     (⇒ ~> (L× l1 (L& l2 l3)))])


; let possibly introduces a L⇑/L⇓ pair if the bound variable
; has a linear type

(define-typed-syntax ty/let
  [(_ (x:id rhs) e) ≫
   [⊢ rhs ≫ rhs- (⇒ : τ) (⇒ ~> l1)]
   #:with l/x↑ (if (Lin? #'τ) (mk-⇑ #'x) #'LUnit)
   #:with l/x↓ (lin-flip #'l/x↑)
   [[x ≫ x- : τ ~> l/x↓] ⊢ e ≫ e- (⇒ : σ) (⇒ ~> l2)]
   --------
   [⊢ (let- ([x- rhs-]) e-)
      (⇒ : σ)
      (⇒ ~> (L× l/x↑ l1 l2))]]

  ; TODO: sugar forms
  )


; lambda is unrestricted, so a trick with L& must be applied to ensure
; that it doesn't cause any linear effects

(define-typed-syntax (ty/lambda (x:id (~datum :) t:type) e) ≫
  #:with τ #'t.norm
  #:with l/x↑ (if (Lin? #'τ) (mk-⇑ #'x) #'LUnit)
  #:with l/x↓ (lin-flip #'l/x↑)
  [[x ≫ x- : τ ~> l/x↓] ⊢ e ≫ e- (⇒ : σ) (⇒ ~> l)]
  --------
  [⊢ (lambda- (x-) e-)
     (⇒ : (→ τ σ))
     (⇒ ~> (L& LUnit (L× l/x↑ l)))
     ; the L& expression prevents use of linear variables outside the
     ; body of the lambda
     ])


; lambda-once does not use L& and instead produces the linear type (Lin (→ τ σ))

(define-typed-syntax (ty/lambda-once (x:id (~datum :) t:type) e) ≫
  #:with τ #'t.norm
  #:with l/x↑ (if (Lin? #'τ) (mk-⇑ #'x) #'LUnit)
  #:with l/x↓ (lin-flip #'l/x↑)
  [[x ≫ x- : τ ~> l/x↓] ⊢ e ≫ e- (⇒ : σ) (⇒ ~> l)]
  --------
  [⊢ (lambda- (x-) e-)
     (⇒ : (Lin (→ τ σ)))
     (⇒ ~> (L× l/x↑ l))])


(define-typed-syntax (mod-begin e ...) ≫
  [⊢ e ≫ e- (⇒ : τ) (⇒ ~> l)] ...
  #:do [(for ([l (in-list (syntax-e #'(l ...)))])
          (check-well-formed l))]
  --------
  [≻ (#%module-begin e- ...)])


(define-typed-syntax (top-interact . e) ≫
  [⊢ e ≫ e-
         (⇒ : τ)
         (⇒ ~> l)]
  #:do [(check-well-formed #'l)]
  #:with t->s (type->str #'τ)
  ;#:with l->s (type->str #'l)
  --------
  [≻ (#%app- printf "~a : ~a\n"
             e-
             't->s)])





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
