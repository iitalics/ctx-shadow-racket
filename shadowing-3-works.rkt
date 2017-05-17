#lang racket
(require (for-syntax racket/syntax
                     syntax/parse
                     syntax/transformer
                     debug-scopes))

(define-syntax valueof
  (syntax-parser
    [(_ x:id)
     #:with v (syntax-property (local-expand #'x 'expression '())
                               '#%value)
     #:when (syntax-e #'v)
     #''v]))

; introduces x with value 'v' into scope for 'e', expanding 'e' to
; produce 'e-' with x bound as 'x-'. returns list of x- and e-
(define-for-syntax (introduce x v e)
  (define/with-syntax x~ (generate-temporary #'x))
  (syntax-parse (local-expand #`(lambda (x~)
                                  (let-syntax ([#,x (make-variable-like-transformer
                                                     (syntax-property #'x~
                                                                      '#%value
                                                                      #,v))])
                                    (#%expression #,e)))
                              'expression
                              '())
    #:literals (#%plain-lambda let-values #%expression)
    [(#%plain-lambda
      (x-)
      (let-values ()
        (let-values ()
          (#%expression e-))))
     (list #'x- #'e-)]))


; executes a list of commands
(define-syntax commands
  (syntax-parser
    #:datum-literals (VAR SET)
    [(_) #'(void)]

    [(_ (VAR x:id v:number initial) . rst)
     #:with (x- e-) (introduce #'x #'v #'(commands . rst))
     #'(let ([x- initial]) e-)]

    [(_ (SET x:id v:number) . rst)
     (syntax-property #'(setter x v (commands . rst))
                      'variable
                      (syntax-local-introduce #'x))]

    [(_ e . rst)
     #'(begin e (commands . rst))]))


(define-syntax setter
  (syntax-parser
    [(_ x v e)
     #:with x/p (syntax-local-introduce (syntax-property this-syntax 'variable))
     #:with (x- e-) (introduce #'x/p #'v #'e)
     #'(let ([x- x]) e-)]))


(commands
 (VAR x 100 'runtime)
 (displayln (valueof x))
 (SET x 50)
 (displayln (valueof x))
 )


#|
(introduce #'x _ _)    => WORKS
(introduce #'x/p _ _)  => DOESNT WORK

----
x in VAR: x⁰˙˙³ˢˡⁱ⁼²⁺ᵘˢᵉ⁼³                    0,1,2,3                  sli=2  use=3
x in valueof: x⁰˙˙¹⁰⁻²ˢˡⁱ⁼¹⁰⁺ᵘˢᵉ⁼              0,1, 3,4,5,6,7,8,9,10   sli=10
----
x in SET: x⁰˙˙¹¹⁻²⁻¹⁰ˢˡⁱ⁼¹¹⁺ᵘˢᵉ⁼
x in setter: x⁰₁³₄⁵₆⁷₈⁹₁₂ˢˡⁱ⁼¹²⁺ᵘˢᵉ⁼         0,1,  3,4,5,6,7,8,9,  12            sli=12
x/p in setter: x⁰˙˙¹¹⁻²⁻¹⁰ˢˡⁱ⁼¹²⁺ᵘˢᵉ⁼         0,1,  3,4,5,6,7,8,9,11              sli=12
x in valueof: x⁰˙˙¹⁹⁻²⁻¹⁰⁻¹¹⁻¹²ˢˡⁱ⁼¹⁹⁺ᵘˢᵉ⁼     0,1,  3,4,5,6,7,8,9,    13,...,19   sli=19


(syntax-local-introduce before attach)
x/p in setter: x⁰˙˙⁹⁻²ˢˡⁱ⁼¹²⁺ᵘˢᵉ⁼             0,1,  3,4,5,6,7,8,9           sli=12
=> DOESNT WORK

(syntax-local-introduce after detach)
x/p in setter: x⁰˙˙¹²⁻²⁻¹⁰ˢˡⁱ⁼¹²⁺ᵘˢᵉ⁼         0,1,  3,4,5,6,7,8,9,11,12     sli=12
=> DOESNT WORK

(syntax-local-introduce before & after)
x/p in setter: x⁰₁³₄⁵₆⁷₈⁹₁₂ˢˡⁱ⁼¹²⁺ᵘˢᵉ⁼       0,1,  3,4,5,6,7,8,9,  12      sli=12
=> WORKS

|#
