#lang racket
(require (for-syntax racket/syntax
                     syntax/parse
                     syntax/transformer))

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
     (syntax-property #'(setter v (commands . rst))
                      'variable
                      #'x)]

    [(_ e . rst)
     #'(begin e (commands . rst))]))


(define-syntax setter
  (syntax-parser
    [(_ v e)
     #:with x (syntax-property this-syntax 'variable)
     #:with (x- e-) (introduce #'x #'v #'e)
     #'(let ([x- x]) e-)]))



(commands
 (VAR x 100 'runtime)
 (displayln (valueof x))
 (SET x 80)
 (displayln (valueof x))
 (SET x 60)
 (displayln (valueof x))
 (displayln x)
 )
