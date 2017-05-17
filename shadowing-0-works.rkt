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

(define-syntax commands
  (syntax-parser
    #:datum-literals (VAR SET)
    [(_) #'(void)]

    [(_ (VAR x:id v:number e) . rst)
     #:with x~ (generate-temporary)
     #'(let ([x~ e])
         (let-syntax ([x (make-variable-like-transformer
                          (syntax-property #'x~
                                           '#%value
                                           v))])
           (commands . rst)))]

    [(_ (SET x:id v:number) . rst)
     #:with x~ (generate-temporary)
     #'(let ([x~ x])
         (let-syntax ([x (make-variable-like-transformer
                          (syntax-property #'x~
                                           '#%value
                                           v))])
           (commands . rst)))]

    [(_ e . rst)
     #'(begin e (commands . rst))]))



(commands
 (VAR x 100 'runtime)
 (displayln (valueof x))
 (SET x 80)
 (displayln (valueof x))
 (SET x 60)
 (displayln (valueof x))
 (displayln x)
 )
