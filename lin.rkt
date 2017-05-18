#lang racket
(require turnstile
         (for-syntax syntax/parse
                     ))


(define-base-type Int)
(define-base-type Bool)
(define-type-constructor × #:arity = 2)


(begin-for-syntax
  (require syntax/parse)

  (define-syntax-class typed
    (pattern e
             #:with type (syntax-property #'e ':)))



  (define (expand+cont e [cb (lambda (x) x)])
    (local-expand (syntax-property e 'continue cb)
                  'expression
                  '()))

  (define (call-cont src-stx #:expa e- #:type [t #f])
    (define cb (or (syntax-property src-stx 'continue)
                   (lambda (x) x)))
    (cb (if t
            (syntax-property e- ': ((current-type-eval) t))
            e-)))

  (define (expands+cont exprs cb)
    (syntax-parse exprs
      [() (cb '())]
      [(e . es)
       (expand+cont #'e
                    (lambda (e-)
                      (expands+cont #'es
                                    (lambda (es-)
                                      (cb (cons e- es-))))))]))

  )


(define-syntax datum
  (syntax-parser
    [(_ . k:integer)
     (call-cont #:expa #''k
                #:type #'Int
                this-syntax)]

    [(_ . k:boolean)
     (call-cont #:expa #''k
                #:type #'Bool
                this-syntax)]))


(define-syntax tup
  (syntax-parser
    [(_ e1 e2)
     #:with s this-syntax
     (expands+cont #'(e1 e2)
                   (syntax-parser
                     [(e1-:typed e2-:typed)
                      (call-cont #:expa #'(#%app list e1- e2-)
                                 #:type #'(× e1-.type e2-.type)
                                 #'s)]))]))

(define-syntax top-interact
  (syntax-parser
    [(_ . e)
     (expand+cont #'e
                  (syntax-parser
                    [e-:typed
                     #:with t2s (type->str #'e-.type)
                     #'(#%app printf "~a : ~a\n"
                              e-
                              't2s)]))]))

(define-syntax mod-begin
  (syntax-parser
    [(_ . es)
     #:with s this-syntax
     #:with (_ _ e-:typed ...)
     (expands+cont #'es
                   (syntax-parser
                     [(e- ...)
                      #'(lambda () e- ...)]))
     #:with (t2s ...) (map type->str
                           (syntax-e #'(e-.type ...)))
     #'(#%module-begin
        (printf "~a : ~a\n"
                e- 't2s) ...)]))



(provide (type-out Int)
         (rename-out [datum #%datum]
                     [top-interact #%top-interaction]
                     [mod-begin #%module-begin]
                     [tup tup]))
