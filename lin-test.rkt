#lang s-exp "lin-no-shadow.rkt"

(let (x (box 3))
  (tup x #t))

(lambda (x : (Lin Int))
  (if #t
      x
      (begin x (box 0))))

; FAILS
#;(lambda (x : (Lin Int))
  (lambda-once (y : (Lin Int))
    (tup x y)))

(lambda (x : (Lin Int))
  (lambda-once (y : (Lin Int))
    (tup x y)))
