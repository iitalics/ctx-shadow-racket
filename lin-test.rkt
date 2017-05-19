#lang s-exp "lin-no-shadow.rkt"
; TODO: typechecking lib

(let (x (box 3))
  (tup x #t))

(λ (x : (Lin Int))
  (if #t
      x
      (begin x (box 0))))

; FAILS
#;(lambda (x : (Lin Int))
    (lambda (y : (Lin Int))
      (tup x y)))

; OK:
(λ (x : (Lin Int))
  (λ-once (y : (Lin Int))
    (tup x y)))


; FAILS
#;(let (x (box 3))
    (begin
      (λ (u : Unit) x)
      x))

#;(let (x (box 3))
    (begin
      (λ (u : Unit) x)))

#;(let (x (box 3))
    (begin
      (λ-once (u : Unit) x)
      x))

(let (x (box 3))
  (begin
    (λ-once (u : Unit) x)))
