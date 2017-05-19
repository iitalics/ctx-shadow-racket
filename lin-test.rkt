#lang s-exp "lin.rkt"

(let (x (box 3))
  (tup x #t))

(let (x (box 3))
  (tup x x))
