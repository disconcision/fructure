#lang racket
(require containment-patterns
         rackunit)

(define situation
  `((🔥 🔥
       (🔥 (4 🍆)))
    (🔥 (🔥
        (1 🍊)) 🔥 🔥)
    (2 🍐) (🔥)))

(check-equal?
 ; seamlessly extract a 🍊 from
 ; a deeply-nested situation 🔥
 (match situation
   [(⋱ `(1 ,target)) target])
 `🍊)

