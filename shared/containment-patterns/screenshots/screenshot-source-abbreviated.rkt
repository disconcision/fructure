#lang racket

; seamlessly extract a 🍊 from
; a deeply-nested 🔥 situation 

(define tangerine-nightmare
  `((🔥🔥 (4 🍆) 🔥
        (🔥 ((1 🍊) 🔥)))))

#;(check-equal? `🍊
    (match tangerine-nightmare
      [(⋱ `(1 ,boi)) boi]))

