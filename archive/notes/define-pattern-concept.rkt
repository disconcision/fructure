#lang racket


; macro macro to define 'constant' patterns?

#;(match stx

    (define-pattern (sort-not-containing-sort)
      (/ sort _/
         (not (⋱ (/ sort _/ _)))))
           
    [(⋱ c1⋱ `(,as ...
              (⋱ c2⋱
                 (capture-when (sort-not-containing-sort))
                 `(,bs ... ,(/ c/ c)))
              ,ds ... ,(/ e/ (▹ e)) ,fs ...))
     (⋱ c1⋱ `(,@as
              ,(⋱+ c2⋱ `(,@bs ,(/ c/ (▹ c))))
              ,@ds ,(/ e / e) ,@fs))]

    (define-pattern (sort-cotaining-sort-containing-cursor)
      (/ sort _/
         (⋱ _ (/ sort _/
                 (⋱ _ (/ _/ (▹ _)))))))
           
    [(⋱ c1⋱ (and (not (sort-cotaining-sort-containing-cursor))
                 (/ sort a/
                    (⋱ c2⋱ (/ b/ (▹ b))))))
     (⋱ c1⋱ (/ sort a/
               (▹ (⋱ c2⋱ (/ b/ b)))))]
         
    [x x])