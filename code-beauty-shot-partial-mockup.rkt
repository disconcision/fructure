#lang racket

(provide mode:navigate)

#;(define (mode:navigate key state)
    ; navigation major mode
  
    (define-from state
      stx mode transforms messages)
    (define update
      (curry hash-set* state))
  
    (match key

      ["up"
       (update 'stx
        (match stx
          ; move ▹ up to the closest containing handle
          [(⋱ c⋱ (and (/ handle as/
                         (⋱ d⋱ (/ bs/ (▹ b))))
                      (not (/ handle _/
                              (⋱ (/ handle _/
                                    (⋱ (/ _/ (▹ _)))))))))
           (⋱ c⋱ (/ handle as/
                    (▹ (⋱ d⋱ (/ bs/ b)))))]
          [x x]))]

      ["down"
       (update 'stx
        (match stx
          ; move ▹ down to the closest contained handle it
          [(⋱ c⋱ (/ a/
                    (▹ (⋱ d⋱ (/ handle bs/ b)))))
           (⋱ c⋱ (/ a/
                    (⋱ d⋱ (/ handle bs/ (▹ b)))))]
          [x x]))]
    
      ["right"
       (update 'stx
        (match stx
          ; move ▹ right in a preorder traversal of handles
          [; if there's a handle under ▹
           (⋱ c⋱ (/ xs/
                    (▹ (⋱ d⋱ (/ handle as/ a)))))
           ; ▹ it
           (⋱ c⋱ (/ xs/
                    (⋱ d⋱ (/ handle as/ (▹ a)))))]
          ; otherwise, imagine the flattened list combining
          ; (▹ subtree) with its 'sibling' handled subtrees
          [(⋱+ c⋱ (capture-when
                   (or (/ _/ (▹ _))
                       (/ handle _/
                          (not (⋱ (/ _/ (▹ _)))))))
               `(,as ... ,(/ bs/ (▹ b)) ,(/ cs/ c) ,ds ...))
           ; advance ▹ rightwads therein
           (⋱+ c⋱
               `(,@as ,(/ bs/ b) ,(/ cs/ (▹ c)) ,@ds))]
          [x x]))]
    
      ["left"
       ; moves the cursor left in a preorder traversal
       ; 1. if there is a left-sibling to the cursor which contains-or-is a handle,
       ;    select its rightmost handle not containing another handle
       ; 2. otherwise, find the most immediate containing handle;
       ;    that is, a containing handle not containing a handle containing ▹
       (define new-stx    
         (f/match stx
           [(⋱c1 ⋱ `(,as ...
                     ,(⋱c2 ⋱ (capture-when (handle _ ... / (not (_ ⋱ (handle _ ... / _)))))
                           `(,bs ... ,(cs ... / c)))
                     ,ds ... ,(▹ es ... / e) ,fs ...))
            (⋱c1 ⋱ `(,@as ,(⋱c2 ⋱... `(,@bs ,(▹ cs ... / c)))
                          ,@ds ,(es ... / e) ,@fs))]
           [(⋱c1 ⋱ (and (handle as ... / (⋱c2 ⋱ (▹ bs ... / b)))
                        (not (handle _ ... / (_ ⋱ (handle _ ... / (_ ⋱ (▹ _ ... / _))))))))
            (⋱c1 ⋱ (▹ handle as ... / (⋱c2 ⋱ (bs ... / b))))]
         
           [x x]))
       (update 'stx new-stx)]

      ["\r"
       ; ENTER: insert a menu and switch to transform mode
       ; todo: if possible, factor out insert-menu-at for encapsulation
       (update
        'mode 'menu
        'stx (match stx 
               [(⋱x c⋱ (/ as/
                          (▹ a)))
                (⋱x c⋱ (/ [transform
                           (insert-menu-at-cursor (/ as/ (▹ a)) stx)]
                          as/ a))]))]

      ["\t"
       (println "metavar case")
       (update
        'stx (match stx
               [(⋱+x c⋱ #;(capture-when (or (/ _ (▹ _)) (/ [metavar _] _ _)))
                     (and ls (or (/ _ (▹ (not (⋱x (/ [metavar _] _ _))))) (/ [metavar _] _ _))))
                (println ls)
                (⋱+x c⋱
                     (map (λ (t m) (match t [(/ x/ x)
                                             (println `(matched-painting ,(/ [metavar m] x/ x)))
                                             (/ [metavar m] x/ x)]))
                          ls (range 0 (length ls))))]))]
      ["escape"
       (define (erase-metavars fr)
         (match fr
           [(/ metavar a/ a)
            (/ a/ (erase-metavars a))]
           [(? list? a)
            (map erase-metavars a)]
           [_ fr]))
       (update 'stx (erase-metavars stx))]
        
      #;[","
         ; COMMA: undo (BUG: currently completely broken)
         ; at minimum, would need to include do-seq and initial-state
         (match transforms
           ['() (update 'messages
                        `("no undo states" ,messages))]
           [_ (update 'messages `("reverting to previous state" ,@messages)
                      'stx (do-seq (hash-ref initial-state 'stx)
                                   (reverse (rest transforms)))
                      'transforms (rest transforms))])]
    
      [_
       ; fallthrough: legacy transformation mode
       (println "warning: legacy fallthrough binding")
       (mode:legacy key state) ]))