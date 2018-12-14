#lang racket

(provide mode:navigate)

(define (mode:navigate key state)
  ; navigation major mode
  
  (define-from state
    stx mode transforms messages)
  (define update (curry hash-set* state))
  
  (match key

    ["up"
     ; moves the cursor up to the nearest containing handle
     (update
      'stx
      (match stx
        [(⋱x c1⋱ (and (/ handle as/
                         (⋱x c2⋱ (/ bs/ (▹ b))))
                      (not (/ handle _
                              (⋱x _ (/ handle _
                                       (⋱x _ (/ _ (▹ _)))))))))
         (⋱x c1⋱ (/ (handle handle) as/
                    ; bug? ▹ isn't bound if no pair attributes?
                    (▹ (⋱x c2⋱ (/ bs/ b)))))]
        [x x]))]

    ["down"
     ; moves the cursor to the closet handle beneath it
     (update
      'stx
      (match stx
        [(⋱x c⋱ (/ b/
                   (▹ (⋱x d⋱ (/ handle a// a)))))
         (⋱x c⋱ (/ b/
                   (⋱x d⋱ (/ (handle handle) a// (▹ a)))))]
        [x x]))]
    
    ["right"
     ; moves the cursor right in a preorder traversal
     ; 1. if there is a handled fruct under the cursor, select it
     ; 2. otherwise, gather all handled subfructs which don't contain the cursor,
     ;    as well as the current selection, and therein advance the cursor
     (define new-stx
       (match stx
         [(⋱x c⋱ (/ xs/
                    (▹ (⋱x d⋱ (/ handle as/ a)))))
          (⋱x c⋱ (/ xs/ ; bug: requires double handle below?
                    (⋱x d⋱ (/ (handle handle) as/ (▹ a)))))] 
         [(⋱+x c⋱ (capture-when (or (/ _ (▹ _))
                                    (/ (handle _) _
                                       (not (⋱x (/ _ (▹ _)))))))
               `(,as ... ,(/ b// (▹ b)) ,(/ c// c) ,ds ...))
          (⋱+x c⋱
               `(,@as ,(/ b// b) ,(/ c// (▹ c)) ,@ds))]
         [x x]))
     (update 'stx new-stx)]
    
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
                         (insert-menu-at-cursor (/ as/ (▹ a)) stx "")]
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



(require "common.rkt"
         "mode-legacy.rkt"
         "mode-transform.rkt") ; for insert-menu; refactor todo

(require "../fructerm/f-match.rkt"
         "new-syntax.rkt"
         ; temporary renames so as not to intefere with f-match
         (only-in "../containment-patterns/containment-patterns.rkt"
                  (⋱ ⋱x)
                  (⋱1 ⋱1x)
                  (⋱+ ⋱+x)))