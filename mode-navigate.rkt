#lang racket

(provide mode:navigate
         mode:navigate-ctrl
         capture-at-cursor)

(define (mode:navigate pr key state)
  ; navigation major mode
  (define-from state
    stx mode transforms messages layout-settings)
  (define update (updater state key))
  
  (if (equal? pr 'release)
      state
      (match key

        ["control"
         (update 'mode 'nav-ctrl)]

        ["f1"
         (println `(BEGIN-STX ,stx))
         state]

        ["up"
         ; moves the cursor up to the nearest containing handle
         ; alternative handle symbol: ⥰
         (update
          'stx
          (match stx
            [(⋱ c1⋱ (and (/ handle as/
                            (⋱ c2⋱ (/ bs/ (▹ b))))
                         (not (/ handle _
                                 (⋱ _ (/ handle _
                                         (⋱ _ (/ _ (▹ _)))))))))
             (⋱ c1⋱ (/ [handle handle] as/
                       ; bug? ▹ isn't bound if no pair attributes?
                       (▹ (⋱ c2⋱ (/ bs/ b)))))]
            [x x]))]

        ["down"
         ; moves the cursor to the closet handle beneath it
         (update
          'stx
          (match stx
            [(⋱ c⋱ (/ b/
                      (▹ (⋱ d⋱ (/ handle a/ a)))))
             (⋱ c⋱ (/ b/
                      (⋱ d⋱ (/ [handle handle] a/ (▹ a)))))]
            [x x]))]
    
        ["right"
         ; moves the cursor right in a preorder traversal
         ; 1. if there is a handled fruct under the cursor, select it
         ; 2. otherwise, gather all handled subfructs which don't contain the cursor,
         ;    as well as the current selection, and therein advance the cursor
         (define new-stx
           (match stx
             [(⋱ c⋱ (/ xs/
                       (▹ (⋱ d⋱ (/ handle as/ a)))))
              (⋱ c⋱ (/ xs/ ; bug: requires double handle below?
                       (⋱ d⋱ (/ [handle handle] as/ (▹ a)))))] 
             [(⋱+ c⋱ (capture-when (or (/ _ (▹ _))
                                       (/ [handle _] _
                                          (not (⋱ (/ _ (▹ _)))))))
                  `(,as ... ,(/ b/ (▹ b)) ,(/ c/ c) ,ds ...))
              (⋱+ c⋱
                  `(,@as ,(/ b/ b) ,(/ c/ (▹ c)) ,@ds))]
             [x x]))
         (update 'stx new-stx)]
    
        ["left"
         ; moves the cursor left in a preorder traversal
         ; 1. if there is a left-sibling to the cursor which contains-or-is a handle,
         ;    select its rightmost handle not containing another handle
         ; 2. otherwise, find the most immediate containing handle;
         ;    that is, a containing handle not containing a handle containing ▹
         (define new-stx
           (match stx
             [(⋱ c1⋱ `(,as ...
                       ,(⋱+ c2⋱ (capture-when (/ [handle _] _ (not (⋱ _ (/ [handle _] _ _)))))
                            `(,bs ... ,(/ c/ c)))
                       ,ds ... ,(/ e/ (▹ e)) ,fs ...))
              (⋱ c1⋱ `(,@as ,(⋱+ c2⋱ `(,@bs ,(/ c/ (▹ c))))
                            ,@ds ,(/ e/ e) ,@fs))]
             [(⋱ c1⋱ (and (/ [handle h] a/ (⋱ c2⋱ (/ b/ (▹ b))))
                          (not (/ [handle _] _ (⋱ _ (/ [handle _] _ (⋱ _ (/ _ (▹ _)))))))))
              (⋱ c1⋱ (/ [handle h] a/ (▹ (⋱ c2⋱ (/ b/ b)))))]
         
             [x x]))
         (update 'stx new-stx)]

        ["\r"
         ; ENTER: insert a menu and switch to transform mode
         ; todo: if possible, factor out insert-menu-at for encapsulation
         (define (setup-transform-mode stx)
           (match stx 
             [(⋱ c⋱ (/ as/
                       (▹ a)))
              (⋱ c⋱ (/ [transform
                        ; todo: fix hardcoded init buffer here:
                        (insert-menu-at-cursor (/ as/ (▹ a)) stx '(▹ ""))]
                       as/ a))]))
         (define hole-under-cursor?
           (match-lambda? (⋱ c⋱ (/ _/ (▹ (or '⊙ '⊙+))))))
         (update
          'mode 'menu
          'stx ((compose setup-transform-mode
                         (λ (x) (if ((disjoin hole-under-cursor? has-captures?) x)
                                    x
                                    (capture-at-cursor x))))
                stx))]

        ["\t"
         ; paint selection as metavariable
         ; if there are metavariables under the selection, erase them first
         ; metavariables sibling/cousin to cursor may be renamed
         (update 'stx (capture-at-cursor stx))]

        ["escape"
         ; release all extant metavariables
         (update 'stx (erase-captures stx))]
        
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
         (mode:legacy key state)])))


(define (mode:navigate-ctrl pr key state)
  ; navigation control mode
  (define-from state
    stx mode layout-settings)
  (define update (updater state key))
  (if (equal? pr 'release)
      (match key
        ["control" (update 'mode 'nav)]
        [_ state])
      (match key
        ["left"
         (define current-size (hash-ref layout-settings 'text-size))
         (define new-size (if (>= 10 current-size) current-size (- current-size 10)))
         (update 'layout-settings
                 (hash-set* layout-settings
                            'text-size new-size))]
        ["right"
         (define current-size (hash-ref layout-settings 'text-size))
         (define new-size (if (>= current-size 240) current-size (+ current-size 10)))
         (update 'layout-settings
                 (hash-set* layout-settings
                            'text-size new-size))]
        [_ state])))


(require "common.rkt"
         "mode-legacy.rkt"
         "mode-transform.rkt") ; for insert-menu; refactor todo

(require "new-syntax.rkt"
         containment-patterns)
