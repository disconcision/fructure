#lang racket

(require "../../shared/containment-patterns/containment-patterns/main.rkt"
         "../../shared/slash-patterns/slash-patterns.rkt"
         "../common.rkt"
         "legacy.rkt"
         "transform.rkt") ; for insert-menu; refactor todo

(provide mode:navigate
         mode:navigate-ctrl
         mode:navigate-shift
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
        
        ["shift"
         (update 'mode 'nav-shift)]

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
                         (λ (x)
                           x
                           ; TODO: add prop
                           ; UNCOMMENT TO AUTOCAPTURE SOURCE SYNTAX IN A TRANSFORM
                           #;(if ((disjoin hole-under-cursor? has-captures?) x)
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

        [(and save-number (or "f9" "f10" "f11" "f12"))
         (save! stx save-number)
         state]
        
        #;["f9"
           (define out (open-output-file "fructure-sav-f9.fruct" #:exists 'replace))
           (write stx out)
           (close-output-port out)
           state]
        [_ state])))

(define (mode:navigate-shift pr key state)
  ; navigation settings shifter
  ; primitive scrobber
  ; control scheme:
  ; - use search buffer to filter menu of prop-names
  ; - up/down moves props
  ; - left/right cycles props
  ; supported properties:
  ; for each property, we must provide an inc and dec
  ; - numeric: calculate from [operation inverse delta max min]
  ; - boolean: inc = dec = not
  ; - enum: integer inc/dec index of (index, [vector of values])
  #;(; numeric
     'text-size
     'line-spacing 'char-padding-vertical 'max-menu-length
     'max-menu-length-chars 'length-conditional-cutoff
     ; boolean
     'force-horizontal-layout? 'display-keypresses?
     ;enum
     'typeface)

  (define property 'length-conditional-cutoff)
  (define prop-inc-decs
    (hash 'length-conditional-cutoff (numeric-inc-dec * div 1.5 3 100)))

  ; ACTUAL fn BEGINS -------------------
  (define-from state
    stx mode layout-settings)
  (define update (updater state key))
 
  (match-define (list inc dec)
    (hash-ref prop-inc-decs property
              (list identity identity)))

  (if (equal? pr 'release)
      (match key
        ["shift" (update 'mode 'nav)]
        [_ state])
      (match key
        ["left"
         (update 'layout-settings
                 (hash-update layout-settings
                              property dec))]
        ["right"
         (update 'layout-settings
                 (hash-update layout-settings
                              property inc))]
        [(and save-number (or "f9" "f10" "f11" "f12"))
         (update 'stx (load! save-number))]
        
        [_ state])))

(define (load! save-number)
  (println `(loading ,save-number))
  (define in
    (open-input-file
     (string-append "saves/fructure-sav-" save-number ".fruct")))
  (define saved-state (read in))
  (close-input-port in)
  saved-state)

(define (save! stx save-number)
  (println `(saving ,save-number))
  (define out
    (open-output-file
     (string-append "saves/fructure-sav-" save-number ".fruct")
     #:exists 'replace))
  (write stx out)
  (close-output-port out))

(define (numeric-inc-dec prop-operation
                         prop-inverse
                         prop-delta
                         prop-min
                         prop-max)
  (define (apply-if p f x)
    (define fx (f x))
    (if (p fx) fx x))
  (define (in-range prop)
    (< prop-min prop prop-max))
  (define inc
    (curry apply-if in-range
           (curryr prop-operation
                   prop-delta)))
  (define dec
    (curry apply-if in-range
           (curryr prop-inverse
                   prop-delta)))
  (list inc dec))

; idea for changed properties to unfixed point
; need a different level of abstraction
#;(define ((adaptive f) prop)
    (let loop ([prop-cur prop]
               [iters 10]
               [fp (f prop)])
      (println `(iters ,iters))
      (when (zero? iters)
        (println "WARNING: ADAPTIVE PROP CHANGER TIMED OUT"))
      (if (or (not (equal? fp f)) (zero? iters))
          fp
          (loop fp (sub1 iters)))))