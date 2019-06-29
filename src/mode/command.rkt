#lang racket

(provide mode:command)

(require "../../shared/containment-patterns/containment-patterns/main.rkt"
         "../../shared/slash-patterns/slash-patterns.rkt"
         "../common.rkt")

(define (mode:command pr key state)
  ; primitive settings scrobber
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

  

  ; ACTUAL fn BEGINS -------------------
  (define-from state
    stx mode layout-settings command-buffer)
  (define update (updater state key))

  (define property (hash-ref layout-settings
                             'temp-command-property
                             'length-conditional-cutoff))
  (define prop-inc-decs
    (hash 'length-conditional-cutoff (numeric-inc-dec * div 1.5 3 100)
          'text-size (numeric-inc-dec + - 10 10 240)))
  
  (match-define (list inc dec)
    (hash-ref prop-inc-decs property
              (list identity identity)))

  (println "IN ALL OUR MODES COMMAND")

  (if (equal? pr 'release)
      (match key
        #;["shift" (update 'mode 'nav)]
        [_ state])
      (match key
        [(or " " "escape")
         (update 'mode 'nav)]
        ["left"
         (update 'layout-settings
                 (hash-update layout-settings
                              property dec))]
        ["right"
         (update 'layout-settings
                 (hash-update layout-settings
                              property inc))]

        ["1"
         (update 'layout-settings
                 (hash-set layout-settings
                           'temp-command-property 'text-size))]
        ["2"
         (update 'layout-settings
                 (hash-set layout-settings
                           'temp-command-property 'length-conditional-cutoff))]

        ["\b"
         ; todo: ideally we'd like to retain current menu selection
         ; after pressing bksp
         (define buffer-candidate
           (match command-buffer
             [""
              ""]
             [(and s (? string?) (not (== "")))
              (substring s 0 (sub1 (string-length s)))]
             [x (error "command-buffer:bksp" x)]))
         #;(define-values (new-stx-candidate newest-buffer-candidate)
             (menu-filter-in-stx "\b" stx search-buffer buffer-candidate))
         (update #;#;'stx new-stx-candidate
                 'command-buffer buffer-candidate)]

        [(regexp #rx"^[0-9A-Za-z?!\\\\-]$" c)
         #:when c
         ; hack? otherwise this seems to catch everything?
         ; maybe since we're matching against a key event...

         (define (convert-special-chars c)
           (hash-ref (hash "\\" "λ")
                                       (first c) (first c)))

         (define buffer-candidate
           (match command-buffer
             [(? string? s)
              (string-append s (convert-special-chars c))]))

         #;(define-values (new-stx-candidate newest-buffer-candidate)
           (menu-filter-in-stx c stx search-buffer buffer-candidate))
         (update #;#;'stx new-stx-candidate
                 'command-buffer buffer-candidate)]
        
        
        [_ state])))


(define (mode-mode-mode pr key state)
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