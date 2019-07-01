#lang racket

(provide mode:command)

(require "../../shared/containment-patterns/containment-patterns/main.rkt"
         "../../shared/slash-patterns/slash-patterns.rkt"
         "../common.rkt")

(define props-map
  (hash 'hole-as-sort? (list 'boolean)
        'quit (list 'boolean)
        'length-conditional-cutoff (list 'numeric * div 1.5 3 100)
        'text-size (list 'numeric + - 10 10 240)
        'line-spacing (list 'numeric + - 1 -1 30)
        'char-padding-vertical (list 'numeric + - 1 0 30)
        'display-keypresses? (list 'boolean)
        'radius-multiplier (list 'numeric + - 0.1 0.0 100)
        #;#;'radius-multiplier-atoms (list 'numeric + - 0.1 0.0 100)))


(define (mode:command pr key state)
  ; primitive settings scrobber
  ; control scheme:
  ; - use search buffer to filter menu of prop-names
  ; - up/down moves props
  ; - left/right cycles props
  (define-from state
    stx mode layout-settings
    command-buffer command-pointer command-pointer-offset)
  (define update (updater state key))

  (match-define (list inc dec)
    (hash-ref prop-inc-decs command-pointer
              (list identity identity)))

  (if (equal? pr 'release)
      (match key
        [_ state])
      (match key
        [(or #;" " "escape")
         (update 'mode (hash-ref state 'mode-last 'nav))]
        ["left"
         (update 'layout-settings
                 (hash-update layout-settings
                              command-pointer dec))]
        ["right"
         (update 'layout-settings
                 (hash-update layout-settings
                              command-pointer inc))]

        ["\u007F" ; delete
         (update 'command-buffer ""
                 'command-pointer 'text-size)]

        ["down"
         (define menu (make-command-menu layout-settings prop-inc-decs))
         (define filtered-menu (filter-menu command-buffer menu))
         (define offset-candidate (modulo (add1 command-pointer-offset) (length filtered-menu)))
         (define new-prop (string->symbol (first (list-ref filtered-menu offset-candidate))))
         ; todo: clean up first first above
         (update 'command-pointer new-prop
                 'command-pointer-offset offset-candidate)]
        ["up"
         (define menu (make-command-menu layout-settings prop-inc-decs))
         (define filtered-menu (filter-menu command-buffer menu))
         (define offset-candidate (modulo (sub1 command-pointer-offset) (length filtered-menu)))
         (define new-prop (string->symbol (first (list-ref filtered-menu offset-candidate))))
         ; todo: clean up first first above
         (update 'command-pointer new-prop
                 'command-pointer-offset offset-candidate)]
        
        ["\b"
         ; todo: ideally we'd like to retain current menu selection
         ; after pressing bksp
         (define command-buffer-candidate
           (match command-buffer
             ["" ""]
             [(and s (? string?)) (substring s 0 (sub1 (string-length s)))]
             [x (error "command-buffer:bksp" x)]))
         (define command-pointer-candidate
           (move-command-pointer prop-inc-decs command-buffer-candidate layout-settings))
         ; todo: refactor: dont require layout-settings, just current prop value
         (if command-pointer-candidate
             (update 'command-pointer command-pointer-candidate
                     'command-pointer-offset 0
                     'command-buffer command-buffer-candidate)
             state)]

        [(regexp #rx"^[0-9A-Za-z?!\\\\-]$" c)
         #:when c
         ; hack? otherwise this seems to catch everything?
         ; maybe since we're matching against a key event...

         (define (convert-special-chars c)
           (hash-ref (hash "\\" "λ")
                     (first c) (first c)))

         (define command-buffer-candidate
           (match command-buffer
             [(? string? s) (string-append s (convert-special-chars c))]))
         (define command-pointer-candidate
           (move-command-pointer prop-inc-decs command-buffer-candidate layout-settings))
         (if command-pointer-candidate
             (update 'command-pointer command-pointer-candidate
                     'command-pointer-offset 0
                     'command-buffer command-buffer-candidate)
             state)]
        
        [_ state])))


(define (make-command-menu layout-settings prop-hs)
  ; list with string names of props and current values
  (for/list ([(k v) prop-hs])
    (list (symbol->string k)
          (hash-ref layout-settings k))))

(define (filter-menu command-buffer menu)
  (filter (match-lambda [(list prop _) (string-prefix? prop command-buffer)])
          menu))

(define (move-command-pointer prop-inc-decs new-command-buffer layout-settings)
  (define menu (make-command-menu layout-settings prop-inc-decs))
  (define filtered-menu (filter-menu new-command-buffer menu))
  (if (empty? filtered-menu)
      #false ; in this case, reject new-command-buffer
      (match (first filtered-menu)
        [(list k v) (string->symbol k)])))
  


(define (boolean-inc-dec)
  (list not not))


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

(define prop-inc-decs
  (for/hash ([(k v) props-map])
    (values k (case (first v)
                ['boolean (boolean-inc-dec)]
                ['numeric (apply numeric-inc-dec (rest v))]))))