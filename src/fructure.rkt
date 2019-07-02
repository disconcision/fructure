#lang racket

; andrew blinn 2018-2019

(require 2htdp/image
         2htdp/universe)

; internal structure
(require "language/syntax.rkt"
         ; syntax generators
         "language/semantics.rkt"
         ; syntax->attributed-syntax
         "layout/draw-fruct-layers.rkt" ; syntax->pixels
         "layout/input-history.rkt"
         
         "mode/transform.rkt" 
         "mode/navigate.rkt"
         "mode/command.rkt"
         "common.rkt")

(define-values (screen-x screen-y)
  (values 1904 1012))

#|

  fructure, a structured interaction engine.

  Fructure is concerned with transforming, considering,
  conversing with, and being transformed by structure.

  FRUCTS are composites of attributed sexprs used to model structure.
  The state of fructure is the object structure augmented with UI widgets
  called syntactic affordances, which comprise both syntactic annotation
  and as an encompassing metagrammar embedding the object grammar.

  LANGUAGE/syntax determines the base transformations of
  the object structure and hence the space of possible structures

  LANGUAGE/semantics annotates structure with calculated attributes & affordances
  informing interaction. These (morally, lol) represent an attribute grammar,
  letting context-free rewriting respect context-sensitive properties.

  MODESs determine the mapping between input and pure functions of state.
  input modes are each defined in seperate modules, indicated above.

  LAYOUT maps structure to the screen

  Certain extensional aspects of the object structure, including history,
  logging, and interaction buffers with external devices, CURRENTLY live
  outside the core state.

|#


; -------------------------------------------------
; SETTINGS

(define-map initial-layout

  ; SCALING PARAMETERS
  'text-size 50
  'radius-multiplier 1.0
  'radius-multiplier-atoms 5/7
  'typeface "Iosevka, Light"
  'line-spacing 0 ; 1
  'char-padding-vertical 0 ; 2 ; 5

  ; DEBUGGING OPTIONS
  'display-keypresses? #t ; show a list of the n last keypresses

  ; PROJECTION OPTIONS
  ; BUG: removing ref is WEIRD
  ; BUG: removing num has no effect
  'implicit-forms '(ref num app cp lp lps mp mapp) ; hide heads
  'top-background-color (color 0 47 54)

  ; FRUCTS
  ; i layout
  'length-conditional-cutoff 14
  'force-horizontal-layout? #f ; uninvade the second dimension 
  'length-conditional-layout? #t ; unless our children weigh more than
  ; ii atoms
  'form-color (color 0 130 214) ; form-id glyph color
  'literal-color (color 255 199 50) ; (numeric) literals glyph color
  'identifier-color (color 48 161 182) #;(color 0 0 0) ; identifier glyph color
  ; iii blocks
  'alternate-bkg-vertical? #t
  'alternate-bkg-horizontal? #t
  'bkg-color (color 0 47 54) ; primary block color, equal to top-background-color
  'outline-block-width 1 ; primary block outline width
  'background-block-color (color 0 52 59) ; secondary block color
  'background-block-width 2 ; BUSTED; todo: implement proper outlines for secondaries
  'outline-block-color (color 0 70 79)#;(color 0 61 65) ; primary outline color
  ; iv patterns
  'pattern-identifier-color (color 230 230 230) ; identifier glyph color in patterns
  'pattern-shade-one (color 11 38 53) #;(color 84 84 84) ; pattern background
  'pattern-shade-two (color 110 110 110) ; mostly unused - future secondary pattern color

  ; HOLES
  'hole-as-sort? #f
  'hole-bottom-color (color 252 225 62)
  'hole-side-color (color 193 115 23)
  '+hole-color (color 25 80 84)
  '+hole-color-pattern (color 170 170 170 120) ; todo: refactor
  '+hole-color-number (color 130 0 0) ; todo: refactor

  ; SELECTOR
  'selection-outline-width 2
  'transform-outline-width 2
  'menu-outline-width 2
  'selected-color (color 230 0 0)
  'selected-atom-color (color 255 255 255)

  ; TRANSFORM
  'erase-captures-after-transform? #t
  'transform-template-only? #f ; don't show -> and target
  'transform-color (color 230 0 0) ; same as selected-color
  'transform-tint-color (color 70 0 0) ; based on selected-color
  'transform-arrow-color (color 255 255 255)
  'tint-template? #t
  'use-transform-template-scheme? #f
  'transform-template-scheme
  (list 'background-block-color (color 215 215 215)
        'bkg-color (color 200 200 200)
        'outline-block-color (color 150 150 150)
        'identifier-color (color 50 50 50)
        'literal-color (color 193 115 23))

  ; MENU
  'simple-menu? #f ; only red outline, dark backing
  'simple-menu-background-color (color 40 40 40)
  'max-menu-length 4 ; default maximum completions
  'max-menu-length-chars 1 ; same, for single-character menus
  'menu-outline-color (color 230 0 0) ; same as selected-color
  'menu-search-color (color 230 0 0) ; same as selected-color
  'menu-bkg-color (color 0 35 39)
  'menu-secondary-color (color 0 24 27)
  
  ; CAPTURES
  'capture-pattern-shade-one (color 0 0 0) ; underlying base
  ; todo: add pattern-identifier-color=black so above can be light
  'capture-atom-color (color 160 160 160)
  'capture-shade-one (color 40 40 40) #;(color 160 160 160)
  'capture-shade-two (color 70 70 70) #;(color 170 170 170)
  'capture-color-a (color 0 215 215)
  'capture-color-b (color 0 215 0)
  'capture-color-c (color 255 0 255)
  'capture-color-d (color 215 215 0)
  'capture-color-x (color 0 215 0)

  ; BUSTED
  'custom-menu-selector? #t ; needs work
  'popout-transform? #t ; layer transform above structure
  'popout-menu? #t ; layer menu above structure

  ; WEIRD
  'quit #f
  )


; calculates dynamic settings derived from the above
(define (add-dynamic-settings layout)
  (define-from layout
    text-size typeface radius-multiplier
    radius-multiplier-atoms
    char-padding-vertical)
  (define (div-integer x y)
    (inexact->exact (round (/ x y))))
  (define space-image
    (text/font " " text-size "black"
               typeface 'modern 'normal 'normal #f))

  (hash-set* layout
             'menu-expander-height (round (* 1/4 text-size))
             'radius (* radius-multiplier (sub1 (div-integer text-size 2)))
             'radius-adj (* radius-multiplier-atoms (sub1 (div-integer text-size 2)))
             'margin (div-integer text-size 5)
             'unit-width (image-width space-image)
             'unit-height (+ char-padding-vertical
                             (image-height space-image))))


; -------------------------------------------------
; SEED

(define-map initial-state
  ; we begin in navigation mode
  'mode 'nav
  'mode-last 'nav
  ; with a selected hole
  'stx (fruct-augment initial-stx)
  ; new: current selection filter
  'search-buffer '(▹ "")
  'command-buffer ""
  'command-pointer 'text-size
  'command-pointer-offset 0
  ; new: initial layout settings
  'layout-settings (add-dynamic-settings initial-layout)
  ; transforms: history, currently broken
  'transforms '()
  'history '()
  'keypresses '(">")
  'key-state #hash()
  ; messages: log, currently disused
  'messages '("bang"))


; -------------------------------------------------
; INPUT

(define ((input-keyboard pr) state key)
  ; mode-loop : key x state -> state
  ; determines the effect of key based on mode
  (define-from state
    stx mode search-buffer
    command-buffer key-state)
  
  ; print debugging information
  (displayln `(mode: ,mode ': ,pr key: ,key))
  #;(displayln `(projected: ,(project stx)))
  #;(displayln `(command-buffer ,command-buffer))
  (displayln `(search-buffer: ,search-buffer))
  #;(displayln `(keypresses ,keypresses))
  #;(displayln state)

  ; dispatch based on current mode
  #;(println "transform time: ")
  (define mode-handler
    (match mode
      ['menu            mode:transform]
      ['command         mode:command]
      #;['transform-ctrl  mode:transform-ctrl]
      ['transform-shift mode:transform-shift]
      ['nav             mode:navigate]
      ['nav-ctrl        mode:navigate-ctrl]
      ['nav-shift       mode:navigate-shift]))
  (define new-state
    (mode-handler pr key state))
  
  ; augment syntax with attributes
  ; calculate dynamic settings
  #;(println "augment time: ")
  (update-map new-state
              ['stx fruct-augment]
              ['layout-settings add-dynamic-settings]
              ['key-state (curryr hash-set key pr)]))


; -------------------------------------------------
; OUTPUT

(define (output state)
  ; output : state -> image
  (define-from state
    stx layout-settings keypresses command-buffer command-pointer)
  (define-from layout-settings
    menu-expander-height menu-outline-width
    display-keypresses? text-size top-background-color)
  (define offset
    (+ menu-expander-height menu-outline-width))
  #;(println "output time: ")
  (match-define (list _ fruct-image)
    (draw-fruct-layers state screen-x screen-y))

  (define empty-card
    (rectangle text-size text-size "solid"(color 0 0 0 0)))

  (define background
    (rectangle screen-x screen-y "solid" top-background-color))

  (define (add-layer-to-stack new-layer stack)
    (overlay/align/offset
     "left" "top"
     new-layer
     0 (+ 50 (* 2 menu-expander-height))
     stack))

  (define (offset-to-match-fruct thing)
    (overlay/align/offset
     "left" "top"
     thing
     (- offset) (- offset)
     (rectangle screen-x text-size "solid" (color 0 0 0 0))))

  (define card-stack
    (list empty-card
          (offset-to-match-fruct (display-command-line state))
          (if display-keypresses?
              (offset-to-match-fruct (display-keypresses state))
              empty-card)
          empty-card
          fruct-image))

  (define nu-stack
    (for/fold ([stack background])
              ([card (reverse card-stack)])
      (add-layer-to-stack card stack)))
   
  (overlay/align/offset "left" "top" nu-stack -100 0 background))

; -------------------------------------------------
; EXPPERIMENTAL ON-TICK UPDATER

#;(define (do-it state)
    #;(println "doing it")
    (define-from state
      stx layout-settings key-state)
    (define pr-left (hash-ref key-state "left" 0))
    (define pr-right (hash-ref key-state "right" 0))
    (define mod-left (if (equal? 'press pr-left) 1 0))
    (define mod-right (if (equal? 'press pr-right) 1 0))
    #;(define simu-key (match (- mod-left mod-right)
                         [0 state]
                         [(? (curry > 0)) (mode:navigate-ctrl 'press "left" state)]
                         [(? (curry < 0)) (mode:navigate-ctrl 'press "left" state)]))
    (match (- mod-left mod-right)
      [0 state]
      [(? (curry > 0)) (mode:navigate-ctrl 'press "left" state)]
      [(? (curry < 0)) (mode:navigate-ctrl 'press "right" state)])
    #;(mode:navigate-ctrl 'press simu-key state))

; -------------------------------------------------
; FRUCTURE CORE

(define (go)
  (big-bang initial-state  
    ; MY LOVE FOR YOU IS LIKE A TRUCK
    [name 'fructure]
    ; BERSERKER
    [on-key (input-keyboard 'press)]
    [on-release (input-keyboard 'release)]
    #;[on-tick do-it 1/4]
    [to-draw output screen-x screen-y]
    [stop-when (λ (state) (define-from state command-pointer layout-settings)
                 (and (equal? command-pointer 'quit)
                      (hash-ref layout-settings 'quit)))]
    ; todo: insert closing image above, tune below 1 (seconds)
    [close-on-stop 1]
    #;[display-mode 'fullscreen]
    #;[record? "gif-recordings"]))

(let ([msg "bye felicia"])
  ; wrapped to supress printing of final state
  (go) msg)