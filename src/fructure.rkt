#lang racket

; andrew blinn 2018-2019

(require 2htdp/image
         2htdp/universe)

; internal structure
(require "language/language.rkt"
         ; syntax generators
         "language/attributes.rkt"
         ; syntax->attributed-syntax
         "layout/layout.rkt"
         ; syntax->pixels
         "mode/transform.rkt"
         ; input mode
         "mode/navigate.rkt"
         ; input mode
         "common.rkt")

(define-values (screen-x screen-y)
  (values 1904 1012))

#|

  fructure, a structured interaction engine.

  Fructure is concerned with transforming, considering,
  conversing with, and being transformed by structure.

  Modes determine the mapping between input and pure functions of state.
  input modes are each defined in seperate modules, indicated above.
  The current state is projected visually by layout.rkt.

  Language.rkt determines the shape of possible mappings, and hence possible structure.

  Our syntax object is a composite of attributed sexprs called fructs.
  The UI is itself part of the syntax, as both syntactic annotation
  and as an encompassing metagrammar.

  Extensional aspects of the object, including history, logging, and
  interaction buffers with external devices, are represented in the state.

  Between interactions, we automatically annotate our structure with
  contextual cues (attributes.rkt) to inform further interaction. These
  (morally, lol) represent an attribute grammar, permitting context-free
  rewriting to respect context-sensitive properties.

|#


; -------------------------------------------------
; DISPLAY SETTINGS

(define-map initial-layout

  ; SCALING PARAMETERS
  'text-size 50
  'typeface "Iosevka, Light"
  'line-spacing 0 ; 1
  'char-padding-vertical 2 ; 5
  
  ; BUSTED & EXPERIMENTAL OPTIONS
  'custom-menu-selector? #t ; beautify menu selector ?
  'show-parens? #f ; show parentheses. not fully implemented
  'background-block-color (color 25 80 84) ;? what even is this?
  'dodge-enabled? #t  ; beautify menu - TODO: REMOVE THIS

  ; DEBUGGING OPTIONS
  'display-keypresses? #t ; show a list of the n last keypresses

  ; PROJECTION OPTIONS
  'implicit-forms '(ref num app cp lp lps mp mapp) ; hide heads

  ; TRANSFORM & MENU OPTIONS
  'menu-bkg-color (color 0 35 39)
  'menu-secondary-color (color 0 24 27)
  'transform-tint-color (color 160 0 0) ;selected-color
  'transform-arrow-color (color 255 255 255)
  'transform-template-only #f ; don't show -> and target
  'simple-menu? #f ; only red outline, dark backing
  'max-menu-length 4 ; maximum completions
  'max-menu-length-chars 1 ; same, for single-character menus
  'popout-transform? #t ; layer transform above structure
  'popout-menu? #t ; same

  ; NAVIGATION & SELECTION OPTIONS
  'selected-color (color 230 0 0)
  'selected-atom-color (color 255 255 255)
  
  ; LAYOUT OPTIONS
  'force-horizontal-layout? #f ; uninvade the second dimension 
  'length-conditional-layout? #t ; unless our children weigh more than
  'length-conditional-cutoff 14
    
  ; BACKGROUND COLORS
  'bkg-color (color 0 47 54)
  'grey-one (color 0 47 54) #;(color 230 230 230)
  'grey-two (color 0 47 54) #;(color 215 215 215)
  'pattern-bkg-color (color 230 230 230)
  'pattern-grey-one (color 17 39 46) #;(color 84 84 84)
  'pattern-grey-two (color 110 110 110)

  ; FORM COLORS
  'form-color (color 0 130 214)
  'literal-color (color 255 131 50)
  'identifier-color (color 48 161 182) #;(color 0 0 0)
  'hole-bottom-color (color 252 225 62)
  'hole-side-color (color 193 115 23)
  '+hole-color (color 25 80 84)
  )


; calculates dynamic settings derived from the above
(define (add-dynamic-settings layout)
  (define-from layout
    text-size typeface
    char-padding-vertical)
  (define (div-integer x y)
    (inexact->exact (round (/ x y))))
  (define space-image
    (text/font " " text-size "black"
               typeface 'modern 'normal 'normal #f))
  (hash-set* layout
             'radius (sub1 (div-integer text-size 2))
             'margin (div-integer text-size 5)
             'unit-width (image-width space-image)
             'unit-height (+ char-padding-vertical
                             (image-height space-image))))


; -------------------------------------------------
; SEED

(define-map initial-state
  ; we begin in navigation mode
  'mode 'nav
  ; with a selected hole
  'stx (fruct-augment initial-stx)
  ; new: current selection filter
  'search-buffer '(▹ "")
  ; new: initial layout settings
  'layout-settings (add-dynamic-settings initial-layout)
  ; transforms: history, currently broken
  'transforms '()
  'history '()
  'keypresses '()
  'key-state #hash()
  ; messages: log, currently disused
  'messages '("bang"))


; -------------------------------------------------
; INPUT

(define ((input-keyboard pr) state key)
  ; mode-loop : key x state -> state
  ; determines the effect of key based on mode
  (define-from state
    stx mode search-buffer key-state)
  
  ; print debugging information
  #;(displayln `(mode: ,mode ': ,pr key: ,key))
  #;(displayln `(projected: ,(project stx)))
  #;(displayln `(search-buffer: ,search-buffer))
  #;(displayln `(keypresses ,keypresses))
  #;(displayln state)

  ; dispatch based on current mode
  #;(println "transform time: ")
  (define mode-handler
    (match mode
      ['menu            mode:transform]
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
    stx layout-settings keypresses)
  (define-from layout-settings
    display-keypresses?)
  #;(println "output time: ")
  (match-define (list _ image-out)
    ; second here skips the top (diamond) affo
    ; todo: make this less hacky by going fs
    (fructure-layout (second stx) layout-settings
                     screen-x screen-y))
  (if display-keypresses?
      (place-image/align (display-keypresses keypresses)
                         200 50
                         "left" "top"
                         image-out)
      image-out))

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
    #;[display-mode 'fullscreen]
    #;[record? "gif-recordings"]))

(let ([msg "bye felicia"])
  ; wrapped to supress printing of final state
  (go) msg)