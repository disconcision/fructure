#lang racket

; andrew blinn 2018

(require 2htdp/image
         2htdp/universe)

; internal structure
(require "language.rkt"
         ; syntax generators
         "attributes.rkt"
         ; syntax->attributed-syntax
         "layout.rkt"
         ; syntax->pixels
         "mode-transform.rkt"
         ; input mode
         "mode-navigate.rkt"
         ; input mode
         "common.rkt")

(define-values (screen-x screen-y)
  (values 1200 800))

#|

  structured interaction engine

  Fructure is transforming, conversing with, and being transformed by structure.

  Modes determine the mapping between input and pure functions of state.
  input modes are each defined in seperate modules, indicated above.
  The fruits of our labors are realized visually, by a layout lens indicated above.

  Language.rkt determines the shape of possible mappings, and hence possible structure.

  Our object 'in itself' is a syntax object, a composite of
  labelled sexprs called fructs. The UI is itself part of the syntax,
  as both syntactic annotation and an encompassing metagrammar.

  Extensional aspects of the object, including history, logging, and
  interaction buffers with external devices, are represented in the state.

  Between interactions, we automatically annotate our structure with
  contextual cues to inform further interaction. These (morally)
  represent an attribute grammar, permitting context-free
  rewriting to respect context-sensitive properties.


|#


; -------------------------------------------------
; DISPLAY SETTINGS

(define-map initial-layout
  
  ; scaling parameters
  'text-size 30
  'line-spacing 0 ; 1
  'char-padding-vertical 3 ; 5
  
  ; hide the heads of these forms
  'implicit-forms '(ref num app cp lp mp)

  ; show a list of the n last keypresses
  'display-keypresses? #t
  
  ; maximum completions
  'max-menu-length 4
  ; for single-character menus
  'max-menu-length-chars 1
  
  ; layer transform/menu above structure
  'popout-transform? #t
  'popout-menu? #t

  'force-horizontal-layout? #f
  ; invade the second dimension 
  'length-conditional-layout? #t
  ; but only if our children weigh more than
  'length-conditional-cutoff 8
  
  ; beautify menu
  'dodge-enabled? #t
  ; beautify menu selector
  'custom-menu-selector? #t

  ; show parentheses. not fully implemented
  'show-parens? #f

  ; look at the pretty colors
  'transform-tint-color (color 160 0 0) ;selected-color
  'selected-atom-color (color 255 255 255)
  'menu-bkg-color (color 112 112 112)
  'form-color (color 0 130 214)
  'literal-color (color 255 131 50)
  'grey-one (color 230 230 230)
  'grey-two (color 215 215 215)
  'pattern-grey-one (color 84 84 84)
  'identifier-color (color 0 0 0)
  'selected-color (color 230 0 0)
  'hole-color (color 0 180 140)
  'transform-arrow-color (color 255 255 255)
  'bkg-color (color 0 47 54)
  'pattern-bkg-color (color 230 230 230)
  'pattern-grey-one (color 76 76 76)
  'pattern-grey-two (color 110 110 110))


; -------------------------------------------------
; SEED

(define-map initial-state
  ; we begin in navigation mode
  'mode 'nav
  ; with a selected hole
  'stx (fruct-augment initial-stx)
  ; new: current selection filter
  'search-buffer ""
  ; new: initial layout settings
  'layout-settings initial-layout
  ; transforms: history, currently broken
  'transforms '()
  'history '()
  'keypresses '()
  ; messages: log, currently disused
  'messages '("bang"))


; -------------------------------------------------
; INPUT

(define (input-keyboard state key)
  ; mode-loop : key x state -> state
  ; determines the effect of key based on mode
  (define-from state stx mode search-buffer keypresses)
  
  ; print debugging information
  #;(displayln `(mode: ,mode  key: ,key))
  #;(displayln `(projected: ,(project stx)))
  (displayln `(search-buffer: ,search-buffer))
  #;(displayln `(keypresses ,keypresses))
  #;(displayln state)

  ; dispatch based on current mode
  (define new-state
    (match mode
      ['menu (mode:transform key state)]
      ['nav  (mode:navigate key state)]))
  
  ; augment syntax with attributes
  (update-map new-state
              [stx fruct-augment]))


; -------------------------------------------------
; OUTPUT

(define (output state)
  ; output : state -> image
  (define-from state
    stx layout-settings keypresses)
  (define-from layout-settings
    display-keypresses?)
  
  (match-define (list _ image-out)
    ; second here skips the top (diamond) affo
    ; todo: make this less hacky by going fs
    (fructure-layout (second stx) layout-settings
                     screen-x screen-y))

  (if display-keypresses?
      (overlay/align
       "left" "top"
       (display-keypresses keypresses)
       image-out)
      image-out))


; -------------------------------------------------
; FRUCTURE CORE

(big-bang initial-state
  ; MY LOVE FOR YOU IS LIKE A TRUCK
  [name 'fructure]
  [on-key input-keyboard]
  [to-draw output screen-x screen-y]
  #;[display-mode 'fullscreen]
  #;[record? "gif-recordings"])

