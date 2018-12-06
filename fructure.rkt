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


#|

  This is fructure. Fructure is a world in the hdtp2 universe.
  Fructure orbits around a STATE, which reflects the outer aspect
  of its STRUCTURE.

  Our object is structural interaction. Fructure is about regarding,
  transforming, conversing with, and being transformed by structure.

  The state concerns our interface with structure. It represents
  the lens through which we apprehend structure, and provides
  a record of our interactions.

  We devide our interactions into modes, which are mappings
  taking inputs and states to states. INPUT modes are defined
  in seperate modules, indicated above. The fruits of our labors
  are realized visually, by an OUTPUT (layout) lens indicated above.

  Within a mode, your input determines a purely functional mapping
  from the current structure to a new one. Our LANGUAGE determines
  the shape possible mappings, and hence possible structure.

  External aspects of this update, including history, logging, and
  interaction buffers with external devices, is represented in
  the 'outer structure' which is the state.

  Between interactions, we annotate our structure with
  contextual cues to inform further interaction. These (morally)
  represent an ATTRIBUTE grammar, permitting context-free
  rewriting to respect context-sensitive properties.


|#


; -------------------------------------------------
; DISPLAY SETTINGS

(define-map initial-layout
  
  ; scaling parameter
  'text-size 30
  
  ; hide the heads of these forms
  'implicit-forms '(ref app)
  
  ; maximum completions
  'max-menu-length 4
  ; for single-character menus
  'max-menu-length-chars 1
  
  ; layer transform/menu above structure
  'popout-transform? #t
  'popout-menu? #t
  
  ; invade the second dimension 
  'length-conditional-layout? #t
  ; but only if our children weigh more than
  'length-conditional-cutoff 8
  
  ; beautify menu
  'dodge-enabled? #t
  ; beautify menu selector
  'custom-menu-selector? #t

  ; look at the pretty colors
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
  ; messages: log, currently disused
  'messages '("bang"))


; -------------------------------------------------
; INPUT

(define (input-keyboard state key)
  ; mode-loop : key x state -> state
  ; determines the effect of key based on mode
  (define-from state stx mode)
  
  ; print debugging information
  #;(displayln `(mode: ,mode  key: ,key))
  #;(displayln `(projected: ,(project stx)))
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
    stx layout-settings)
  (match-define (list _ image-out)
    ; second here skips the top (diamond) affo
    ; todo: make this less hacky by going fs
    (fructure-layout (second stx) layout-settings))
  image-out)


; -------------------------------------------------
; FRUCTURE CORE

(big-bang initial-state
  ; MY LOVE FOR YOU IS LIKE A TRUCK
  [name 'fructure]
  [on-key input-keyboard]
  [to-draw output 800 400]
  #;[display-mode 'fullscreen]
  #;[record? "gif-recordings"])

