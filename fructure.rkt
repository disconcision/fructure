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


; -------------------------------------------------
; OUTPUT & DISPLAY SETTINGS

(define (output state)
  ; output : state -> image
  (define real-layout-settings
    (hash 'text-size 30
          'max-menu-length 4
          'max-menu-length-chars 1
          'popout-transform? #t
          'popout-menu? #t
          'custom-menu-selector? #t
          'length-conditional-layout? #t
          'length-conditional-cutoff 8
          'dodge-enabled? #t
          'implicit-forms '(ref app)
          'selected-atom-color "white"
          'menu-bkg-color (color 112 112 112)
          'form-color (color 0 130 214)
          'literal-color (color 255 131 50)
          'grey-one (color 230 230 230)
          'grey-two (color 215 215 215)
          'pattern-grey-one (color 84 84 84)
          'identifier-color "black"
          'selected-color (color 230 0 0)
          'hole-color (color 0 180 140)
          'transform-arrow-color (color 255 255 255)
          'bkg-color (color 0 47 54)
          'pattern-bkg-color (color 230 230 230)
          'pattern-grey-one (color 76 76 76)
          'pattern-grey-two (color 110 110 110)))
  (define-from state stx)
  (match-define (list new-fruct image-out)
    (fructure-layout (second stx) real-layout-settings))
  image-out)


; -------------------------------------------------
; SEED

(define initial-state
  ; we begin in navigation mode,
  ; with a single selected hole of sort 'expression
  ; transforms: undo history; currently broken
  ; messages: a messages log, currently disused
  (hash 'stx (fruct-augment initial-stx)
        'mode 'nav
        'transforms '()
        'messages '("hello world")))


; -------------------------------------------------
; INPUT LOOP / MODAL DISPATCH

(define (input-keyboard state key)
  ; mode-loop : key x state -> state
  ; determines the effect of key based on mode
  (define-from state stx mode)
  
  ; print debugging information
  (displayln `(mode: ,mode  key: ,key))
  #;(displayln `(projected: ,(project stx)))
  #;(displayln state)

  ; dispatch based on current mode
  (define new-state
    (match mode
      ['menu (mode:transform key state)]
      ['nav  (mode:navigate key state)]))
  
  ; augment syntax with attributes
  (transform-in new-state
                [stx fruct-augment]))


; -------------------------------------------------
; FRUCTURE CORE

(big-bang initial-state
  ; bug if true (c)
  ; MY LOVE FOR YOU IS LIKE A TRUCK
  [name 'fructure]
  [on-key input-keyboard]
  [to-draw output 800 400]
  #;[display-mode 'fullscreen]
  #;[record? "gif-recordings"])

