#lang racket

(require charterm)
; basic structure editor


; [] denotes a selector
; () is an empty block with border symbols "(" and ")"
; (block) is a block with head symbol "block"
; (block child) is a block with a single child
; worldtree: (root (block) (block (child) ..) (another-block) ..)
; the selector points to a subtree: (root (block) [(block (child) ..)] (another-block) ..)


; data
; main-string <- load from savefile.rkt
; main-string is a string containg " ", "(", ")", [a-z]
; parse main-string into block (alias for list)
; selector: HOW DO I REPRESENT THIS?
; point selector at root block
; ... navigation, editing ...
; press F5 to save+quit
; generate main-string from block
; overwrite main-string -> savefile.rkt


; navigation
; you begin in a formless void: []
; arrow keys navigate blocktree: up:parent, dn:firstchild, lf,rh:cyclechildren


; editing
; press space
;   if selector empty
;      1. create empty block : [] -> ([])
;      2. accept chars
;   if selector around block [(symbol ..)]
;      1. move selector to block head ([symbol] ..)
;      2. accept chars
;   accept chars
;      until:
;         press esc: cancel input : (oldsymbol) -> (newsymb[]) -> [(oldsymbol)]
;         press space: submit chars : (symbol[]) -> goto submit symbol
;       loop:
;         1. read in chars
;         2. optional tabcomplete, autocomplete, incorporating alt-\ keybindings
;   submit symbol
;      if symbol type takes no children
;         if symbol's parent has a next child, select its head
;            (parent .. (symbol[]) (next ..) ..) -> (parent .. (symbol) ([next] ..) ..)  
;         else select symbol's parent
;            (parent .. (symbol[])) -> [(parent .. (symbol))]  
;      if symbol type has default children, populate them, select first child's head
;         (symbol[]) -> (symbol ([type]) (type) ..)
;      if symbol type variadic and selector on last-default/no-default child, create new empty box as child
;         (symbol .. (lastdef[])) -> (symbol .. (lastdef) ([]))
; press esc
;   move selector to root 
; press del
;   delete selected block, move to previous child, else move to parent


; display
; press ctrl : enter style mode
; block style toggles (for individual boxes, eventually box types)
;    f : f<, f> :  box child flow
;    x : x<, x^, x> : box fixity (prefix/infix/postfix)
;    h : hv, hc[color], hk[color], hh[symbol] : head visible/color/bkgcolor/(?chars alias?)
;    b : bv, bc[color], bk[color], bb[symbol symbol] : borders visible/colors/chars : default "(" ")"
;    d : dv, dc[color], dk[color], dd[symbol] dividers visible/colors/char : default " "
; selector toggles
;    t : tv, tc, tk, tt : visible/colors/chars
;    also need: subtree tint colors

(require scribble/blueboxes)
;(fetch-blueboxes-strs '(def ('#%kernel list*)))
;(fetch-blueboxes-strs '(def ('#%kernel build-list)))
;(fetch-blueboxes-strs '(def ('#%kernel map)))
;(fetch-blueboxes-strs '(def ('#%kernel apply)))
;(fetch-blueboxes-strs '(def ('#%kernel cons)))



; name is string representing symbol
; children is list of blocks
(define (block name children)
  (list name children))

(define out-file (open-output-file "frucfile.rkt" #:exists 'can-update))
(write "(a (a) (a (a a)) a)" out-file)
(close-output-port out-file)

(define in-file (open-input-file "frucfile.rkt"))
(define in-string (read-string 11 in-file))
(close-input-port in-file)




; parse : string -> blocktree
; figure out top "(" or "[a-z]"
; if ( get next char
;   if ) return (block "()" '())
;   if [a-z] read chars -> namestring until space or ")" (or "("?)
;     if ) return (block namestring
;     if " " return (block namestring (parse rest-of-string-before-close-bracket)
(define (parse string)
  "lol")

; render : blocktree -> string
(define (render blocktree)
  "lol")




(define world
  (parse in-string))


(struct pos (list-of-child-nums))

(define hand (pos '()))

(define move-up 
  (list))

(define move-dn 
  (list))

(define move-lt 
  (list))

(define move-rt 
  (list))

(define (input-loop display hand)
  (with-charterm
      (charterm-display display)
    (define command (charterm-read-key))
    (cond [(equal? command #\w) (charterm-display "w") (move-up) (input-loop display hand)]
          [(equal? command #\s) (charterm-display "s") (move-dn) (input-loop display hand)]
          [(equal? command #\a) (charterm-display "a") (move-lt) (input-loop display hand)]
          [(equal? command #\d) (charterm-display "d") (move-rt) (input-loop display hand)]
          [else (charterm-display "u wot mate")])))


(input-loop in-string hand)
