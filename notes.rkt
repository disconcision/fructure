#lang racket

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
