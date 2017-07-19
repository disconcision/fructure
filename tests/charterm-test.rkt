#lang racket

(require charterm)

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
