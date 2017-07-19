#lang racket

(require racket/draw)
(require racket/gui)


(define target (make-bitmap 300 300))
(define dc (new bitmap-dc% [bitmap target]))

(send dc set-brush "green" 'transparent)
(send dc set-pen "green" 1 'solid)



(define (make source [parent-tlx 0] [parent-tly 0])
  (if (and (list? source) (not (empty? source)))
      (let* ([tlx (+ parent-tlx 4)]
             [tly (+ parent-tly 4)]
             [kids (let* ([first-kid (make (first source) tlx tly)]
                          [new-tlx (+ 4 (third first-kid))]
                          [new-tly (second first-kid)])
                     (make (rest source) new-tlx new-tly))
              #;(map (λ (x) (make x tlx tly)) source)]
             #;[last-kid (last kids)]
             [brx (third kids)]
             [bry (fourth kids)])
        #;(send dc draw-rectangle tlx tly (- brx tlx) (- bry tly))
        `(,tlx ,tly ,brx ,bry)
        #;kids)
      (let* ([tlx (+ parent-tlx 0)]
             [tly (+ parent-tly 0)])
        (send dc draw-rectangle tlx tly 10 10)
        `(,tlx ,tly ,(+ tlx 0) ,(+ tly 0)))))



(define (make-row source x y)
  (if (and (list? source) (not (empty? source)))
      (let* ([first-child (make-row (first source) x y)]
            [new-x (third first-child)]
            [new-y y]
            [rest-childs (make-row (rest source) new-x new-y)])
        `( ,@rest-childs))
      (let ([a 0])
        (send dc draw-rectangle x y 10 10)
        `(,x ,y ,(+ x 10) ,(+ y 10)))))




(make-row '(()()()()) 0 0)

(make-object image-snip% target)