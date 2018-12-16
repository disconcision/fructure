#lang racket

(require 2htdp/image)

(provide rounded-rectangle
         rounded-rectangle-outline
         rounded-backing
         beside* above* beside/align* above/align*
         invisible
         per-color-linear-dodge-tint)


(define invisible (color 0 0 0 0))


(define (per-color-linear-dodge-tint tint-color opacity)
  ; tints a color using linear dodge photoshop formula
  (match-define (color r-tint g-tint b-tint _) tint-color)
  (define (per-channel-transformer x x-tint)
    (inexact->exact
     (round (+ (* opacity x)
               (* (- 1 opacity) (min 255 (+ x x-tint)))))))
  (match-lambda
    [(color r g b a)
     (color (per-channel-transformer r r-tint)
            (per-channel-transformer g g-tint)
            (per-channel-transformer b b-tint)
            a)]))



(define (rounded-backing source-rows r my-color mode)
  ; enforce precondition:
  ; radius is no greater than 1/2 min row height/width
  ; this is bugged right now; radius is getting added to height
  #;(define r (apply min init-r
                   (map (λ (x) (inexact->exact (round (* 1/2 x))))
                        (apply append source-rows))))
  (define t 0.39)
  (define (corner a c sl b d h rot)
    (list (make-pulled-point 0 0
                             (+ (* a r) sl) (+ (* b r) h)
                             t rot)
          (make-pulled-point t (- rot)
                             (+ (* c r) sl) (+ (* d r) h)
                             0 0)))
  (define (row n sl ltp ltn init-h final-h)
    ; ltp - longer than previous
    ; ltn - longer than next
    (append
     (corner (- ltp) 0 sl
             (+ 0 (* 2 n)) (+ 1 (* 2 n)) init-h
             (* ltp -45))
     (corner 0 (- ltn) sl
             (+ 1 (* 2 n)) (+ 2 (* 2 n)) final-h
             (* ltn -45))))
  (define (left-side num-rows total-height)
    (append
     (corner 1 0 0 (* 2 num-rows) (+ -1 (* 2 num-rows)) total-height -45)
     (corner 0 1 0 1 0 0 -45)))
  (define (3> a b)
    (cond
      [(> a b) 1]
      [(= a b) 0]
      [else -1]))
  (define-values (rows-with-ltp _)
    (for/fold ([acc '()]
               [prev-w 0])
              ([r source-rows])
      (match-define (list w h) r)
      (values `(,@acc ,(list w h (3> w prev-w))) w)))
  (define-values (rows-with-ltp-ltn __)
    (for/fold ([acc '()]
               [prev-w 0])
              ([r (reverse rows-with-ltp)])
      (match-define (list w h ltp) r)
      (values `(,@acc ,(list w h ltp (3> w prev-w))) w)))
  (define rows-with-both-augs
    (reverse rows-with-ltp-ltn))
  (define-values (rows num-rows total-h)
    (for/fold ([acc '()]
               [n 0]
               [h 0])
              ([r rows-with-both-augs])
      (match-define (list c-w c-h ltp ltn) r)
      (values `(,@acc ,(row n c-w ltp ltn h (+ h c-h)))
              (add1 n)
              (+ h c-h))))
  (polygon (append
            (apply append rows)
            (left-side num-rows total-h))
           mode
           my-color))


(define (rounded-rectangle width height init-r my-color)
  (rounded-rectangle-internal width height init-r "solid" my-color))

(define (rounded-rectangle-outline width height init-r my-color)
  (rounded-rectangle-internal width height init-r "outline" my-color))

(define (rounded-rectangle-internal width height init-r mode my-color)
  (define r (if (width . < . (* 2 init-r))
                (inexact->exact (round (/ width 2))) ; note possible syntax issue
                (inexact->exact (round init-r))))
  (define sl (max 0 (- width (* 2 r))))
  (define sh (max 0 (- height (* 2 r))))
  (define t 0.39) ; magic tightness constant
  ; this constant is visually determined via the following test code
  #; (local [(define t 0.39) ; both .38 and .4 show more slack
             (define r 120)
             (define l 240)]
       (beside
        (overlay
         (rounded-rectangle-2 l l r "solid" "red" t)
         (rounded-rectangle l l r "white"))
        (overlay
         (rounded-rectangle l l r "red")
         (rounded-rectangle-2 l l r "solid" "white" t))))
  (polygon (list (make-pulled-point 0 0 0 r t -45)
                 (make-pulled-point t 45 r 0 0 0)
                 (make-pulled-point 0 0 (+ r sl) 0 t -45)
                 (make-pulled-point t 45 (+ r r sl) r 0 0)
                 (make-pulled-point 0 0 (+ r r sl) (+ r sh) t -45)
                 (make-pulled-point t 45 (+ r sl) (+ r r sh) 0 0)
                 (make-pulled-point 0 0 r (+ r r sh) t -45)
                 (make-pulled-point t 45 0 (+ r sh) t 0))
           mode
           my-color))


(define (beside* . ls)
  (match (length ls)
    [0 empty-image]
    [1 (first ls)]
    [_ (apply beside ls)]))


(define (above* . ls)
  (match (length ls)
    [0 empty-image]
    [1 (first ls)]
    [_ (apply above ls)]))


(define (beside/align* alignment . ls)
  (match (length ls)
    [0 empty-image]
    [1 (first ls)]
    [_ (apply beside/align alignment ls)]))


(define (above/align* alignment . ls)
  (match (length ls)
    [0 empty-image]
    [1 (first ls)]
    [_ (apply above/align alignment ls)]))



