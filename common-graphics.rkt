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









(define (augment polarity init-width source-rows)
  (define (3> a b)
    (cond
      [(> a b) polarity]
      [(= a b) 0]
      [else (- polarity)]))
  (define-values (rows-with-ltp _)
    (for/fold ([acc '()]
               [prev-w init-width])
              ([r source-rows])
      (match-define (list w h) r)
      (values `(,@acc ,(list w h (3> w prev-w))) w)))
  (define-values (rows-with-ltp-ltn __)
    (for/fold ([acc '()]
               [prev-w init-width])
              ([r (reverse rows-with-ltp)])
      (match-define (list w h ltp) r)
      (values `(,@acc ,(list w h ltp (3> w prev-w))) w)))
  (reverse rows-with-ltp-ltn))


(define (rounded-backing source-right-profile
                         source-left-profile
                         r my-color mode outline-w)
  ; todo: enforce precondition:
  ; radius is no greater than 1/2 min row height/width

  (define t 0.39) ; empirical roundness parameter
  
  (define (4-point-row p n offset
                       ltp ltn
                       init-h final-h)
    ; p +1 for top-down, -1 for bottom-up
    ; n-th row
    ; offset - x from right
    ; ltp - longer than previous
    ; ltn - longer than next
    ; init-h - initial height of row
    ; final-h - final height of row
    
    (define y1 (+ (* 2 n r) init-h))
    (define y2 (+ (* 2 n r) (* p r) init-h))
    (define y3 (+ (* 2 n r) (* p r) final-h))
    (define y4 (+ (* 2 n r) (* p (* 2 r)) final-h))

    (define x1 (+ offset (* (- p) ltp r)))
    (define x2 offset)  
    (define x3 offset)
    (define x4 (+ offset (* (- p) ltn r)))

    (list
     (make-pulled-point 0 0 x1 y1 t (* ltp -45))
     (make-pulled-point t (* ltp +45) x2 y2 0 0)
     (make-pulled-point 0 0 x3 y3 t (* ltn -45))
     (make-pulled-point t (* ltn +45) x4 y4 0 0)))
  
  (define (calc-rows p init num-rows total-h
                     augged-rows)
    (for/fold ([acc '()]
               [n num-rows]
               [h total-h])
              ([r (augment p init augged-rows)])
      (match-define (list c-w c-h ltp ltn) r)
      (values `(,@acc ,(4-point-row p n c-w ltp ltn h
                                    (+ h (* p c-h))))
              (+ p n)
              (+ h (* p c-h)))))

  (define-values (right-profile num-rows total-h)
    (calc-rows +1 0 0 0
               source-right-profile))
  (define-values (left-profile _ __)
    (calc-rows -1 +inf.f num-rows total-h
               (reverse source-left-profile)))

  (polygon (append
            (apply append right-profile)
            (apply append left-profile))
           mode (if (zero? outline-w) my-color
                    (pen my-color outline-w "solid" "round" "bevel"))))

; test
#; (rounded-backing (list '(300 20)
                          '(100 20)
                          '(400 20)
                          '(400 20)
                          '(300 20)
                          '(150 20)
                          '(250 20))
                    (list '(0 20)
                          '(0 20)
                          '(300 20)
                          '(100 20)
                          '(150 20)
                          '(150 20)
                          '(150 20))
                    10 "red" "outline")

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



