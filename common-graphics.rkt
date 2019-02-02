#lang racket

(require 2htdp/image)

(provide rounded-rectangle
         rounded-rectangle-outline
         rounded-backing
         rounded-profile
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


(define (4-point-row p n offset
                     ltp ltn
                     init-h final-h r t)
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
   ; these will become pulled points
   (list t 0 x1 y1 t (* ltp -45))
   (list t (* ltp +45) x2 y2 t 0)
   (list t 0 x3 y3 t (* ltn -45))
   (list t (* ltn +45) x4 y4 t 0)))


(define (profile-row p n offset
                     ltp ltn
                     init-h final-h r t)
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

  ; x1 y1 a1 pl x2 y2 a2 p2
  (list
   (list x1 y1 (cond [(= 1 (* (+ p) ltp)) 0] [(= -1 (* (+ p) ltp)) -180] [else -90]) t x2 y2 (* (+ p) -90) t)
   (list x2 y2 -90 0 x3 y3  90 0)
   (list x3 y3 (* (+ p) -90) t x4 y4 (cond [(= 1 (* (+ p) ltn)) -180] [(= -1 (* (+ p) ltn)) 0] [else 90]) t)))

(define (calc-rows-profile p init num-rows total-h
                           augged-rows header-exception? r t)
  (define augmented-rows
    (augment p init augged-rows))
  ; header-exception makes the first row curve in,
  ; regardless of whether the next is longer
  (define new-augmented-rows
    (if header-exception?
        (match augmented-rows
          [`((,w ,h ,ltp ,ltn) ,xs ...)
           `((,w ,h ,ltp ,(abs ltn)) ,@xs)])
        augmented-rows))
  (for/fold ([acc '()]
             [n num-rows]
             [h total-h])
            ([row new-augmented-rows])
    (match-define (list c-w c-h ltp ltn) row)
    (values `(,@acc ,@(profile-row p n c-w ltp ltn h
                                   (+ h (* p c-h)) r t))
            (+ p n)
            (+ h (* p c-h)))))

(define (calc-rows p init num-rows total-h
                   augged-rows header-exception? r t)
  (define augmented-rows
    (augment p init augged-rows))
  ; header-exception makes the first row curve in,
  ; regardless of whether the next is longer
  (define new-augmented-rows
    (if header-exception?
        (match augmented-rows
          [`((,w ,h ,ltp ,ltn) ,xs ...)
           `((,w ,h ,ltp ,(abs ltn)) ,@xs)])
        augmented-rows))
  (for/fold ([acc '()]
             [n num-rows]
             [h total-h])
            ([row new-augmented-rows])
    (match-define (list c-w c-h ltp ltn) row)
    (values `(,@acc ,@(4-point-row p n c-w ltp ltn h
                                   (+ h (* p c-h)) r t))
            (+ p n)
            (+ h (* p c-h)))))

(define (rounded-backing init-source-right-profile
                         init-source-left-profile
                         r my-color mode outline-w
                         header-exception?)
  ; ATTENTION:
  ; the way this is currently implemented,
  ; both profiles must have the same length
  ; and same total height
  
  ; todo: enforce precondition:
  ; radius is no greater than 1/2 min row height/width
  (define source-right-profile
    (for/list ([row init-source-right-profile])
      (match row
        [`(,x ,y) `(,(- x 0) ,(- y (* 2 r)))])))
  (define source-left-profile
    (for/list ([row init-source-left-profile])
      (match row
        [`(,x ,y) `(,(- x 0) ,(- y (* 2 r)))])))

  (define t 0.39) ; empirical roundness parameter
  
  (define-values (right-profile num-rows total-h)
    (calc-rows +1 0 0 0
               source-right-profile header-exception? r t))
  (define-values (left-profile _ __)
    (calc-rows -1 +inf.f num-rows total-h
               (reverse source-left-profile) #f r t))

  
  #;(println (draw-profile left-profile))
  #;(println (draw-profile right-profile))

  (polygon (append
            (map (curry apply pulled-point) right-profile)
            (map (curry apply pulled-point) left-profile))
           mode (if (not (equal? mode "outline")) my-color
                    (pen my-color outline-w "solid" "round" "bevel"))))

(define (rounded-profile init-source-right-profile
                         init-source-left-profile
                         r my-color left?
                         header-exception?)
  ; ATTENTION:
  ; the way this is currently implemented,
  ; both profiles must have the same length
  ; and same total height
  
  ; todo: enforce precondition:
  ; radius is no greater than 1/2 min row height/width
  (define source-right-profile
    (for/list ([row init-source-right-profile])
      (match row
        [`(,x ,y) `(,(- x 0) ,(- y (* 2 r)))])))
  (define source-left-profile
    (for/list ([row init-source-left-profile])
      (match row
        [`(,x ,y) `(,(- x 0) ,(- y (* 2 r)))])))

  (define t 0.39) ; empirical roundness parameter
  
  (define-values (right-profile num-rows total-h)
    (calc-rows-profile +1 0 0 0
               source-right-profile header-exception? r t))
  (define-values (left-profile _ __)
    (calc-rows-profile -1 +inf.f num-rows total-h
               (reverse source-left-profile) #f r t))

  (define (correct rp)
    (if (< (length rp) 4) rp
        (match rp
          [`(,a ,b
                ,(list x1 y1 a1 pl x2 y2 a2 p2)
                ,(list x3 y3 a3 p3 x4 y4 a4 p4) . ,xs)
           `(,a ,b
                ,(list x1 y1 a1 pl x2 y2 a2 p2)
                ,(list x2 y2 0 0 x3 y3 0 0)
                . ,(correct `(,(list x3 y3 a3 p3 x4 y4 a4 p4) . ,xs)))])))

  #;(pretty-print (correct left-profile))
  (for/fold ([acc empty-image])
            ([row (correct (if left? left-profile right-profile))])
    (match row
      [(list x1 y1 a1 pl x2 y2 a2 p2)
       (add-curve acc  x1 y1 a1 pl x2 y2 a2 p2 my-color)])))

; test
#; (overlay (rounded-backing
             (list '(300 50)
                   '(100 50)
                   '(400 50)
                   '(400 50)
                   '(300 50)
                   '(150 50)
                   '(250 50))
             (list '(0 50)
                   '(0 50)
                   '(300 50)
                   '(100 50)
                   '(150 50)
                   '(150 50)
                   '(150 50))
             10 "red" "outline" 12)
            (rectangle 600 600 "solid" (color 0 0 0 0)))

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






#;(define (rounded-profile ls color)
  (pretty-print ls)
  (define (destroy-list ls)
    (match-define `(,x ,ys ... ,z) ls)
    (define a2 (for/list ([x ys]) (list x x)))
    (define a3 (apply append a2))
    (define a4 `(,x ,@a3 ,z))
    (define (make-into-pairs ls)
      (if (empty? ls) ls
          `((,(first ls) ,(second ls))
            . ,(make-into-pairs (rest (rest ls))))))
    (make-into-pairs a4))
  (pretty-print (destroy-list ls))
  (for/fold ([acc empty-image])
            ([pair (destroy-list ls)])
    (match pair
      [`(,(list _     _     x1 y1 pull1 angle1)
         ,(list pull2 angle2 x2 y2 _ _))
       (add-curve acc
                  ; 90 is total hack, and it still looks wrong
                  x1 y1 (+ 90 angle1) (if (zero? pull1) 0 pull1)	 	 	 	 
                  x2 y2 (+ 90 angle2) (if (zero? pull2) 0 pull2)	 	 	 	 
                  color)])))

#;(rounded-profile '((0 0 109 0 0.39 -45) (0.39 45 119 10 0 0) (0 0 119 31 0.39 -45) (0.39 45 109 41 0 0) (0 0 313 41 0.39 -45) (0.39 45 323 51 0 0) (0 0 323 72 0.39 0) (0.39 0 323 82 0 0) (0 0 323 82 0.39 0) (0.39 0 323 92 0 0) (0 0 323 113 0.39 -45) (0.39 45 313 123 0 0)) "red")

