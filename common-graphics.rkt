#lang racket

(require 2htdp/image
         lang/posn)

(provide rounded-rectangle
         rounded-rectangle-outline
         rounded-rectangle-new
         beside* beside/align* above/align*
         1px invisible
         linear-dodge-tint
         linear-dodge-tint-red
         per-color-linear-dodge-tint)


;
(require images/flomap
         racket/flonum
         (only-in racket/draw
                  bitmap%
                  bitmap-dc%))


(define-match-expander *flvector
  (lambda (stx)
    (syntax-case stx ()
      [(*flvector a b c d)
       #'(? flvector?
            (app (λ (x) (vector (flvector-ref x 0)
                                (flvector-ref x 1)
                                (flvector-ref x 2)
                                (flvector-ref x 3)))
                 (vector a b c d)))])))

(define (image->bitmap image)
  (let* ([width (image-width image)]
         [height (image-height image)]
         [bm (make-object bitmap% width height #f #t)]
         [dc (make-object bitmap-dc% bm)])
    #;(send dc clear)
    (send image draw dc 0 0 0 0 width height 0 0 #f)
    bm))

(define (apply-image-fn f image)
  (define image-flomap
    (bitmap->flomap (image->bitmap image)))
  (define new-flomap
    (build-flomap* 4 (image-width image) (image-height image)
                   (λ (x y) (f (flomap-ref* image-flomap x y)))))
  (flomap->bitmap new-flomap))

(define (per-flvector-linear-dodge-tint tint-color opacity)
  (match-define (vector r-tint g-tint b-tint _) tint-color)
  (define (per-channel-transformer x x-tint)
    (+ (* opacity x)
       (* (- 1 opacity) (min 1.0 (+ x x-tint)))))
  (match-lambda
    [(*flvector r g b a)
     (flvector (per-channel-transformer r r-tint)
               (per-channel-transformer g g-tint)
               (per-channel-transformer b b-tint)
               a)]))

(define (linear-dodge-tint-red-per-pixel fv)
  (flvector
   (flvector-ref fv 0)
   (+ (* 0.3 (flvector-ref fv 1))
      (* 0.7 (min 1.0 (+ (flvector-ref fv 1) 0.4))))
   (flvector-ref fv 2)
   (flvector-ref fv 3)))

(define (linear-dodge-tint-red my-image)
  (apply-image-fn linear-dodge-tint-red-per-pixel my-image))



(define (per-color-linear-dodge-tint tint-color opacity)
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

(define (linear-dodge-tint my-image tint-color opacity)
  (color-list->bitmap
   (map (per-color-linear-dodge-tint tint-color opacity)
        (image->color-list my-image))
   (image-width my-image) (image-height my-image)))




(define (rounded-rectangle w h init-radius my-color)
  ; added rounding to try and fix subpixel layout issues
  ; doesn't seem to have done much. todo: check if it does anything
  (define width (inexact->exact (round w)))
  (define height (inexact->exact (round h)))
  (define radius
    (inexact->exact
     (round
      (if (width . < . (* 2 init-radius))
          (/ width 2) ; note possible syntax issue
          init-radius))))
  
  (define pen
    (make-pen my-color (* 2 radius) "solid" "round" "round"))
  (underlay/align
   "middle" "middle"
   ; bounding box
   (rectangle width height "solid" (color 0 0 0 0))
   ; fill in inside
   (rectangle (max 0 (- width (* 2 radius)))
              (max 0 (- height (* 2 radius)))
              "solid" my-color)
   (polygon
    (list (make-posn radius radius)
          (make-posn (- width radius) radius)
          (make-posn (- width radius) (- height radius))
          (make-posn radius (- height radius)))
    "outline" pen)))

; BELOW DOES NOT WORK todo bug
(define (rounded-rectangle-outline w h init-radius my-color)
  ; added rounding to try and fix subpixel layout issues
  ; doesn't seem to have done much. todo: check if it does anything
  (define width (inexact->exact (round w)))
  (define height (inexact->exact (round h)))
  (define radius
    (inexact->exact
     (round
      (if (width . < . (* 2 init-radius))
          (/ width 2) ; note possible syntax issue
          init-radius))))
  
  (define pen
    (make-pen my-color 1 "solid" "round" "round"))
  (underlay/align
   "middle" "middle"
   ; bounding box
   (rectangle width height "solid" (color 0 0 0 0))
   (polygon
    (list (make-posn radius radius)
          (make-posn (- width radius) radius)
          (make-posn (- width radius) (- height radius))
          (make-posn radius (- height radius)))
    "outline" pen)))

(define (rounded-rectangle-new width height radius my-color)
  (define my-radius (inexact->exact (round radius)))
  (define corner
    (crop/align "left" "top" my-radius my-radius
                (circle my-radius "solid" my-color)))
  (define top-bar (rectangle (max 0 (- width (* 2 my-radius)))
                             my-radius  "solid" my-color))
  (define top-side (beside corner top-bar (rotate -90 corner)))
  (above top-side
         (rectangle (image-width top-side)
                    (max 0 (- height (* 2 my-radius)))
                    "solid" my-color)
         (rotate 180 top-side)))


#;(define (bracket-h width my-color corner-radius rect)
    (underlay
     (rounded-rectangle (+ (image-width rect) (* 2 width))
                        (image-height rect)
                        corner-radius my-color)
     rect))


#;(define (bracket-hl width my-color corner-radius rect)
    (underlay/align "right" "middle"
                    (rounded-rectangle (+ (image-width rect) width)
                                       (image-height rect)
                                       corner-radius my-color)
                    rect))


#;(define (bracket-hr width my-color corner-radius rect)
    (underlay/align "left" "middle"
                    (rounded-rectangle (+ (image-width rect) width)
                                       (image-height rect)
                                       corner-radius my-color)
                    rect))


#;(define (bracket-ht height my-color corner-radius rect)
    (underlay/align "middle" "bottom"
                    (rounded-rectangle (image-width rect)
                                       (+ (image-height rect) height)
                                       corner-radius my-color)
                    rect))


#;(define (bracket-hb height my-color corner-radius rect)
    (underlay/align "middle" "top"
                    (rounded-rectangle (image-width rect)
                                       (+ (image-height rect) height)
                                       corner-radius my-color)
                    rect))


(define (beside* . ls)
  (match (length ls)
    [0 empty-image]
    [1 (first ls)]
    [_ (apply beside ls)]))


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


(define 1px
  (rectangle 1 1 "solid" (color 0 0 0 0)))


(define invisible
  (color 0 0 0 0))