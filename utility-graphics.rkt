#lang racket

(require 2htdp/image
         lang/posn)

(provide rounded-rectangle
         rounded-rectangle-new
         beside* beside/align* above/align*
         1px invisible
         linear-dodge-tint)


(define (linear-dodge-tint my-image tint-color opacity)
  (match-define (color r-tint g-tint b-tint _) tint-color)
  (define (per-channel-transformer x x-tint)
    (inexact->exact
     (round (+ (* opacity x)
               (* (- 1 opacity) (min 255 (+ x x-tint)))))))
  (define transformer
    (match-lambda
      [(color r g b a)
       (color (per-channel-transformer r r-tint)
              (per-channel-transformer g g-tint)
              (per-channel-transformer b b-tint)
              a)]))
  (color-list->bitmap (map transformer (image->color-list my-image))
                      (image-width my-image) (image-height my-image)))


(define (rounded-rectangle width height radius my-color)
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