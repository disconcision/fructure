#lang racket

(define abcdef 6)
(define dfdsf 6)
(provide ns)
(define-namespace-anchor a)
(define ns (namespace-anchor->namespace a))
