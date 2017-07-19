#lang racket


(define pat (read))
(define-namespace-anchor a)
(define ns (namespace-anchor->namespace a))

(eval `(match '(1) [,pat b]) ns)