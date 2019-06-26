#lang racket


(define ns (dynamic-require "test-source-file.rkt" 'ns))

(define in-scope-symbols (namespace-mapped-symbols ns))

#; (member 'dfdsf in-scope-symbols)

(length in-scope-symbols)

(index-of in-scope-symbols 'dfdsf)