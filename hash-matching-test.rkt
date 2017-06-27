#lang racket

(match #hash((gh . 1) ("c" . 2) ("b" . 2) )
    [(hash-table ("b" b) ('gh a)) (list b a)])