#lang racket

(provide L1
         sort-name?
         terminal-name?
         form-name?
         affo-name?)

(define L1 '((free (|| (free ...)))
             (def  (|| (define (name name ...) expr ...)
                       (define name expr)))
             (expr (|| (if expr expr expr)
                       (begin expr ...)
                       (define (name name ...) expr ...)
                       (define name expr)
                       (let ([name expr] ...) expr ...)
                       (lambda (name ...) expr ...)
                       (send expr expr expr ...)
                       (new expr [name expr] ...)
                       (env expr ...)
                       (kit expr ...)
                       (meta expr ...)
                       (expr expr ...)))))

(define L1-form-names '(if begin define let lambda send new env kit meta))
(define L1-terminal-names '(name name-new name-ref literal))
(define L1-sort-names '(expr name hole free))
(define L1-affo-names '(▹ selector ▹▹ search-selector c▹ command-selector c▹▹ command-sub-selector :))

(define-values (sort-name?
                terminal-name?
                form-name?
                affo-name?) (values (λ (x) (member x L1-sort-names))
                                    (λ (x) (member x L1-terminal-names))
                                    (λ (x) (member x L1-form-names))
                                    (λ (x) (member x L1-affo-names))))