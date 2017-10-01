#lang racket

(provide L1
         sort-name?
         terminal-name?
         form-name?
         affo-name?)

; sorts
(define L1-sort-names '(expr
                        name
                        hole
                        free))


; terminal sorts
(define L1-terminal-names '(name
                            name-new
                            name-ref
                            literal))


; form identifiers (reserved words)
(define L1-form-names '(if cond begin define let lambda
                           local
                           send new
                           env kit meta))


; grammar
(define L1 '(#; (free (|| (free ...)))
             (def  (|| (define (name name ...) expr ...)
                       (define name expr)))
             (expr (|| (if expr expr expr)
                       (cond [expr expr] ...)
                       (begin expr ...)
                       (define (name name ...) expr ...)
                       (define name expr)
                       (let ([name expr] ...) expr ...)
                       (lambda (name ...) expr ...)
                       (local (expr ...) expr ...)
                       (send expr expr expr ...)
                       (new expr [name expr] ...)
                       (env expr ...)
                       (kit expr ...)
                       (meta expr ...)
                       (expr expr ...)))))



; snaffos: syntactic affordances for interface metalanguage
(define L1-affo-names '(▹ selector
                          ▹▹ subselector
                          s▹ search-selector
                          c▹ command-selector
                          c▹▹ command-subselector
                          ⋈ fruct-unquote))

; membership predicates
(define-values (sort-name?
                terminal-name?
                form-name?
                affo-name?) (values (λ (x) (member x L1-sort-names))
                                    (λ (x) (member x L1-terminal-names))
                                    (λ (x) (member x L1-form-names))
                                    (λ (x) (member x L1-affo-names))))