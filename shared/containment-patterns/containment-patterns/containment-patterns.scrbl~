#lang scribble/doc

@(require (for-label memoize))
@begin[(require scribble/manual)
       (require scribble/eval)
       (require scribble/basic)
       ]

@(define the-eval
   (let ([the-eval (make-base-eval)])
     (the-eval '(require "main.rkt"))
     the-eval))

@title[#:tag "top"]{Memoize: Lightweight Dynamic Programming}

by Dave Herman (@tt{dherman at ccs dot neu dot edu})

Memoize is a simple library for doing dynamic programming in Scheme with a minimum
of effort. The library provides drop-in replacement forms for defining Scheme
functions that cache their results.

@table-of-contents[]

@defmodule[memoize]{}

@section[#:tag "intro"]{Example: Fibonacci}

A typical example of a dynamic programming problem is computing the Fibonacci numbers,
whose simplest implementation involves a heavy amount of duplicated computation. By
simply defining the function with @scheme[define/memo], previously computed answers
are cached, avoiding the duplicated computation.

@defexamples[#:eval the-eval
             (define (fib n)
               (if (<= n 1) 1 (+ (fib (- n 1)) (fib (- n 2)))))
             (time (fib 35))
             (define/memo (fib n)
               (if (<= n 1) 1 (+ (fib (- n 1)) (fib (- n 2)))))
             (time (fib 35))]

@section[#:tag "forms"]{Forms}

Just like the function definition forms in PLT Scheme, the formals list of a memoized function
may be a single identifier, a proper list of identifiers, or an improper list of identifiers.

@schemegrammar[formals id () (id . formals)]

@subsection[#:tag "definitions"]{Definition Forms}

@defform[#:id define/memo (define/memo (name . formals) body ...)]{
Defines a memoized function @scheme[name] with formal arguments @scheme[formals] and function body forms @scheme[body ...].
Inputs are cached in a hash table and looked up with @scheme[eq?].}

@defform[#:id define/memo* (define/memo* (name . formals) body ...)]{
Like @scheme[define/memo], but uses @scheme[equal?] to look up values in the cache.}

@subsection[#:tag "expressions"]{Expression Forms}

@defform[#:id memo-lambda (memo-lambda formals body ...)]{
An anonymous memoized function with formal arguments @scheme[formals] and function body forms @scheme[body ...].
Inputs are cached in a hash table and looked up with @scheme[eq?].}

@defform[#:id memo-lambda* (memo-lambda* formals body ...)]{
Like @scheme[memo-lambda], but uses @scheme[equal?] to look up values in the cache.}
