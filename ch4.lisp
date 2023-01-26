;; obvious from last chapter (*db*) but
; global variables are given names that start and end with *.
; constants are given names starting and ending in +
; some programmers will name particularly low-level functions with names that start with % or even %%
; keyword symbols--symbols whose names start with :

;; "special operators" like (if) can do things that user-defined functions cannot do. there's 25 if-like special operators in lisp

;; ' is syntactic sugar for (quote)
; [1]> (quote (1 2))
; (1 2)
; [2]> (quote (+ 1 2))
; (+ 1 2)
; [3]> 
; '(+ 1 2)

;; variable binding and propagation from inner to outer
; [4]> (let ((x 10)) x)
; 10
; [5]> (let ('(x 10)) x)

; *** - EVAL: undefined function X
; The following restarts are available:
; USE-VALUE      :R1      Input a value to be used instead of (FDEFINITION 'X).

;; the symbol NIL is the only false value, and everything else is true.

