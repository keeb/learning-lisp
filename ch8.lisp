; MACROS

;; The key to understanding macros is to be quite clear 
;;about the distinction between the code that generates code (macros)
;; and the code that eventually makes up the program (everything else).

;; (defmacro name (parameter*)
;;   "Optional documentation string."
;;   body-form*)

(defun primep (number)
    (when (> number 1)
        (loop for fac from 2 to (isqrt number) never (zerop (mod number fac)))))

(defun next-prime (number)
    (loop for n from number when (primep n) return n))


;; we want to make this work, 
; i.e, give me all primes between 0 and 19

(defmacro do-primes (var-and-range &rest body)
    (let ((var (first var-and-range))
        (start (second var-and-range))
        (end (third var-and-range)))
        `(do ((, var (next-prime ,start) (next-prime (1+, var))))
            ((> ,var ,end))
            ,@body)))

(do-primes p 0 19)
    (format t "~d " p)

;; doing this with (do)

(do ((p (next-prime 0) (next-prime (1+ p))))
    ((> p 19))
    (format t "~d " p))

;; a shorter version of (do-primes) macro

(defmacro do-primes ((var start end) &body body)
    `(do ((,var (next-prime ,start) (next-prime (1+ ,var)))
        (ending-value ,end))
        ((> ,var ending-value))
        ,@body))

;; gensym can be used to generate unique symbols in macros so that there's no collision between the name of the 
;; parameter/variable and the name of the variable in the body of the macro

; here's a gensym version of the do-primes macro
(defmacro do-primes ((var start end) &body body)
    (let ((ending-value (gensym)))
        `(do ((,var (next-prime ,start) (next-prime (1+ ,var)))
            (,ending-value ,end))
            ((> ,var ,ending-value))
            ,@body)))

;; Break 15 [17]> (macroexpand-1 '(do-primes (ending-value 0 (random 100))))
; (DO ((ENDING-VALUE (NEXT-PRIME 0) (NEXT-PRIME (1+ ENDING-VALUE))) (#:G3141 (RANDOM 100))) ((> ENDING-VALUE #:G3141))) ;
; T

; the real form of creating a macro if possibles takes the following form...
; (defmacro name ((var start end) &body body)
;     (let ((some-var (gensym))) 
;         `(some-form ,var ,start ,end ,some-var ,@body)))

