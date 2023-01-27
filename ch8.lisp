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

(do-primes p 0 19)
    (format t "~d " p)

