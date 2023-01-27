(if (> 2 3) "Yup" "Nope") ; Nope
(if (> 2 3) "Yup") ; nil
(if (> 3 2) "Yup" "Nope") ; Yup

; if is kind of annoying because of the way lisp works, 
; the `then` and `else` clauses are limited to single statements
; so what happens when you want to do N things inside of the THEN clause

; PROGN

(if (> 3 2)
    (progn
        (print "Yup")
        (print "Second Yup")))

; so progn is a expanded version, but you should use macros instead.

;; Break 11 [16]> (if (> 3 2)
;;     (progn
;;         (print "Yup")
;;         (print "Second Yup")))

;; "Yup"
;; "Second Yup"
;; "Second Yup"

; btw why does this print twice?

; enter when macro, this is built in
(when (> 3 2) (print "Yup")(print "Second Yup"))
;; "Yup"
;; "Second Yup"
;; "Second Yup"

; btw why does this print twice?

;; so how would we make our own `when` macro?

(defmacro when2 (condition &rest body)
    `(if ,condition (progn ,@body)))

; this uses the same concepts learned in ch3,
; &rest takes everything else
; ` takes the literal if expression
; ,condition takes the evaluated value
; @body - had to check notes, returns the evaluted *result* of execution not the statement itself.

(when2 (> 3 2) "Yup") ; Yup

(defmacro unless2 (condition &rest body)
    `(if (not ,condition) (progn ,@body)))

(unless2 (> 2 3) "Yup") ; Yup

;; cond
; the switch of lisp, multi branch conditionals. if x == 10; elif x == 20; elif ..

;; (cond
;;   (test-1 form*)
;;       .
;;       .
;;       .
;;   (test-N form*))

; i.e,
;; (cond (a (do-x))
;;       (b (do-y))
;;       (t (do-z)))

; NOT AND OR, same as always

;; (not nil)             ==> T
;; (not (= 1 1))         ==> NIL
;; (and (= 1 2) (= 3 3)) ==> NIL
;; (or (= 1 2) (= 3 3))  ==> T

; LOOPS

;; some convenience stuff is here..

(dolist (x '(1 2 3))  ; (x '(1 2 3)) is evaluated once, 
; and then the body is executed for each computed item in the list
    (print x)
    (if (not (evenp x)) (print "odd!")))

; returning early, aka break in python, is simple

(dolist (x '(1 2 3))
    (print x)
    (if (evenp x) (return))
    (if (not (evenp x)) (print "odd!")))

; dotimes

(dotimes (i 4) (print i))

; nested

(dotimes (x 20)
  (dotimes (y 20)
    (format t "~3d " (* (1+ x) (1+ y))))
  (format t "~%"))

; but all of these are macros for the basic (do)

;; (do (variable-definition*)
;;     (end-test-form result-form*)
;;   statement*)

; variable definition contains the item which will be in scope of the rest, i.e, for i=0 .. i is in scope


; fibonacci sequence, get N fib number
; i.e, the sum of the current with the previous

(do ((n 0 (1+ n)) ; n is bound, n+1 each increment
    (cur 0 next) 
    (next 1 (+ cur next)))
    (( = 10 n) cur )) ; loop 10 times

; non-idiomatic do whereby the result-form has been omitted.

; print sequence 0-3
(do ((i 0 (1+ i)))
    ((>= i 4))
  (print i))


;; TODO: make this work

;; (do ()
;;     ((> (get-universal-time) *some-future-date*))
;;   (format t "Waiting~%")
;;   (sleep 60)) 

; LOOP

;; (loop
;;    body-form*)

;; (loop
;;   (when (> (get-universal-time) *some-future-date*)
;;     (return))
;;   (format t "Waiting~%")
;;   (sleep 60))

;; loop is less idiomatic lisp because it has less ()'s

do ((nums nil) (i 1 (1+ i)))
    ((> i 10) (nreverse nums))
  (push i nums)) ==> (1 2 3 4 5 6 7 8 9 10)

; vs

;; special symbols for (loop) ..
; across, and, below, collecting, counting, finally, for, from, summing, then, to


(loop for i from 1 to 10 collecting i)
(loop for x from 1 to 10 summing (expt x 2))

; vowels in a str
(loop for x across "the quick brown fox jumps over the lazy dog"
      counting (find x "aeiou"))

; fib in loop

(loop for i below 10
      and a = 0 then b
      and b = 1 then (+ b a)
      finally (return  a))
