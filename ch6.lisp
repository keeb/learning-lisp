;; variable scope

(defun foo (x)
  (format t "Parameter: ~a~%" x)      ; |<------ x is argument 
  (let ((x 2))                        ; |
    (format t "Outer LET: ~a~%" x)    ; | |<---- x is 2
    (let ((x 3))                      ; | |
      (format t "Inner LET: ~a~%" x)) ; | | |<-- x is 3
    (format t "Outer LET: ~a~%" x))   ; | |
  (format t "Parameter: ~a~%" x))     ; |


(dotimes (x 10) (format t "x" x))
; 10 x's

;; can use something like pointers, with let*, where x is propagated to (y's scope)

(let* ((x 10)
       (y (+ x 10)))
  (list x y))
; (10 20)


; (let ((x 10)
    ;   (y (+ x 10)))
;   (list x y))
; *** - LET: variable X has no value


;; CLOSURE time

; The key thing to understand about closures is that it's the binding, not the value of the variable, that's captured.

(defun countit () (let ((count 0)) #'(lambda () (setf count (1+ count)))))

(defparameter *fn* countit)

; need to figure out how to get this to work vs.
(defparameter *fn* (let ((count 0)) #'(lambda () (setf count (1+ count)))))
; which works when used with (funcall *fn*)

;; Break 4 [9]> (defun countit () '(let ((count 0)) #'(lambda () (setf count (1+ count)))))
;; COUNTIT
;; Break 4 [9]> (defparameter *fn* 'countit)
;; *FN*
;; Break 4 [9]> (funcall *fn*)
;; (LET ((COUNT 0)) #'(LAMBDA NIL (SETF COUNT (1+ COUNT))))

; how do i use this in practice? this returns 3 options for count, inc, dec and return
(let ((count 0))
  (list 
    #'(lambda () (incf count))
    #'(lambda () (decf count))
    #'(lambda () count)))

;; non-lexical variables

;; defvar & defparameter
; The difference between the two forms is that DEFPARAMETER always assigns 
; the initial value to the named variable while DEFVAR does so only if the variable is undefined

(defvar *count* 0 
  "Count of widgets made so far")

(defparameter *gap-tolerance* 0.001
  "Tolerance to be allowed in widget gaps.")

(defun increment-widget-count () (incf *count*))

; (let) allows you to override global scoping via `dynamic variables`.
; as a ex

(let ((*standard-output* *some-other-stream*))
  (stuff))

; this will override in the context of (let) what *standard-output* stream actually is
; allowing you to do something like stream stdout to a file.

(defvar *x* 10)
(defun foo () (format t "X: ~d~%" *x*))

;; Break 5 [10]> (foo)
;; X: 10
;; NIL

(let ((*x* 20)) (foo))

;; Break 5 [10]> (let ((*x* 20)) (foo))
;; X: 20
;; NIL
;; Break 5 [10]> (foo)
;; X: 10
;; NIL

(defun bar ()
  (foo)
  (let ((*x* 20)) (foo))
  (foo))

;; Break 5 [10]> (bar)
;; X: 10
;; X: 20
;; X: 10
;; NIL

; lexical vs. global variables *names* are a matter of convention, so as
; to not confuse the two. the *'s themselves are not special in any way

;; Break 7 [12]> (defun foo2 () (format t "Y: ~d~%" y))
;; FOO2
;; Break 7 [12]> (foo2)
;; Y: 20
;; NIL
;; Break 7 [12]>

; defconstant

;; like any other constant. vs. defparameter is a matter of if it is truly 
; constant or not. defconstant values can redefined in certain contexts,
; but if this is happening it should probably be a defparameter.


; variable assignment

(setf x 10)

(defun foo (x) (setf x 10))

(let ((y 20))
  (foo y)
  (print y))

(setf x 1)
(setf y 2)
; same as

(setf x 1 y 2)

; same as

(setf x (setf y (random 10))) ; same random value

; setf assignments can of course be modified, i.e
; (setf x (+ x 1))  


;; modify-macros are macros that modify the value of, i.e, x in the example above


(let ((x 10)) (setf x (+ x 1)))
; 11

; examples of this are (incf) (decf) which, in effect, create local temp variables to ensure
; scope propagate properly
; they also guarantee expressions are evaluated exactly once from left to right
; i am sure this can be broken

