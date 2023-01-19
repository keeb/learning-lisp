;; optional parameters to a function/procedure definition are prepended by & optional

; [1]> (defun foo (a b &optional c d) (list a b c d))
; FOO
; [2]> (foo 1 2
; )
; (1 2 NIL NIL)
; [3]> (foo 1 2 3 4)
; (1 2 3 4)
; [4]> 

;; default values can be set as we saw in ch3

; [4]> (defun foo (a b &optional (c 10) d) (list a b c d))
; FOO
; Break 1 [7]> (foo 1 2)
; (1 2 10 NIL)

;; supplied-p lets you understand if the optional parameter was passed or not

; Break 1 [7]> (defun foo (a b &optional (c 10 c-supplied-p) d) (list a b c d c-supplied-p))
; FOO
; Break 1 [7]> (foo 1 2 3)
; (1 2 3 NIL T)
; Break 1 [7]> (foo 1 2)
; (1 2 10 NIL NIL)

;; &rest is how you do N arg function calls

;; &key we learned about in ch3 - kwargs
;; like python def square(x=10, y=10) == (defun square(&key x y))
;; setting/getting the value of the parameter is how (select) and (where) worked using :param


;; of course, all preceding things work in this context as well
; (defun foo (x &optional y &key z) (list x y z))

;; breaking from do/for loops

(defun foo (n)
  (dotimes (i 10)
    (dotimes (j 10)
      (when (> (* i j) n)
        (return-from foo (list i j))))))

;; (funcall ) is a way to call a previously defined function
; (foo 1 2 3) === (funcall #'foo 1 2 3)
; (foo 1 2 3) === (funcall 'foo 1 2 3)

;; this is only useful when you're defining a procedure which takes a procedure as an argument, i.e

(defun plot (fn min max step)
    (loop for i from min to max by step do
        (loop repeat (funcall fn i) do (format t "*"))
        (format t "~%")))


;; apply is weird

; (apply #'plot '(exp 2 3 4))

;; which is better?

; (defun double(x) (* 2 x))
; (plot #'double 0 10 1)

;; or

; (plot #'(lambda (x) (* 2x)) 0 10 1)

