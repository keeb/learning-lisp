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


