;;; CHAPTER 9 - Unit test Framework

; in general you want to design in two ways
; one is a function that returns a boolean value - true in the case of success, false in the case of failure.
; the other is to generate a function that has some side effect, in this case we test the value later and return true if the value is correct, false otherwise.

;i.e,

(defun test+- ()
    (and
        (= (+ 1 2 3) 6)
        (= (+ 3 5 2) 10)
        (= (- 3 2 1) 0)))

; some of the downside here are that you cannot evaluate which part of the test failed only that it binary passes or fails.
; one way to get around this is to print the success of each test as it runs.
; the problem with doing it this way is you have a lot of duplication and need to carry this around, so
; you can create a function to do that for you. rewritten in this manner it would like this..

(defun report-result (result form)
  (format t "~:[FAIL~;pass~] ... ~a~%" result form))

(defun test+- ()
    (report-result (= (+ 1 2 3) 6) '(= (+ 1 2 3) 6)))

; in here you duplicate code because you have evalue the expression and then also print the expression out

; a better way to do this is to create a macro that will do this for you.
(defmacro check (form)
    `(report-result ,form ',form))

(defun test+- ()
    (check (= (+ 1 2 3) 6))
    (check (= (+ 3 5 2) 10))
    (check (= (- 3 2 1) 0)))

; of course we can remove the repeated calls to check by creating a macro which uses progn

(defmacro check (&body forms)
    `(progn
        ,@(loop for f in forms collect `(report-result ,f ',f))))

(defun test+- ()
    (check (= (+ 1 2 3) 6)
           (= (+ 3 5 2) 10)
           (= (- 3 2 1) 0)
           (= (+ -3 -2) 34)))

; report-result doesn't propagate the actual result through to check and the test
; (test+-) returns NIL instead of T if all tests pass, FALSE if any fail.

(defun report-result (result form)
  (format t "~:[FAIL~;pass~] ... ~a~%" result form)
  result)
    
; Break 5 [9]> (report-result (= (+ 1 2 3) 6) '(= (+ 1 2 3) 6))
; pass ... (= (+ 1 2 3) 6)
; T

; now that report-result does propagate the status of the test, we can modify
; check to combine the results of all the tests.

; need to create our own version of (with-gensyms because the one in the book and the form in the book conflicts with the one that's in the standard library)

(defmacro with-gensyms-from-book ((&rest names) &body body)
  `(let ,(loop for n in names collect `(,n (gensym)))
     ,@body))

(defmacro combine-results (&body forms)
     (with-gensyms-from-book (result)
     `(let ((,result t))
         ,@(loop for f in forms collect `(unless ,f (setf ,result nil)))
         ,result)))

(defmacro check (&body forms)
     `(combine-results
         ,@(loop for f in forms collect `(report-result ,f ',f))))

(defun test+- ()
     (check (= (+ 1 2 3) 6)
            (= (+ 3 5 2) 10)
            (= (- 3 2 1) 0)
            (= (+ -3 -2) 34) ; meant to fail
            ))

; here we lern how to make a little test framework that will print the results of the tests that are inside of it by using (check) to evaluate and (print-result) to print
; we then combine all of the results by propagating the `result` from the bottom of the stack to the top

; what's missing is the name of the function that is actually being tested so that when a test fails, as an example in (test+-) the failure can be properly attributed to 
; that particulare function

; assuming we have (test+-), (test++), (test*) .. and we aggregated this into (test-arithmetic), we would want to know, as an example, that (test+-) inside of (test-arithmetic) failed
; was the one the one that failed for debugging purposes.

(defvar *test-name* nil)

; add in *test-name* to the report results
(defun report-result (result form)
  (format t "~:[FAIL~;pass~] ... ~a: ~a~%" result *test-name* form)
  result)


; add *test-name* as a dynamic global variable in the environment that test+- is evaluated in
(defun test+- ()
    (let ((*test-name* 'test+-))
     (check (= (+ 1 2 3) 6)
            (= (+ 3 5 2) 10)
            (= (- 3 2 1) 0)
            (= (+ -3 -2) 34) ; meant to fail
            )))

; the limitation that we have now is that you again have to repeat yourself for test-name.. also,
; we're using a normal function, whereas we could create a macro that would express the intent better than a function

; lets create a macro (deftest) that creates a function for the tests we want to create

(defmacro deftest (name parameters &body body)
    `(defun ,name ,parameters
        (let ((*test-name* ',name))
            ,@body)))

(deftest test+- ()
    (check (= (+ 1 2 3) 6)
           (= (+ 3 5 2) 10)
           (= (- 3 2 1) 0)
           (= (+ -3 -2) 34) ; meant to fail
           ))

; now imagine you wanted group and create a hierarchy of tests, for example, you have (test-arithmetic) and you want to group (test+-) and (test++) under (test-arithmetic)
; to do this simply, we can modify *test-name* to be a list

(defmacro deftest (name parameters &body body)
    `(defun ,name ,parameters
        (let ((*test-name* (append *test-name* (list ',name)))) ; append the name of the test to the list of tests
            ,@body)))


(deftest test+- ()
    (check (= (+ 1 2 3) 6)
           (= (+ 3 5 2) 10)
           (= (- 3 2 1) 0)
           (= (+ -3 -2) 34) ; meant to fail
           ))

(deftest test*()
    (check (= (* 1 2 3) 6)))

(deftest test-arithmetic () ; a test suite for arithmetic
    (combine-results
        (test+-) ; scope propagated as test-arithmetic . test+-
        (test*))) ; test-arithmetic . test*

; this works for infinite levels of nesting so

(deftest test-math ()
    (test-artithmetic))

