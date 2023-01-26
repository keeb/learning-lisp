


; global variable the holds the database in memory
(defvar *db* nil)

(defun make-cd (title artist rating ripped)
    (list :title title :artist artist :rating rating :ripped ripped))

(defun add-record (cd) (push cd *db*))

(defun dump-db ()
    (dolist (cd *db*)
        (format t "~{~a:~10t~a~%~}~%" cd)))

(defun dump-db-oneliner ()
  (format t "~{~{~a:~10t~a~%~}~%~}" *db*))

(defun prompt-read (prompt)
    (format *query-io* "~a: " prompt)
    (force-output *query-io*)
    (read-line *query-io*))

(defun prompt-for-cd () 
    (make-cd 
        (prompt-read "Title")
        (prompt-read "Artist")
        (or (parse-integer (prompt-read "Rating") :junk-allowed t) 0)
        (y-or-n-p "Ripped [y/n]")))

(defun add-cds ()
    (loop (add-record (prompt-for-cd))
        (if (not (y-or-n-p "Another? [y/n]: ")) (return))))

(defun save-db (filename)
    (with-open-file (out filename 
                    :direction :output
                    :if-exists :supersede)
        (with-standard-io-syntax
            (print *db* out))))

(defun load-db (filename)
    (with-open-file (in filename)
        (with-standard-io-syntax
            (setf *db* (read in)))))



(defun select-by-artist (artist)
    ; find :artist in *db*
    ; downside of this approach is that you have to *select-by-x* for each x
    (remove-if-not
        #'(lambda (cd) (equal (getf cd :artist) artist))
        *db*))
    
(defun select (selector-fn)
    ; this is more generic it just takes in the anonymous function instead
    (remove-if-not selector-fn *db*))

; but the downside has moved up the stack again now we have to write field-specific selectors...
(defun artist-selector (artist)
    #'(lambda (cd) (equal (getf cd :artist) artist)))

(defun title-selector (title)
    #'(lambda (cd) (equal (getf cd :title) title)))


; so let's do it the righter way, starting from a new primitive we're learning
; a new way to do it where we use kwargs and select on filter on all that are provided
(defun where-non-macro (&key title artist rating (ripped nil ripped-p))
    #'(lambda (cd)
        (and
            (if title    (equal (getf cd :title)  title)  t)
            (if artist   (equal (getf cd :artist) artist) t)
            (if rating   (equal (getf cd :rating) rating) t)
            (if ripped-p (equal (getf cd :ripped) ripped) t))))


(defun update (selector-fn &key title artist rating (ripped nil ripped-p))
    (setf *db*
        (mapcar
            #'(lambda (row)
                (when (funcall selector-fn row)
                    (if title (setf (getf row :title) title))
                    (if artist (setf (getf row :artist) artist))
                    (if rating (setf (getf row :rating) rating))
                    (if ripped-p (setf (getf row :ripped) ripped)))
            row) *db*)))

(defun delete-rows (selector-fn)
    (setf *db* (remove-if selector-fn *db*)))


;; LEARNING

; &key == kwargs from python, allows you to define defaults for specific keys.
; in this function, a is the only *required* argument
; c-p populates to TRUE if C is overridden

(defun foo (&key a (b 20) (c 30 c-p)) (list a b c c-p))
; Break 1 [15]> (foo :a 1)
; (1 20 30 NIL)
; Break 1 [15]> (foo :c 200)
; (NIL 20 200 T)

; remake function with no required parameters, and notice only c-p is overridden to T if C is called.
(defun foo2 (&key (a 10) (b 20) (c 30 c-p)) (list a b c c-p))
; Break 1 [15]> (foo2)
; (10 20 30 NIL)
; Break 1 [15]> (foo2 :a 100)
; (100 20 30 NIL)
; Break 1 [15]> (foo2 :c 100)
; (10 20 100 T)



;;;; MACROS

(defun make-comparison-expr-broken (field value)
    (list 'equal (list 'getf 'cd field) value))

; Break 4 [5]> (make-comparison-expr-broken :rating 10)
; (EQUAL (GETF CD :RATING) 10)


;; ` takes the literal string as an argument
;; , inside of a ` goes back to interpretation
; Break 3 [4]> `(1 2 3)
; (1 2 3)
; Break 3 [4]> '(1 2 3)
; (1 2 3)
; Break 3 [4]> `(1 2 (+ 1 2))
; (1 2 (+ 1 2))
; Break 3 [4]> `(1 2 ,(+ 1 2))
; (1 2 3)

;; using , we can make it work by prepending appropriately and evaluating the value of field/value

(defun make-comparison-expr (field value)
    `(equal (getf cd ,field) ,value))

; Break 4 [5]> (make-comparison-expr :rating 10)
; (EQUAL (GETF CD :RATING) 10)


(defun make-comparison-list (fields)
    (loop while fields
        collecting (make-comparison-expr (pop fields) (pop fields))))

;; difference between , and ,@
;; , takes the resulting literal eval with ()
;; @ paired with , takes the resulting literal eval *results* (book calls this splicing)
;; in my mind, this is the way to return an actual value as opposed to an expression?

; Break 4 [5]> `(and ,(list 1 2 3))
; (AND (1 2 3))
; Break 4 [5]> `(and ,@(list 1 2 3))
; (AND 1 2 3)

;; using this is magic, this basically expands to become the function that is (where-non-macro)
(defmacro where (&rest clauses)
    `#'(lambda (cd) (and ,@(make-comparison-list clauses))))

; Break 6 [7]> (where :title "The Chronic")
; #<FUNCTION :LAMBDA (CD) (AND (EQUAL (GETF CD :TITLE) "The Chronic"))>
; Break 6 [7]> (select (where :title "The Chronic"))
; ((:TITLE "The Chronic" :ARTIST "Dr Dre" :RATING 10 :RIPPED NIL))




; how the printf of clisp works 
(format t "~a" "Dixie Chicks")
(format t "~a" :title)
(format t "~a:~10t~a" :artist "Dixie Chicks")


; remove-if-not showcase, evens

(remove-if-not #'evenp '(1 2 3 4 5 6 7 8 9 10))
(remove-if-not #'(lambda (x) (= 0 (mod x 2))) ' (1 2 3 4 5 6 7 8 9 10))

; remove-if-not showcase, odds
(remove-if-not #'(lambda (x) (= 1 (mod x 2))) ' (1 2 3 4 5 6 7 8 9 10))


; use the primitives we created

; manual creation of records
(add-record (make-cd "Roses" "Kathy Mattea" 7 t))
(add-record (make-cd "Fly" "Dixie Chicks" 8 t))
(add-record (make-cd "Home" "Dixie Chicks" 9 t))

; at some point should make a check if file exists and if not go into prompt..
(load-db "waow.db")

;; usage

(select (where :ripped nil))
(select (where :ripped (not nil)))


