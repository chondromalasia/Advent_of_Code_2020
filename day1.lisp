(defparameter *filename* (string "/home/heath/Projects/lisp/Advent_of_Code_2020/day1_input.txt"))

(defun read_file (flnm)
  "Read a file into a list line by line as integars"
  (with-open-file (stream flnm)
    (loop for line = (read-line stream nil)
	  while line
	  collect (parse-integer line)))
  )

(defvar *num_list* (read_file *filename*))

(defun test_add_ (tt list)
  "For int tt and lisnt of ints tt, check to see if int tt adds to 2020 with any numbers
  of the list. Return the two numbers multiplied if so, nil if nothing is found"
  (cond
    ((null (first list)) nil)
    ((eql 2020 (+ tt (first list))) (* tt (first list)))
    (T (test_add_ tt (rest list)))))

(defun test_add (lst)
  "For each integar in lst, check to see if it adds to 2020 with any of the others, via
  test_add_, if so, return those numbers multiplied"
  (let ((tempn (test_add_ (first lst) (rest lst))))
    (if (null tempn)
     (test_add (rest lst))
     tempn)))

; (test_add *num_list*)

(defun test_add_2_top (x y lst)
  "Checks if x + y and any of lst = 2020, returns them multiplied if so,
  otherwise nil"
  (cond
    ((null (first lst)) nil)
    ((eql 2020 (+ x y (first lst))) (* x y (first lst)))
    (T (test_add_2_top x y (rest lst)))))

(defun test_add_2_mid (x lst)
  "Passes a number and the first digit of lst to test_add_2_top to see if
  any number contained in list adds to 2020 with any of those"
  (let ((tempn (test_add_2_top x (first lst) (rest lst))))

    (cond
      ((null (first lst)) nil)
      ((null tempn) (test_add_2_mid x (rest lst)))
      (T tempn))))
    

(defun test_add_2 (lst)
  "Goes through a list to see if any 3 of the numbers equals 2020 when added up
  by passing the first digit and the rest of the list to test_add_2_mid"
  (let ((tempn (test_add_2_mid (first lst) (rest lst))))
    (if (null tempn)
	(test_add_2 (rest lst))
	tempn)))

; (test_add_2 *num_list*)

