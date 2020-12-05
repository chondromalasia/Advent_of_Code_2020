;;;Day 3 of Advent of Code
;;;https://adventofcode.com/2020/day/3

(defparameter *filename* (string "/home/heath/Projects/lisp/Advent_of_Code_2020/day3_input.txt"))
(defparameter *dummy_file* (string "/home/heath/Projects/lisp/Advent_of_Code_2020/day3_dummy.txt"))

(defun read_file (flnm)
  (with-open-file (stream flnm)
    (loop for line = (read-line stream nil)
	  while line
	  collect (coerce line 'list))
  ))

(defun list-to-2d-array (list)
  "Convert a list into a 2d array"
  (make-array (list (length list)
                    (length (first list)))
              :initial-contents list))

; *trees* is the array of the trees
(defparameter *trees* (list-to-2d-array (read_file *filename*)))
(defparameter *dummy_trees* (list-to-2d-array (read_file *dummy_file*)))

; *max* is the length of the array
(defparameter *max* (- (first (array-dimensions *trees*)) 1))
(defparameter *dummy_max* (- (first (array-dimensions *dummy_trees*)) 1))


(defun toboggan (row column row_inc col_inc)
  "Go through the array, incrementing by row_inc and col_inc
  counting all the 'trees' landed on. Returns count"
  (if (> column 30)
      (setq column (- column 31))) ; wrap around the columns

  (cond
    ((> row *max* ) 0)
    ((equal #\# (aref *trees* row column))
     (1+
      (toboggan (+ row_inc row) (+ col_inc column) row_inc col_inc)))
    ((toboggan (+ row_inc row) (+ col_inc column) row_inc col_inc))))


;;Part 2
; these are the new increments to go through
(defparameter *slides* (list '(1 1) '(1 3) '(1 5) '(1 7) '(2 1)))

(defun tobogganator (slides)
  "Wrapper for toboggan, goes through a list of slides, multiplies
  the outcome together"
  (cond
    ((null (first slides)) 1)
    (T (* (toboggan 0 0 (nth 0 (first slides)) (nth 1(first slides)))
	  (tobogganator (rest slides))))))

  
