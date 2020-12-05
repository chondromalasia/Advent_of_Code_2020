(defparameter *filename* (string "/home/heath/Projects/lisp/Advent_of_Code_2020/day3_input.txt"))
(defparameter *dummy_file* (string "/home/heath/Projects/lisp/Advent_of_Code_2020/day3_dummy.txt"))

(defun read_file (flnm)
  (with-open-file (stream flnm)
    (loop for line = (read-line stream nil)
	  while line
	  collect (coerce line 'list))
  ))

(defun list-to-2d-array (list)
  (make-array (list (length list)
                    (length (first list)))
              :initial-contents list))

(defparameter *trees* (list-to-2d-array (read_file *filename*)))
(defparameter *dummy_trees* (list-to-2d-array (read_file *dummy_file*)))

(defparameter *max* (- (first (array-dimensions *trees*)) 1))
(defparameter *dummy_max* (- (first (array-dimensions *dummy_trees*)) 1))


(setq x 0)
(setq y 0)

(defun toboggan (row column row_inc col_inc)
  (if (> column 30)
      (setq column (- column 31)))
  ;(print (format nil "row column: ~a " (list row column))) 

  (cond
    ((> row *max* ) 0)
    ((equal #\# (aref *trees* row column))
     (1+
      (toboggan (+ row_inc row) (+ col_inc column) row_inc col_inc)))
    ((toboggan (+ row_inc row) (+ col_inc column) row_inc col_inc))))


(defparameter *slides* (list '(1 1) '(1 3) '(1 5) '(1 7) '(2 1)))

(defun tobogganator (slides)
  (cond
    ((null (first slides)) 1)
    (T (* (toboggan 0 0 (nth 0 (first slides)) (nth 1(first slides)))
	  (tobogganator (rest slides))))))

  
