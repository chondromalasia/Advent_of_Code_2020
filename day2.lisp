;;; Advent of Code day 2 
;;; https://adventofcode.com/2020/day/2

(ql:quickload "cl-ppcre")

(setq dummy_example (string "1-3 a: abcde"))

(setq dummy_list '("1-3 a: abcde"
		   "2-9 c: ccccccccc"
		   "1-3 b: cdefg"))

(defparameter *filename* (string "/home/heath/Projects/lisp/Advent_of_Code_2020/day2_input.txt"))


(defun read_file (flnm)
  "Read a file into a list line by line"
  (with-open-file (stream flnm)
    (loop for line = (read-line stream nil)
	  while line
	  collect line))
  )

(setq *psswdlst* (read_file *filename*))

(defun count_valid_helper (lst)
  "Takes the list from process name and sees if the count of letter lst[2]
  in string (lst[3]) is betwen lst[0] and lst[1]. Returns T if True, nil if otherwise."
  (let ((count_ 0))
    (setq count_ (count (nth 2 lst) (nth 3 lst)))
    (if (and (<= (nth 0 lst) count_) (>= (nth 1 lst) count_))
	T
	nil)))


(defun process_name (str)
  "Splits a string of style 'x-y a: str' into a list of int(x) int(y)
  char(a) and str(str)
  Author's note: This could be lispier for sure, but this is what I'm going with"
  (let ((min_ 0)
	(max_ 0)
	(letter #\x)
	(string "")
	(rest_ ""))
    (setq rest_ (cl-ppcre:split "-" str))
    (setq min_ (parse-integer (first rest_)))
    (setq rest_ (cl-ppcre:split " " (first (rest rest_))))
    (setq max_ (parse-integer (first rest_)))
    (setq letter (coerce (first (cl-ppcre:split ":" (nth 1 rest_))) 'character))
    (setq string (nth 2 rest_))
    (list min_ max_ letter string)))

(defun count_list_helper (lst)
  "For each member of a list, process its name and see if it has the right
   number of chars in the string"
  (cond
    ((null (first lst)) 0)
    ((null (count_valid_helper (process_name (first lst)))) (count_list_helper (rest lst)))
    (T (1+ (count_list_helper (rest lst))))))

(defun count_list (lst)
  "Wrapper function for count_list_helper"
  (count_list_helper lst))

;; find if it is in the correct position
(defun xor (a b)
  "Turns out Common Lisp as far as I could tell does not have a builtin xor!"
  (cond
    ((and (not (null a)) (null b)) T)
    ((and (not (null b)) (null a)) T)
    (T nil)))
  
(defun right_position (lst)
  "If there is only one instance of the character at the given position of 
  one of the integars (xor) then return T"
  (if
   (xor
   (eql (nth 2 lst) (char (nth 3 lst) (- (nth 0 lst) 1 )))
   (eql (nth 2 lst) (char (nth 3 lst) (- (nth 1 lst) 1 ))))
      T
      nil))

(defun count_correct_position_helper (lst)
  "For each member of the list, see if the characters are in the right position,
  passes list to process_name and the right_position. Returns count of correct
  passwords"
  (cond
    ((null (first lst)) 0)
    ((null (right_position (process_name (first lst))))
     (count_correct_position_helper (rest lst)))
    (T (1+ (count_correct_position_helper (rest lst))))))

(defun count_correct_position (lst)
  "Wrapper function for count_correct_position_helper"
  (count_correct_position_helper lst))
  
