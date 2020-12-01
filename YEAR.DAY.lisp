(unless (find-package :cl-ppcre)
  (ql:quickload "cl-ppcre"))
(unless (find-package :iterate)
  (ql:quickload "iterate"))
(unless (find-package :parseq)
  (ql:quickload "parseq"))
(unless (find-package :fiveam)
  (ql:quickload "fiveam"))
(unless (find-package :series)
  (ql:quickload "series"))
(unless (find-package :cl-permutation)
  (ql:quickload "cl-permutation"))
(unless (find-package :bordeaux-threads)
  (ql:quickload "bordeaux-threads"))
(defpackage :aoc-YEAR-DAY
  (:use :common-lisp
        :iterate
        :parseq
        :fiveam)
  (:export :problem-a
           :problem-b))
(in-package :aoc-YEAR-DAY)

(defun read-input (file)
  (iter (for line in-file file using #'read-line)
        (collect line)))
(defparameter *input*
  (read-input "input/DAY.txt"))
(defparameter *input*
  (read-input "input/DAY.txt"))
(defun problem-a () (format t "Problem DAY A: ~a~%" (identity *input*)))
(defun problem-b () (format t "Problem DAY B: ~a~%" (identity *input*)))
(problem-a)
(problem-b)
