(ql:quickload "series")
(defpackage :learning-series
  (:use :common-lisp
        :series)
  (:export :problem-a
           :problem-b))
(in-package :learning-series)
(series::install)

(defun fft-partial-sum (numbers)
  (declare (optimizable-series-function))
  (collecting-fn 'integer (lambda () 0)
                 (lambda (sum x) (mod (+ sum x) 10))
                 numbers))
(defun reverse-series (series)
  (declare (optimizable-series-function))
  (scan (reverse (collect series))))


(defun scan-fft-input (string)
  "Returns the FFT sequence in reverse as an infinite series."
  (declare (optimizable-series-function))
  (let ((inf (reverse (map 'list (lambda (c) (- (char-code c) (char-code #\0))) string))))
    (setf (cdr (last inf)) inf)
    (scan inf)))
(defun fft-part-2 (string &optional (times 100))
  (declare (optimizable-series-function))
  (let* ((offset (parse-integer (subseq string 0 7)))
         (limit (- (* 10000 (length string)) offset))
         (sequence (subseries (scan-fft-input string) 0 limit)))
    (reverse-series (choose (mask (scan-range :from (- limit 8)))
                            (collect-nth times
                                         (scan-fn t (lambda () sequence)
                                                  #'fft-partial-sum))))))
(defvar *test-val-1* "03036732577212944063491565474664")
(defvar *test-val-2* "02935109699940807407585447034323")
(defvar *test-val-3* "03081770884921959731165446850517")
(defun test-fft-part-2 ()
  (print (time (fft-part-2 *test-val-1*)))
  (print (time (fft-part-2 *test-val-2*)))
  (print (time (fft-part-2 *test-val-3*))))
(defvar *input* "59787832768373756387231168493208357132958685401595722881580547807942982606755215622050260150447434057354351694831693219006743316964757503791265077635087624100920933728566402553345683177887856750286696687049868280429551096246424753455988979991314240464573024671106349865911282028233691096263590173174821612903373057506657412723502892841355947605851392899875273008845072145252173808893257256280602945947694349746967468068181317115464342687490991674021875199960420015509224944411706393854801616653278719131946181597488270591684407220339023716074951397669948364079227701367746309535060821396127254992669346065361442252620041911746738651422249005412940728")
