(defun knot-reverse (orig pos len)
  (let* ((s (copy-seq orig)))
     (setf (cdr (last s)) s)
     (loop for i from pos
           for j downfrom (+ pos len -1)
           while (< i j)
           do (psetf (nth i s) (nth j s)
                     (nth j s) (nth i s)))
     (subseq s 0 (length orig))))

(defun knot-hash (lengths size)
  (loop for l in lengths
        with pos = 0
        with skip = 0
        with string = (loop for i from 0 below size collect i)
        finally (return string)
        do (setf string (knot-reverse string pos l))
           (incf pos (+ l skip))
           (setf pos (mod pos size))
           (incf skip)))
(let* ((lengths (list 76 1 88 148 166 217 130 0 128 254 16 2 130 71 255 229))
       (size 256)
       (hash (knot-hash lengths size)))
  (print hash)
  (print (* (first hash) (second hash))))