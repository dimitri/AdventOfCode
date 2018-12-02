(in-package :advent/2018)

(defparameter *input/d1/p1*
  (asdf:system-relative-pathname :advent "2018/d01.input"))

(defun d1/p1 ()
  "Sum numbers read one per line."
  (with-open-file (input *input/d1/p1*
                         :direction :input
                         :element-type 'character)
    (loop :for line := (read-line input nil nil)
       :while line
       :sum (read-from-string line))))

(defun d1/p2 ()
  "Find the first frequency (partial sum) read twice, considering that the
   input list infinitely repeats itself."
  (let ((frequencies  (make-hash-table :test 'eql))
        (current-freq 0))
    (loop :for repeats :from 1
       :do (with-open-file (input *input/d1/p1*
                                  :direction :input
                                  :element-type 'character)
             (loop :for line := (read-line input nil nil)
                :while line
                :do (let ((freq-change (read-from-string line)))
                      (incf current-freq freq-change)
                      (let ((entry (gethash current-freq frequencies)))
                        (if entry
                            (return-from d1/p2
                              (values entry
                                      repeats
                                      (hash-table-count frequencies)))
                            (setf (gethash current-freq frequencies)
                                  current-freq)))))))))

(defun d1/summary ()
  (format t "Day 1: Chronal Calibration~%")
  (format t "  Puzzle 1: sum of frequency changes~%")
  (print-result (d1/p1))
  (format t "  Puzzle 2: first frequency read twice~%")
  (print-result (d1/p2)))
