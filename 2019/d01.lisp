(in-package :advent/2019)

(defparameter *input/d1/p1*
  (asdf:system-relative-pathname :advent "2019/d01.input"))

(defun compute-fuel-from-mass (mass)
  (declare (type fixnum mass))
  (- (floor mass 3) 2))

(defun test/d1/p1 ()
  (every #'identity
         (list (= 2 (compute-fuel-from-mass 12))
               (= 2 (compute-fuel-from-mass 14))
               (= 654 (compute-fuel-from-mass 1969))
               (= 33583 (compute-fuel-from-mass 100756)))))

(defun d1/p1 ()
  "Sum numbers read one per line."
  (with-open-file (input *input/d1/p1*
                         :direction :input
                         :element-type 'character)
    (loop :for line := (read-line input nil nil)
       :while line
       :sum (compute-fuel-from-mass (read-from-string line)))))

(defun compute-total-fuel-from-mass (mass)
  (let ((initial-fuel  (compute-fuel-from-mass mass)))
    (+ initial-fuel
       (loop
          :for rec-fuel := (compute-fuel-from-mass initial-fuel)
          :then (compute-fuel-from-mass rec-fuel)
          :while (< 0 rec-fuel)
          :sum rec-fuel))))

(defun d1/p2 ()
  "Sum numbers read one per line."
  (with-open-file (input *input/d1/p1*
                         :direction :input
                         :element-type 'character)
    (loop :for line := (read-line input nil nil)
       :while line
       :sum (compute-total-fuel-from-mass (read-from-string line)))))

(defun d1/summary ()
  (format t "Day 1: The Tyranny of the Rocket Equation~%")
  (format t "  Puzzle 1: fuel for all the modules~%")
  (print-result (d1/p1))
  (format t "  Puzzle 2: fuel for the fuel too~%")
  (print-result (d1/p2)))
