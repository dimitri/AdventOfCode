(in-package :advent/2018)

#|

dabAcCaCBAcCcaDA  The first 'cC' is removed.
dabAaCBAcCcaDA    This creates 'Aa', which is removed.
dabCBAcCcaDA      Either 'cC' or 'Cc' are removed (the result is the same).
dabCBAcaDA        No further actions can be taken.

After all possible reactions, the resulting polymer contains 10 units.

How many units remain after fully reacting the polymer you scanned?

|#

(defparameter *input/d5/p1*
  (with-open-file (s (asdf:system-relative-pathname :advent "2018/d05.input")
                     :direction :input
                     :element-type 'character)
    (read-line s)))

(defparameter *input/d5/test/1* "dabAcCaCBAcCcaDA")
(defparameter *input/d5/test/2* "bBtrlaALRBb")
(defparameter *debug* nil)

(defun units-reaction (unit1 unit2)
  (declare (type character unit1 unit2))
  (if (and (char/= unit1 unit2)
           (char= (char-downcase unit1) (char-downcase unit2)))
      nil
      (list unit1 unit2)))

(defun reduce-polymer (polymer)
  (declare (type string polymer))
  (when *debug*
    (format t "reduce-polymer ~a~%" polymer))
  (let* ((size      (length polymer))
         (result    (make-string size :initial-element #\-))
         (rpos      0))
    (if (< size 2)
        polymer
        (let ((ppos      0)
              (candidate nil))
          (flet ((push-candidate ()
                   (setf (aref result rpos) candidate)
                   (incf rpos)
                   (setf candidate nil)))
            (loop
               :while (< ppos size)
               :do (setf candidate
                         (or candidate
                             (prog1
                                 (aref polymer ppos)
                               (incf ppos))))
               :while (< ppos size)
               :do (let ((reaction
                          (units-reaction candidate (aref polymer ppos))))
                     (when *debug*
                       (format t "~a ~20t[~a ~a] [~a/~a]~%"
                               (subseq result 0 rpos)
                               candidate (aref polymer ppos) rpos ppos))
                     (if reaction
                         (push-candidate)
                         (progn
                           (incf ppos)
                           (if (< 0 rpos)
                               (progn
                                 (decf rpos)
                                 (setf candidate (aref result rpos)))
                               (setf candidate nil))))))
            (when candidate (push-candidate)))
          (subseq result 0 rpos)))))

(defun d5/p1/test ()
  (values (length (reduce-polymer *input/d5/test/1*))
          (length (reduce-polymer *input/d5/test/2*))))

(defun d5/p1 (&optional (polymer *input/d5/p1*))
  (length (reduce-polymer polymer)))

#|

One of the unit types is causing problems; it's preventing the polymer from
collapsing as much as it should. Your goal is to figure out which unit type
is causing the most problems, remove all instances of it (regardless of
polarity), fully react the remaining polymer, and measure its length.

|#

(defun remove-every-unit (polymer)
  (declare (type string polymer))
  (let ((units (make-hash-table :test 'equalp)))
    (loop :for unit :across polymer
       :do (unless (gethash unit units)
             (setf (gethash unit units)
                   (reduce-polymer
                    (remove-if (lambda (c) (equalp c unit)) polymer)))))
    units))

(defun d5/p2 (&optional (polymer *input/d5/p1*))
  (let ((units (remove-every-unit polymer))
        winner)
    (maphash (lambda (unit candidate)
               (when (or (null winner)
                         (< (length candidate) (length (gethash winner units))))
                 (setf winner unit)))
             units)
    (values (length (gethash winner units))
            winner)))

(defun d5/summary ()
  (format t "Day 5: Alchemical Reduction~%")
  (format t "  Puzzle 1: Polymer reactions~%")
  (print-result (d5/p1))
  (format t "  Puzzle 2: Time to improve the polymer.~%")
  (print-result (d5/p2)))
