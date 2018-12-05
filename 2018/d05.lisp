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

(defun units-react-p (unit1 unit2)
  (declare (type character unit1 unit2))
  (and (char/= unit1 unit2)
       (char-equal unit1 unit2)))

#|

https://www.reddit.com/r/adventofcode/comments/a3912m/2018_day_5_solutions/

part1 :: String -> Int
part1 = length . foldr step ""
  where
    step x (y:ys) | x /= y && toUpper x == toUpper y = ys
    step x ys                                        = x : ys

reduce-polymer-foldr is nice but so slow!

  Puzzle 1: Polymer reactions
  1426.065ms 10250
  Puzzle 2: Time to improve the polymer.
  16801.107ms 6188

Compared to the manual too-complex thing:

  Puzzle 1: Polymer reactions
   65.761ms 10250
  Puzzle 2: Time to improve the polymer.
  1398.750ms 6188

|#

(defun reduce-polymer-foldr (polymer)
  (reduce (lambda (unit1 unit2)
            (if (and (string/= "" unit2)
                     (char/= unit1 (aref unit2 0))
                     (char= (char-downcase unit1) (char-downcase (aref unit2 0))))
                (subseq unit2 1)        ; pop
                (concatenate 'string (list unit1) unit2)))
          polymer
          :from-end t
          :initial-value ""))

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
          (flet ((maybe-consider-next-candidate ()
                   (unless candidate
                     (when (< ppos size)
                       (setf candidate (prog1
                                           (aref polymer ppos)
                                         (incf ppos))))))
                 (push-candidate ()
                   (setf (aref result rpos) candidate)
                   (incf rpos)
                   (setf candidate nil))
                 (pop-candidate ()
                   (incf ppos)
                   (if (< 0 rpos)
                       (progn
                         (decf rpos)
                         (setf candidate (aref result rpos)))
                       (setf candidate nil))))
            (loop
               :do (maybe-consider-next-candidate)
               :while (< ppos size)
               :do (when *debug*
                     (format t "~a ~20t[~a ~a] [~a/~a]~%"
                             (subseq result 0 rpos)
                             candidate (aref polymer ppos) rpos ppos))
               :do (if (units-react-p candidate (aref polymer ppos))
                       (pop-candidate)
                       (push-candidate)))
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
                   (reduce-polymer (remove unit polymer :test #'char-equal)))))
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
