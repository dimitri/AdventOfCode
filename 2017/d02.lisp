(in-package :advent/2017)

(defparameter *input/d2/p1*
  (asdf:system-relative-pathname :advent "2017/d02.input"))

#|

As you walk through the door, a glowing humanoid shape yells in your
direction. "You there! Your state appears to be idle. Come help us repair
the corruption in this spreadsheet - if we take another millisecond, we'll
have to display an hourglass cursor!"

The spreadsheet consists of rows of apparently-random numbers. To make sure
the recovery process is on the right track, they need you to calculate the
spreadsheet's checksum. For each row, determine the difference between the
largest value and the smallest value; the checksum is the sum of all of
these differences.

For example, given the following spreadsheet:

5 1 9 5
7 5 3
2 4 6 8

  - The first row's largest and smallest values are 9 and 1, and their
    difference is 8.

  - The second row's largest and smallest values are 7 and 3, and their
    difference is 4.

  - The third row's difference is 6.

In this example, the spreadsheet's checksum would be 8 + 4 + 6 = 18.

|#

(defun read-spreadsheet-line (line)
  (let ((position 0))
    (loop :for n := (multiple-value-bind (n pos)
                        (parse-integer line :start position :junk-allowed t)
                      (setf position pos)
                      n)
       :while n
       :collect n)))

(defun line-checksum (line)
  (loop :for n :in (read-spreadsheet-line line)
     :maximizing n :into max
     :minimizing n :into min
     :finally (return (- max min))))

(defun checksum (stream)
  (loop :for line := (read-line stream nil nil)
     :while line
     :sum (line-checksum line)))

(defun test/d2/p1 ()
  (with-input-from-string (s "5 1 9 5
7 5 3
2 4 6 8
")
    (checksum s)))

(defun d2/p1 ()
  (with-open-file (stream *input/d2/p1*
                          :direction :input
                          :element-type 'character)
    (checksum stream)))


#|

It sounds like the goal is to find the only two numbers in each row where
one evenly divides the other - that is, where the result of the division
operation is a whole number. They would like you to find those numbers on
each line, divide them, and add up each line's result.

For example, given the following spreadsheet:

5 9 2 8
9 4 7 3
3 8 6 5

  - In the first row, the only two numbers that evenly divide are 8 and 2;
    the result of this division is 4.

  - In the second row, the two numbers are 9 and 3; the result is 3.

  - In the third row, the result is 2.

  - In this example, the sum of the results would be 4 + 3 + 2 = 9.
|#

(defun line-checksum-p2 (line)
  (let* ((numbers (coerce (read-spreadsheet-line line) 'vector))
         (size    (length numbers)))
    (loop :for i :below size
       :do (loop :for j :below size
              :unless (= i j)
              :do (let ((min (min (aref numbers i) (aref numbers j)))
                        (max (max (aref numbers i) (aref numbers j))))
                    (multiple-value-bind (quotient remainder)
                        (truncate max min)
                      (when (zerop remainder)
                        (return-from line-checksum-p2 quotient))))))))

(defun checksum-p2 (stream)
  (loop :for line := (read-line stream nil nil)
     :while line
     :sum (line-checksum-p2 line)))

(defun test/d2/p2 ()
  (every #'identity (list (= 4 (line-checksum-p2 "5 9 2 8"))
                          (= 3 (line-checksum-p2 "9 4 7 3"))
                          (= 2 (line-checksum-p2 "3 8 6 5"))
                          (= 9 (with-input-from-string (s "5 9 2 8
9 4 7 3
3 8 6 5
")
                                 (checksum-p2 s))))))

(defun d2/p2 ()
  (with-open-file (input *input/d2/p1*
                         :direction :input
                         :element-type 'character)
    (checksum-p2 input)))


(defun d2/summary ()
  (format t "Day 2: Corruption Checksum~%")
  (format t "  Puzzle 1: checksum sum of difference smallest/largest~%")
  (print-result (d2/p1))
  (format t "  Puzzle 2: checksum sum of single exact quotient per line ~%")
  (print-result (d2/p2)))
