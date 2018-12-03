(in-package :advent/2017)

(defparameter *input/d1/p1*
  (asdf:system-relative-pathname :advent "2017/d01.input"))

#|

The captcha requires you to review a sequence of digits (your puzzle input)
and find the sum of all digits that match the next digit in the list. The
list is circular, so the digit after the last digit is the first digit in
the list.

For example:

  - 1122 produces a sum of 3 (1 + 2) because the first digit (1) matches the
    second digit and the third digit (2) matches the fourth digit.

  - 1111 produces 4 because each digit (all 1) matches the next.

  - 1234 produces 0 because no digit matches the next.

  - 91212129 produces 9 because the only digit that matches the next one is
    the last digit, 9.

|#

(defun char-digit-to-number (digit)
  (- (char-code digit) #. (char-code #\0)))

(defun captcha (string)
  (let ((string (concatenate 'string string (subseq string 0 1))))
    (loop :for previous-digit := nil :then digit
       :for digit :across string
       :when (and previous-digit (char= digit previous-digit))
       :sum (char-digit-to-number digit))))

(defun test/d1/p1 ()
  (every #'identity (list (= 3 (captcha "1122"))
                          (= 4 (captcha "1111"))
                          (= 0 (captcha "1234"))
                          (= 9 (captcha "91212129")))))

(defun d1/p1 ()
  (with-open-file (input *input/d1/p1*
                         :direction :input
                         :element-type 'character)
    (captcha (read-line input))))

#|

Now, instead of considering the next digit, it wants you to consider the
digit halfway around the circular list. That is, if your list contains 10
items, only include a digit in your sum if the digit 10/2 = 5 steps forward
matches it. Fortunately, your list has an even number of elements.

For example:

  - 1212 produces 6: the list contains 4 items, and all four digits match
    the digit 2 items ahead.

  - 1221 produces 0, because every comparison is between a 1 and a 2.

  - 123425 produces 4, because both 2s match each other, but no other digit
    has a match.

  - 123123 produces 12.
  - 12131415 produces 4.

|#

(defun captcha-p2 (string)
  (declare (type string string))
  (let* ((size   (length string))
         (steps  (truncate size 2))
         (string (concatenate 'string string string)))
    (loop :repeat size
       :for position :from 0
       :for char :across string
       :when (char= char (aref string (+ steps position)))
       :sum (char-digit-to-number char))))

(defun test/d1/p2 ()
  (every #'identity (list (= 6 (captcha-p2 "1212"))
                          (= 0 (captcha-p2 "1221"))
                          (= 4 (captcha-p2 "123425"))
                          (= 12 (captcha-p2 "123123"))
                          (= 4 (captcha-p2 "12131415")))))

(defun d1/p2 ()
  (with-open-file (input *input/d1/p1*
                         :direction :input
                         :element-type 'character)
    (captcha-p2 (read-line input))))


(defun d1/summary ()
  (format t "Day 1: Inverse Captcha~%")
  (format t "  Puzzle 1: captcha (next digit)~%")
  (print-result (d1/p1))
  (format t "  Puzzle 2: captcha (digit halfway around)~%")
  (print-result (d1/p2)))
