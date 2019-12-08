(in-package :advent/2019)

(defparameter *input/d4* (cons 402328 864247))

(declaim (inline digit-list))

(defun digit-list (password)
  (loop :for pow :from 5 :downto 0
     :for prev := 0 :then rem
     :for rem := (truncate password (expt 10 pow))
     :collect (- rem (* prev 10))))

(defun valid-password-p (password)
  (declare (type fixnum password))
  (let ((digits (digit-list password))
        (saw-same-adjacent-digits nil))
    (loop :for count :from 0
       :for previous := nil :then digit
       :for digit :in digits
       :while (or (null previous) (<= previous digit))
       :do (when (and previous (= previous digit))
             (setf saw-same-adjacent-digits t))
       :finally (return (and (= count 6)
                             saw-same-adjacent-digits)))))

(defun test/d4/p1 ()
  (every #'identity
         (list
          (valid-password-p 111111)
          (valid-password-p 122345)
          (valid-password-p 111123)
          (not (valid-password-p 135679))
          (not (valid-password-p 223450))
          (not (valid-password-p 123789)))))

(defun count-valid-password-in-range (range)
  (loop :for candidate-password :from (car range) :to (cdr range)
     :count (valid-password-p candidate-password)))

(defun d4/p1 ()
  (count-valid-password-in-range *input/d4*))

(defun valid-password-with-a-double-p (password)
  (declare (type fixnum password))
  (let ((digits (digit-list password))
        (dvect  (make-array 10 :element-type 'fixnum :initial-element 0)))
    (loop :for count :from 0
       :for previous := nil :then digit
       :for digit :in digits
       :while (or (null previous) (<= previous digit))
       :do (incf (aref dvect digit))
       :finally (return (and (= count 6)
                             (find 2 dvect))))))

(defun count-valid-password-with-a-double-in-range (range)
  (loop :for candidate-password :from (car range) :to (cdr range)
     :count (valid-password-with-a-double-p candidate-password)))

(defun d4/p2 ()
  (count-valid-password-with-a-double-in-range *input/d4*))

(defun d4/summary ()
  (format t "Day 4: Secure Container~%")
  (format t "  Puzzle 1: count valid passwords in given range~%")
  (print-result (d4/p1))
  (format t "  Puzzle 2: count valid passwords with a double in range~%")
  (print-result (d4/p2)))
