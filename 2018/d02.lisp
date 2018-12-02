(in-package :advent/2018)

(defparameter *input/d2/p1*
  (asdf:system-relative-pathname :advent "2018/d02.input"))

(defun contains-two-or-three-of-any-letter (string)
  "Returns non-nil when STRING contains exactly two of any letter, and
   exactly three of any letter, as separate values.."
  (declare (type string string))
  (let ((chars (make-hash-table :test 'eql :size (length string))))
    (loop :for char :across string
       :do (incf (gethash char chars 0)))

    (loop :for value :being :the :hash-value :of chars
       :count (= 2 value) :into two-of-any
       :count (= 3 value) :into three-of-any
       :finally (return (values (< 0 two-of-any) (< 0 three-of-any))))))

(defun test/contains-two-or-three-of-any-letter ()
  "Test our function given the examples at https://adventofcode.com/2018/day/2"
  (every #'identity
         (list (equal (values nil nil)
                      (contains-two-or-three-of-any-letter "abcdef"))
               (equal (values t t)
                      (contains-two-or-three-of-any-letter "bababc"))
               (equal (values t nil)
                      (contains-two-or-three-of-any-letter "abbcde"))
               (equal (values nil t)
                      (contains-two-or-three-of-any-letter "abcccd"))
               (equal (values t nil)
                      (contains-two-or-three-of-any-letter "aabcdd"))
               (equal (values t nil)
                      (contains-two-or-three-of-any-letter "abcdee"))
               (equal (values nil t)
                      (contains-two-or-three-of-any-letter "ababab")))))

(defun d2/p1 ()
  "Sum numbers read one per line."
  (let ((two-of-any-letter 0)
        (three-of-any-letter 0))
   (with-open-file (input *input/d2/p1*
                          :direction :input
                          :element-type 'character)
     (loop :for line := (read-line input nil nil)
        :while line
        :do (multiple-value-bind (two three)
                (contains-two-or-three-of-any-letter line)
              (when two (incf two-of-any-letter))
              (when three (incf three-of-any-letter)))
        :finally (return (* two-of-any-letter three-of-any-letter))))))

(defun strings-differ-at-only-one-position-p (string1 string2)
  "Returns non-nil when STRING1 and STRING2 are the same series of
   characters except for one."
  (declare (type string string1 string2))
  (when (= (length string1) (length string2))
    (= 1
       (loop :for char1 :across string1
          :for char2 :across string2
          :count (char/= char1 char2)))))

(defun d2/p2 ()
  "Find the only two labels with only two different letters at the same position."
  (let* ((strings
          (coerce (with-open-file (input *input/d2/p1*
                                         :direction :input
                                         :element-type 'character)
                    (loop :for line := (read-line input nil nil)
                       :while line
                       :collect line))
                  'vector))
         (count (length strings)))
    (destructuring-bind (string1 string2)
        ;; quadratic approach, good enough for the input size
        (loop :for i :below count
           :for candidate1 := (aref strings i)
           :thereis (loop :for j :below count
                       :for candidate2 := (aref strings j)
                       :when (strings-differ-at-only-one-position-p candidate1
                                                                    candidate2)
                       :return (list candidate1 candidate2)))
      ;; build the resulting label
      (values string1
              string2
              (coerce (loop :for char1 :across string1
                         :for char2 :across string2
                         :unless (char/= char1 char2)
                         :collect char1)
                      'string)))))

(defun d2/summary ()
  (format t "Day 2: Inventory Management System~%")
  (format t "  Puzzle 1: checksum of box ids~%")
  (print-result (d2/p1))
  (format t "  Puzzle 2: common letters in box ids one letter apart~%")
  (print-result (d2/p2)))
