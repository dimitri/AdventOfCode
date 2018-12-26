(in-package :advent/2018)

(defparameter *d14/board* "37")
(defparameter *d14/input* 323081)
(defparameter *d14/test* 9)
(defparameter *d14/size* (* 100 *d14/input*))

(defstruct scoreboard recipes pos-elf1 pos-elf2)

(defun create-board (board &key (capacity *d14/size*))
  (let ((recipes (make-array capacity
                             :element-type '(integer 0 9)
                             :adjustable t
                             :fill-pointer 0)))
    (loop :for digit :across board
       :do (vector-push (parse-integer (string digit)) recipes))
    recipes))

(defun create-scoreboard (&key (board *d14/board*))
  (make-scoreboard :recipes (create-board board) :pos-elf1 0 :pos-elf2 1))

(declaim (inline combine-recipes))

(defmethod combine-recipes (scoreboard &optional (nb-recipes 1))
  (with-slots (recipes pos-elf1 pos-elf2)
      scoreboard
    (flet ((next-position (position size)
             (mod (+ position 1 (aref recipes position)) size)))
      (loop :repeat nb-recipes
         :do (let* ((sum (+ (aref recipes pos-elf1) (aref recipes pos-elf2))))
               (multiple-value-bind (d1 d2) (truncate sum 10)
                 (unless (zerop d1) (vector-push d1 recipes))
                 (vector-push d2 recipes))

               (setf pos-elf1 (next-position pos-elf1 (length recipes)))
               (setf pos-elf2 (next-position pos-elf2 (length recipes))))))
    scoreboard))

(defmethod print-scoreboard ((scoreboard scoreboard))
  (with-slots (recipes pos-elf1 pos-elf2)
      scoreboard
    (format t "~{~a~}~%"
            (loop :for i :below (length recipes)
               :for score := (aref recipes i)
               :collect (cond
                          ((= i pos-elf1) (format nil "(~d)" score))
                          ((= i pos-elf2) (format nil "[~d]" score))
                          (t              (format nil " ~d " score)))))))

(defmethod scoreboard-to-string ((scoreboard scoreboard) start length)
  (let ((result (make-string length)))
    (loop :for i :from start :repeat length
       :for j :from 0
       :do (setf (aref result j)
                 (code-char (+ (aref (scoreboard-recipes scoreboard) i)
                               #. (char-code #\0)))))
    result))

(defun find-n-recipes-after-m-recipes (scoreboard n m)
  (scoreboard-to-string (combine-recipes scoreboard (+ m n)) m n))

(defun test/find-n-recipes-after-m-recipes ()
  (every #'identity
         (list (string= (let ((s (create-scoreboard)))
                          (find-n-recipes-after-m-recipes s 10 9))
                        "5158916779")
               (string= (let ((s (create-scoreboard)))
                          (find-n-recipes-after-m-recipes s 10 5))
                        "0124515891")
               (string= (let ((s (create-scoreboard)))
                          (find-n-recipes-after-m-recipes s 10 18))
                        "9251071085")
               (string= (let ((s (create-scoreboard)))
                          (find-n-recipes-after-m-recipes s 10 2018))
                        "5941429882"))))

(defun d14/p1 (&optional (input *d14/input*))
  (let ((s (create-scoreboard)))
    (find-n-recipes-after-m-recipes s 10 input)))

#|
51589 first appears after 9 recipes.
01245 first appears after 5 recipes.
92510 first appears after 18 recipes.
59414 first appears after 2018 recipes.
|#

(defun find-sequence (scoreboard target &optional (nb-recipes 1))
  (let* ((sequence (create-board target)))

    (loop :for start := 0 :then (+ start nb-recipes)
       :do (combine-recipes scoreboard nb-recipes)
       :thereis (search sequence (scoreboard-recipes scoreboard) :start2 start))))

(defun test/find-sequence ()
  (every #'identity
         (list (= 9    (find-sequence (create-scoreboard) "51589" 10))
               (= 5    (find-sequence (create-scoreboard) "01245" 10))
               (= 18   (find-sequence (create-scoreboard) "92510" 20))
               (= 2018 (find-sequence (create-scoreboard) "59414" 3000)))))

(defun d14/p2 (&optional (input *d14/input*))
  (let ((target (princ-to-string input)))
    (find-sequence (create-scoreboard) target *d14/size*)))

(defun d14/summary ()
  (format t "Day 14: Chocolate Charts~%")
  (format t "  Puzzle 1: scores of the ten recipes immediately after 323081~%")
  (print-result (d14/p1))
  (format t "  Puzzle 2: how many recipes to the left of 323081~%")
  (print-result (d14/p2)))
