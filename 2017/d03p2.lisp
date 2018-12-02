(in-package :advent/2017)

#|

As a stress test on the system, the programs here clear the grid and then
store the value 1 in square 1. Then, in the same allocation order as shown
above, they store the sum of the values in all adjacent squares, including
diagonals.

So, the first few squares' values are chosen as follows:

  - Square 1 starts with the value 1.

  - Square 2 has only one adjacent filled square (with value 1), so it also
    stores 1.

  - Square 3 has both of the above squares as neighbors and stores the sum
    of their values, 2.

  - Square 4 has all three of the aforementioned squares as neighbors and
    stores the sum of their values, 4.

  - Square 5 only has the first and fourth squares as neighbors, so it gets
    the value 5.

Once a square is written, its value does not change. Therefore, the first
few squares would receive the following values:

147  142  133  122   59
304    5    4    2   57
330   10    1    1   54
351   11   23   25   26
362  747  806--->   ...

What is the first value written that is larger than your puzzle input?
|#

(defun compute-first-square-larger-than (input)
  (let ((values (make-hash-table :test 'equal))
        (row 0)
        (col 0)
        (current-width 0))
    (setf (gethash (cons 0 0) values) 1)
    (loop
       :do (loop :for (rstep cstep)
              ;; move right by 1, then up by 1
              ;; then left by 2, then bottom by 2
              ;; then right by 3, then up by 3
              ;; then left by 4, then bottom by 4
              ;; etc
              :in (list (list 0                    (incf current-width))
                        (list current-width        0)
                        (list 0                    (* -1 (incf current-width)))
                        (list (* -1 current-width) 0))
              :do (flet ((adjacent-squares-sum (row col)
                           (loop :for r :from -1 :to 1
                              :sum (loop :for c :from -1 :to 1
                                      :unless (= 0 r c)
                                      :sum (let ((rr (+ row r))
                                                 (cc (+ col c)))
                                             (gethash (cons rr cc) values 0))))))
                    (loop :repeat (abs rstep)
                       :for incr := (signum rstep)
                       :for value := (adjacent-squares-sum (incf row incr) col)
                       :when (< input value)
                       :do (return-from compute-first-square-larger-than value)
                       :do (setf (gethash (cons row col) values) value))

                    (loop :repeat (abs cstep)
                       :for incr := (signum cstep)
                       :for value := (adjacent-squares-sum row (incf col incr))
                       :when (< input value)
                       :do (return-from compute-first-square-larger-than value)
                       :do (setf (gethash (cons row col) values) value)))))))

(defun d03/p2 (&optional (input 265149))
  "Find how many steps are required to move back to 1 (0 0) from input."
  (compute-first-square-larger-than input))

#|
147  142  133  122   59
304    5    4    2   57
330   10    1    1   54
351   11   23   25   26
362  747  806--->   ...
|#
(defun test/d03/p2 ()
  "Test our spiral walking loop with given test values from the puzzle."
  (every #'identity (list (= 2   (d03/p2 1))
                          (= 10  (d03/p2 5))
                          (= 26  (d03/p2 25))
                          (= 122 (d03/p2 100))
                          (= 304 (d03/p2 200))
                          (= 747 (d03/p2 400)))))

(defun d3/summary ()
  (format t "Day 3: Spiral Memory~%")
  (format t "  Puzzle 1: manhattan distance on a spiral~%")
  (print-result (d03/p1))
  (format t "  Puzzle 2: first value larger than sum of previous ones on a spiral~%")
  (print-result (d03/p2)))
