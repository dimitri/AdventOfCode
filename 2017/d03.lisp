(in-package :advent/2017)

#|

17  16  15  14  13
18   5   4   3  12
19   6   1   2  11
20   7   8   9  10
21  22  23---> ...
|#

(defun get-spiral-coordinates (input)
  (let ((row 0)
        (col 0)
        (current-square 1)
        (current-width 0))
    (loop
       :until (= current-square input)
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
              :do (let ((steps (+ (abs rstep) (abs cstep))))
                    ;; we might be able to rush through to the end of this
                    ;; line/col, or we're almost there and move only
                    ;; partly to the end
                    (let* ((moves (if (< (+ current-square steps) input)
                                      steps
                                      (- input current-square)))
                           (rstep (if (= 0 rstep) 0 (* moves (signum rstep))))
                           (cstep (if (= 0 cstep) 0 (* moves (signum cstep)))))
                      (incf row rstep)
                      (incf col cstep)
                      (incf current-square moves))))
       :finally (return (list row col)))))

(defun manhattan-distance (r1 c1 &optional (r2 0) (c2 0))
  "https://en.wikipedia.org/wiki/Taxicab_geometry"
  (+ (abs (- r1 r2)) (abs (- c1 c2))))

(defun d03/p1 (&optional (input 265149))
  "Find how many steps are required to move back to 1 (0 0) from input."
  (apply #'manhattan-distance (get-spiral-coordinates input)))

;; Data from square 1 is carried 0 steps, since it's at the access port.
;; Data from square 12 is carried 3 steps, such as: down, left, left.
;; Data from square 23 is carried only 2 steps: up twice.
;; Data from square 1024 must be carried 31 steps.
(defun test/d03/p1 ()
  "Test our spiral walking loop with given test values from the puzzle."
  (every #'identity (list (= 0 (d03/p1 1))
                          (= 3 (d03/p1 12))
                          (= 2 (d03/p1 23))
                          (= 31 (d03/p1 1024)))))

(defun d3/summary ()
  (format t "Day 3: Spiral Memory~%")
  (format t "  Puzzle 1: manhattan distance on a spiral~%")
  (print-result (d03/p1)))
