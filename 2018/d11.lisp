(in-package :advent/2018)

(defparameter *d11/input* 1718)

(defun compute-power-level (x y grid-serial-number)
  "The power level in a given fuel cell can be found through the following
  process:

  - Find the fuel cell's rack ID, which is its X coordinate plus 10.
  - Begin with a power level of the rack ID times the Y coordinate.
  - Increase the power level by the value of the grid serial number (your
    puzzle input).
  - Set the power level to itself multiplied by the rack ID.
  - Keep only the hundreds digit of the power level (so 12345 becomes 3;
    numbers with no hundreds digit become 0).
  - Subtract 5 from the power level."
  (let* ((rack-id     (+ x 10))
         (power-level (* y rack-id))
         (power-level (+ power-level grid-serial-number))
         (power-level (* power-level rack-id))
         (hundreds    (mod (truncate power-level 100) 10)))
    (+ -5 hundreds)))

(defun test/compute-power-level ()
  (every #'identity (list (= (compute-power-level 3 5 8) 4)
                          (= (compute-power-level 122 79 57) -5)
                          (= (compute-power-level 217 196 39) 0)
                          (= (compute-power-level 101 153 71) 4))))

;;; map x-y coordinates (0 based) to grid row/column (1 based)
(defun xy-to-rc (xy) (+ 1 xy))
(defun rc-to-xy (rc) (+ -1 rc))

(defun build-grid (serial-number &optional (rows 300) (cols 300))
  (let ((grid (make-array (list rows cols) :element-type 'fixnum)))
    (loop :for x :below rows
       :do (loop :for y :below cols
              :do (setf (aref grid x y)
                        (compute-power-level (xy-to-rc x)
                                             (xy-to-rc y)
                                             serial-number))))
    grid))

(defun compute-square-total-power (grid r c rows cols
                                   &optional size total debug)
  "The square must be entirely within the 300x300 grid.

   SIZE and TOTAL are a memoization of the computing of a smaller square
   starting at the same position."
  (let ((x (rc-to-xy r))
        (y (rc-to-xy c))
        (p (or total 0)))
    (when (and (< x (+ (* -1 rows) 1 (array-dimension grid 0)))
               (< y (+ (* -1 rows) 1 (array-dimension grid 1))))
      (loop :for x-offset :from 0
         :repeat rows
         :do (when debug (format t "~&"))
         :do (loop :for y-offset :from 0
                :repeat cols
                :do (when (or (null size)
                              (and size (or (<= size x-offset)
                                            (<= size y-offset))))
                      (incf p (aref grid (+ x x-offset) (+ y y-offset))))
                :do (when debug
                      (if (or (null size) (and size (or (<= size x-offset)
                                                        (<= size y-offset))))
                          (format t "~3d "
                                  (aref grid (+ x x-offset) (+ y y-offset)))
                          (format t "  - ")))))
      p)))

(defun test/compute-square-total-power ()
  (every #'identity
         (list (= (compute-square-total-power (build-grid 18) 33 45 3 3) 29)
               (= (compute-square-total-power (build-grid 42) 21 61 3 3) 30))))

(defun find-max-3x3-square-total-power (grid-serial-number)
  (let* ((grid (build-grid grid-serial-number))
         (tmax nil)
         (cmax (cons nil nil)))
    (loop :for x :below (array-dimension grid 0)
       :do (loop :for y :below (array-dimension grid 1)
              :do (let ((total-power
                         (compute-square-total-power grid
                                                     (xy-to-rc x)
                                                     (xy-to-rc y)
                                                     3 3)))
                    (when (and total-power
                               (or (null tmax) (< tmax total-power)))
                      (setf tmax total-power)
                      (setf cmax (cons (xy-to-rc x) (xy-to-rc y)))))))
    (values cmax tmax)))

(defun test/find-max-square-total-power ()
  (every #'identity
         (list (equal (find-max-3x3-square-total-power 18) (cons 33 45))
               (equal (find-max-3x3-square-total-power 42) (cons 21 61)))))

(defun d11/p1 (&optional (input *d11/input*))
  (destructuring-bind (r . c)
      (find-max-3x3-square-total-power input)
    (format nil "~a,~a" r c)))

(defun copy-grid (grid)
  (let ((copy (make-array (array-dimensions grid))))
    (loop :for x :below (array-dimension grid 0)
       :do (loop :for y :below (array-dimension grid 1)
              :do (setf (aref copy x y) (aref grid x y))))
    copy))

(defun find-max-square-total-power (grid-serial-number &optional progress)
  (let* ((grid  (build-grid grid-serial-number))
         (cache (copy-grid grid))
         (tmax  nil)
         (cmax  (list nil nil nil)))
    (loop :for cached-size := 1 :then square-size
       :for square-size :from 1 :upto 300
       :do (when progress
             (format t ".~[~&~]" (mod square-size 80)))
       :do (loop :for x :below (array-dimension grid 0)
              :do (loop :for y :below (array-dimension grid 1)
                     :do (let* ((cached-power (aref cache x y))
                                (total-power
                                 (compute-square-total-power grid
                                                             (xy-to-rc x)
                                                             (xy-to-rc y)
                                                             square-size
                                                             square-size
                                                             cached-size
                                                             cached-power)))
                           ;; update the cache now
                           (setf (aref cache x y) total-power)

                           ;; is that a winner?
                           (when (and total-power
                                      (or (null tmax) (< tmax total-power)))
                             (setf tmax total-power)
                             (setf cmax (list (xy-to-rc x)
                                              (xy-to-rc y)
                                              square-size)))))))
    (values cmax tmax)))

(defun d11/p2 (&optional (input *d11/input*))
  (destructuring-bind (row col size)
      (find-max-square-total-power input)
    (format nil "~a,~a,~a" row col size)))


;;; VISUAL solution with McCLIM

(defun d11/summary ()
  (format t "Day 11: Chronal Charge~%")
  (format t "  Puzzle 1: Find the 3x3 square with the largest total power~%")
  (print-result (d11/p1))
  (format t "  Puzzle 2: Find the square with the largest total power~%")
  (print-result (d11/p2)))
