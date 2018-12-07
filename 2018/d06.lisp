(in-package :advent/2018)

(defparameter *d6/input*
  (with-open-file (s (asdf:system-relative-pathname :advent "2018/d06.input")
                     :direction :input
                     :element-type 'character)
    (uiop:read-file-lines s)))

(defparameter *d6/test*
  (with-input-from-string (s
                           "1, 1
1, 6
8, 3
3, 4
5, 5
8, 9
")
    (uiop:slurp-stream-lines s)))

(defstruct coord x y finite-p area-size color)

(defun parse-coordinates (line)
  (let ((comma (position #\, line)))
    (make-coord :x (parse-integer line :end comma)
                :y (parse-integer line :start (+ 2 comma))
                :finite-p nil
                :area-size 0)))

(defun read-coordinates (lines)
  (mapcar #'parse-coordinates lines))

#|

1, 1
1, 6
8, 3
3, 4
5, 5
8, 9

aaaaa.cccc
aAaaa.cccc
aaaddecccc
aadddeccCc
..dDdeeccc
bb.deEeecc
bBb.eeee..
bbb.eeefff
bbb.eeffff
bbb.ffffFf

In this example, the areas of coordinates A, B, C, and F are infinite -
while not shown here, their areas extend forever outside the visible grid.
However, the areas of coordinates D and E are finite: D is closest to 9
locations, and E is closest to 17 (both including the coordinate's location
itself). Therefore, in this example, the size of the largest area is 17.

|#

(defun bounding-box (coords)
  (let (min-x min-y max-x max-y)
    (loop :for coord :in coords
       :do (let ((x (coord-x coord))
                 (y (coord-y coord)))
             (when (or (null min-x) (< x min-x)) (setf min-x x))
             (when (or (null max-x) (< max-x x)) (setf max-x x))
             (when (or (null min-y) (< y min-y)) (setf min-y y))
             (when (or (null max-y) (< max-y y)) (setf max-y y))))
    (list min-x min-y max-x max-y)))

(defun manhattan-distance (coord1 coord2)
  (+ (abs (- (coord-x coord1) (coord-x coord2)))
     (abs (- (coord-y coord1) (coord-y coord2)))))

(defun find-closest-coord (x y coords)
  (let* ((anchor (make-coord :x x :y y))
         (distances
          (sort
           (mapcar (lambda (p) (cons p (manhattan-distance p anchor))) coords)
           #'<
           :key #'cdr)))
    (if (= (cdr (first distances))
           (cdr (second distances)))
        nil
        (car (first distances)))))

(defun compute-closest-coords (coords bounding-box)
  (destructuring-bind (min-x min-y max-x max-y)
      bounding-box
    (let ((grid (make-hash-table :test 'equal)))
      (loop :for x :from min-x :upto max-x
         :do (loop :for y :from min-y :upto max-y
                :do (let ((closest-coord (find-closest-coord x y coords)))
                      (when closest-coord
                        (setf (gethash (cons x y) grid) closest-coord)
                        (incf (coord-area-size closest-coord))))))
      grid)))

(defun compute-finite-p (coords bounding-box grid)
  (destructuring-bind (min-x min-y max-x max-y)
      bounding-box
    (loop :for coord :in coords
       :do (let ((x (coord-x coord))
                 (y (coord-y coord)))
             (setf (coord-finite-p coord)
                   (and
                    (loop :for cx :from x :downto min-x
                       :thereis (not (eq coord (gethash (cons cx y) grid))))
                    (loop :for cx :from x :upto max-x
                       :thereis (not (eq coord (gethash (cons cx y) grid))))
                    (loop :for cy :from y :downto min-y
                       :thereis (not (eq coord (gethash (cons x cy) grid))))
                    (loop :for cy :from y :upto max-y
                       :thereis (not (eq coord (gethash (cons x cy) grid))))))))
    coords))

(defun compute-area-sizes (&optional (input *d6/test*))
  (let* ((coords       (read-coordinates input))
         (bounding-box (bounding-box coords))
         (grid         (compute-closest-coords coords bounding-box)))
    (compute-finite-p coords bounding-box grid)))

(defun largest-area-size (&optional (input *d6/test*))
  (let* ((finite-coord-areas
          (remove-if-not #'coord-finite-p (compute-area-sizes input)))
         (largest-area
          (first (sort finite-coord-areas #'> :key #'coord-area-size))))
    (values (coord-area-size largest-area)
            largest-area)))

(defun d6/p1/test (&optional (input *d6/test*))
  (= 17 (largest-area-size input)))

(defun d6/p1 (&optional (input *d6/input*))
  (largest-area-size input))

#|

On the other hand, if the coordinates are safe, maybe the best you can do is
try to find a region near as many coordinates as possible.

For example, suppose you want the sum of the Manhattan distance to all of
the coordinates to be less than 32. For each location, add up the distances
to all of the given coordinates; if the total of those distances is less
than 32, that location is within the desired region. Using the same
coordinates as above, the resulting region looks like this:

..........
.A........
..........
...###..C.
..#D###...
..###E#...
.B.###....
..........
..........
........F.

This region, which also includes coordinates D and E, has a total size of
16.

What is the size of the region containing all locations which have a total
distance to all given coordinates of less than 10000?

|#

(defun manhattan-sum (x y coords)
  (let* ((anchor (make-coord :x x :y y)))
    (reduce #'+ coords :key (lambda (p) (manhattan-distance p anchor)))))

(defun compute-manhattan-sum (coords bounding-box)
  (destructuring-bind (min-x min-y max-x max-y)
      bounding-box
    (let ((grid (make-hash-table :test 'equal)))
      (loop :for x :from min-x :upto max-x
         :do (loop :for y :from min-y :upto max-y
                :do (let ((sum (manhattan-sum x y coords)))
                      (setf (gethash (cons x y) grid) sum))))
      grid)))

(defun make-manhattan-sum-grid (&optional (input *d6/test*) coords)
  (let* ((coords       (or coords (read-coordinates input)))
         (bounding-box (bounding-box coords)))
    (compute-manhattan-sum coords bounding-box)))

(defun count-coords-within-distance-sum (grid &optional (threshold 32))
  (let ((count 0))
    (maphash (lambda (pos sum)
               (declare (ignore pos))
               (when (< sum threshold) (incf count)))
             grid)
    count))

(defun d6/p2 (&optional (input *d6/input*) (threshold 10000))
  (count-coords-within-distance-sum (make-manhattan-sum-grid input) threshold))

(defun d6/summary ()
  (format t "Day 6: Chronal Coordinates~%")
  (format t "  Puzzle 1: dangerous, keep away, largest finite area~%")
  (print-result (d6/p1))
  (format t "  Puzzle 2: safe, keep close, within manhattan sum threshold~%")
  (print-result (d6/p2)))
