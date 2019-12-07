(in-package :advent/2019)

(defparameter *input/d3*
  (uiop:read-file-string
   (asdf:system-relative-pathname :advent "2019/d03.input")))

(defparameter *test/input/d3*
  "R8,U5,L5,D3
U7,R6,D4,L4")

(defun manhattan-distance (coord1 &optional (coord2 '(0 . 0)))
  "https://en.wikipedia.org/wiki/Taxicab_geometry"
  (let ((r1 (car coord1))
        (c1 (cdr coord1))
        (r2 (car coord2))
        (c2 (cdr coord2)))
   (+ (abs (- r1 r2)) (abs (- c1 c2)))))

(defun read-wire-path-steps (string)
  (with-input-from-string (s string)
    (loop :for direction := (read-char s nil nil)
       :while direction
       :collect (cons direction
                      (parse-integer
                       (coerce (loop :for char := (read-char s nil nil)
                                  :while (and char (char/= char #\,))
                                  :collect char)
                               'string))))))

(defun follow-wire (w string wire-track-hash &optional (r-o 0) (c-o 0))
  (declare (type symbol w)
           (type string string)
           (type fixnum r-o c-o))
  (let (intersection-list
        (steps 0)
        (r r-o)
        (c c-o))
    (loop :for (direction . length) :in (read-wire-path-steps string)
       :do (let ((coords
                  (case direction
                    (#\U (loop :for cr :from (+ r 1) :to (+ r length)
                            :collect (cons cr c)))
                    (#\D (loop :for cr :from (- r 1) :downto (- r length)
                            :collect (cons cr c)))
                    (#\R (loop :for cc :from (+ c 1) :to (+ c length)
                            :collect (cons r cc)))
                    (#\L (loop :for cc :from (- c 1) :downto (- c length)
                            :collect (cons r cc))))))
             (loop :for coord :in coords
                :do (incf steps)
                :do (let ((wire-list (gethash coord wire-track-hash)))
                      (when (and (not (equal coord '(0 . 0)))
                                 wire-list
                                 (not (member w (mapcar #'car wire-list))))
                        (appendf intersection-list (list coord))))
                :do (appendf (gethash coord wire-track-hash)
                             (list (cons w steps)))
                :do (setf r (car coord)
                          c (cdr coord)))))
    intersection-list))

(defun compute-wire-intersections (multi-line-wire-tracks)
  (declare (type string multi-line-wire-tracks))
  (with-input-from-string (s multi-line-wire-tracks)
    (let ((tracks (make-hash-table :test 'equal))
          (wire-1 (read-line s))
          (wire-2 (read-line s)))
      (follow-wire 'a wire-1 tracks)
      (values (follow-wire 'b wire-2 tracks)
              tracks))))

(defun closest-intersection-point (multi-line-wire-tracks)
  (let* ((intersection-point-list
          (compute-wire-intersections multi-line-wire-tracks))
         (closest-point
          (first (sort intersection-point-list '< :key #'manhattan-distance))))
    (values (manhattan-distance closest-point)
            closest-point)))

(defun test/d3/p1 ()
    (every #'identity
           (list (= 6   (closest-intersection-point *test/input/d3*))
                 (= 159 (closest-intersection-point
                         "R75,D30,R83,U83,L12,D49,R71,U7,L72
U62,R66,U55,R34,D71,R55,D58,R83"))
                 (= 135 (closest-intersection-point
                         "R98,U47,R26,D63,R33,U87,L62,D20,R33,U53,R51
U98,R91,D20,R16,D67,R40,U7,R15,U6,R7")))))

(defun d3/p1 ()
  (closest-intersection-point *input/d3*))

(defun fewest-steps-to-intersection-point (multi-line-wire-tracks)
  (multiple-value-bind (intersection-point-list tracks-hash-table)
      (compute-wire-intersections multi-line-wire-tracks)
    (let* ((steps-to-intersection-point
            (loop :for coord :in intersection-point-list
               :for wire-steps := (mapcar #'cdr (gethash coord tracks-hash-table))
               :collect (cons coord (reduce #'+ wire-steps)))))
      (setf steps-to-intersection-point
            (sort steps-to-intersection-point '< :key #'cdr))
      (values (cdr (first steps-to-intersection-point))
              (first steps-to-intersection-point)))))

(defun d3/p2 ()
  (fewest-steps-to-intersection-point *input/d3*))

(defun d3/summary ()
  (format t "Day 3: Crossed Wires~%")
  (format t "  Puzzle 1: find distance to closest wire intersection~%")
  (print-result (d3/p1))
  (format t "  Puzzle 2: find fewest combined steps to reach intersection ~%")
  (print-result (d3/p2)))
