(in-package :advent/2018)

(defparameter *d17/input*
  (uiop:read-file-lines
   (asdf:system-relative-pathname :advent "2018/d17.input")))

(defparameter *d17/test*
  (with-input-from-string (s "x=495, y=2..7
y=7, x=495..501
x=501, y=3..7
x=498, y=2..4
x=506, y=1..2
x=498, y=10..13
x=505, y=10..13
y=13, x=498..504
x=503, y=9..12
x=501, y=9..12")
    (uiop:slurp-stream-lines s)))

#|
x=495, y=2..7
y=7, x=495..501
x=501, y=3..7
x=498, y=2..4
x=506, y=1..2
x=498, y=10..13
x=504, y=10..13
y=13, x=498..504
|#

(defparameter *d17/spring* (cons 500 0))

(defun parse-clay-definition (string)
  (flet ((parse-xy (string start end)
           (let* ((equal-pos (position #\= string :start start :end end))
                  (x-or-y (intern
                           (string-upcase (subseq string start equal-pos))))
                  (range-pos
                   (search ".." string :start2 (+ equal-pos 1) :end2 end)))
             (cons x-or-y
                   (if range-pos
                       (let ((range-start (parse-integer string
                                                         :start (+ equal-pos 1)
                                                         :end range-pos))
                             (range-end   (parse-integer string
                                                         :start (+ range-pos 2)
                                                         :end end)))
                         (loop :for x :from range-start :upto range-end
                            :collect x))
                       (list (parse-integer string
                                            :start (+ equal-pos 1)
                                            :end end)))))))
    (let* ((comma-pos (position #\, string))
           (coord1    (parse-xy string 0 comma-pos))
           (coord2    (parse-xy string (+ 2 comma-pos) (length string))))
      (if (and (eq 'x (car coord1)) (eq 'y (car coord2)))
          (cons (cdr coord1) (cdr coord2))
          (cons (cdr coord2) (cdr coord1))))))

(defstruct slice min-x max-x min-y max-y grid)

(defun slice-y-to-row (slice y) (- y (slice-min-y slice)))
(defun slice-x-to-col (slice x) (- x (slice-min-x slice)))
(defun slice-row-to-y (slice r) (+ r (slice-min-y slice)))
(defun slice-col-to-x (slice c) (+ c (slice-min-x slice)))

(defun slice-aref (slice y x) (aref (slice-grid slice)
                                    (slice-y-to-row slice y)
                                    (slice-x-to-col slice x)))

(defun (setf slice-aref) (value slice y x)
  (setf (aref (slice-grid slice)
              (slice-y-to-row slice y)
              (slice-x-to-col slice x))
        value))

(defun print-slice (stream slice)
  (loop :for r :below (array-dimension (slice-grid slice) 0)
     :do (format stream "~&~3d  " (slice-row-to-y slice r))
     :do (loop :for c :below (array-dimension (slice-grid slice) 1)
            :do (format stream "~c" (aref (slice-grid slice) r c)))
     :do (format stream "~%")))

(defun parse-clay-definitions (&optional
                                 (input *d17/test*)
                                 (spring *d17/spring*))
  (let ((min-x (car spring))
        (max-x (car spring))
        (min-y (cdr spring))
        (max-y (cdr spring)))
    (let* ((clays
            (loop :for line :in input
               :for def := (parse-clay-definition line)
               :do (loop :for x :in (car def)
                      :when (or (null min-x) (< x min-x)) :do (setf min-x x)
                      :when (or (null max-x) (< max-x x)) :do (setf max-x x))
               :do (loop :for y :in (cdr def)
                      :when (or (null min-y) (< y min-y)) :do (setf min-y y)
                      :when (or (null max-y) (< max-y y)) :do (setf max-y y))
               :collect def))
           (slice (make-slice  :min-x min-x
                               :max-x max-x
                               :min-y min-y
                               :max-y max-y
                               :grid (make-array (list (+ 2 (- max-y min-y))
                                                       (+ 1 (- max-x min-x)))
                                                 :element-type 'char
                                                 :initial-element #\.))))
      (loop :for (x-list . y-list) :in clays
         :do (loop :for x :in x-list
                :do (loop :for y :in y-list
                       :do (setf (slice-aref slice y x) #\#))))
      (setf (slice-aref slice (cdr spring) (car spring)) #\+)
      slice)))

(defun cell-stops-water-p (slice y x)
  (member (slice-aref slice y x) (list #\# #\~) :test #'char=))

(defun in-clay-reservoir-p (slice spring)
  (flet ((closed-to-the-direction (slice spring direction)
           (let ((y (cdr spring)))
             (loop :for x := (+ direction (car spring)) :then (+ direction x)
                :while (and (<= (slice-min-x slice) x)
                            (<= x (slice-max-x slice))
                            (cell-stops-water-p slice (+ 1 y) x)
                            (member (slice-aref slice y x) (list #\| #\.)
                                    :test #'char=))
                :finally (return (and
                                  (<= (slice-min-x slice) x)
                                  (<= x (slice-max-x slice))
                                  (char= #\# (slice-aref slice y x))))))))
    (and (closed-to-the-direction slice spring -1)
         (closed-to-the-direction slice spring +1))))

(defun fill-in-reservoir-line (slice spring)
  (flet ((fill-in-direction (slice spring direction)
           (let ((y (cdr spring)))
             (loop :for x := (car spring) :then (+ x direction)
                :until (char= #\# (slice-aref slice y x))
                :do (setf (slice-aref slice y x) #\~)))))
    (fill-in-direction slice spring -1)
    (fill-in-direction slice spring +1)))

(defun run-water (slice &optional (spring *d17/spring*))
  (let ((spring-x (car spring))
        (spring-y (cdr spring)))
    (unless (or (> spring-y (slice-max-y slice))
                (< spring-y (slice-min-y slice))
                (< spring-x (slice-min-x slice))
                (> spring-x (slice-max-x slice))
                (not (member (slice-aref slice spring-y spring-x)
                             (list #\. #\+)
                             :test #'char=)))
      (ecase (slice-aref slice (+ 1 spring-y) spring-x)
        (#\|
         (setf (slice-aref slice spring-y spring-x) #\|))
        (#\.
         (run-water slice (cons spring-x (+ 1 spring-y)))
         (if (in-clay-reservoir-p slice spring)
             (fill-in-reservoir-line slice spring)
             (progn
               (when (char= #\~ (slice-aref slice (+ 1 spring-y) spring-x))
                 (run-water slice spring))
               (unless (member (slice-aref slice spring-y spring-x)
                               (list #\+ #\~))
                 (setf (slice-aref slice spring-y spring-x) #\|)))))

        ((#\# #\~)
         (if (in-clay-reservoir-p slice spring)
             (fill-in-reservoir-line slice spring)
             (progn
               (setf (slice-aref slice spring-y spring-x) #\|)
               (run-water slice (cons (+ -1 spring-x) spring-y))
               (run-water slice (cons (+ 1 spring-x) spring-y))))))))
  slice)

(defun count-tiles-reached (slice)
  (loop :for r :below (array-dimension (slice-grid slice) 0)
     :sum (loop :for c :below (array-dimension (slice-grid slice) 1)
             :count (member (aref (slice-grid slice) r c)
                            (list #\~ #\|)
                            :test #'char=))))

(defun d17/p1 (&optional (input *d17/input*) (spring *d17/spring*))
  (let ((slice (parse-clay-definitions input)))
    (run-water slice spring)
    (print-slice t slice)
    (count-tiles-reached slice)))


(defun d17/p2 (&optional (input *d17/input*))
  )

(defun d17/summary ()
  (format t "Day 17: Reservoir Research~%")
  (format t "  Puzzle 1: ~%")
  (print-result (d17/p1))
  (format t "  Puzzle 2: ~%")
  (print-result (d17/p2)))
