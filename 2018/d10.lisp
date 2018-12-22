(in-package :advent/2018)

(defparameter *d10/input*
  (uiop:read-file-lines
   (asdf:system-relative-pathname :advent "2018/d10.input") ))

(defparameter *d10/test*
  (with-input-from-string (s "position=< 9,  1> velocity=< 0,  2>
position=< 7,  0> velocity=<-1,  0>
position=< 3, -2> velocity=<-1,  1>
position=< 6, 10> velocity=<-2, -1>
position=< 2, -4> velocity=< 2,  2>
position=<-6, 10> velocity=< 2, -2>
position=< 1,  8> velocity=< 1, -1>
position=< 1,  7> velocity=< 1,  0>
position=<-3, 11> velocity=< 1, -2>
position=< 7,  6> velocity=<-1, -1>
position=<-2,  3> velocity=< 1,  0>
position=<-4,  3> velocity=< 2,  0>
position=<10, -3> velocity=<-1,  1>
position=< 5, 11> velocity=< 1, -2>
position=< 4,  7> velocity=< 0, -1>
position=< 8, -2> velocity=< 0,  1>
position=<15,  0> velocity=<-2,  0>
position=< 1,  6> velocity=< 1,  0>
position=< 8,  9> velocity=< 0, -1>
position=< 3,  3> velocity=<-1,  1>
position=< 0,  5> velocity=< 0, -1>
position=<-2,  2> velocity=< 2,  0>
position=< 5, -2> velocity=< 1,  2>
position=< 1,  4> velocity=< 2,  1>
position=<-2,  7> velocity=< 2, -2>
position=< 3,  6> velocity=<-1, -1>
position=< 5,  0> velocity=< 1,  0>
position=<-6,  0> velocity=< 2,  0>
position=< 5,  9> velocity=< 1, -2>
position=<14,  7> velocity=<-2,  0>
position=<-3,  6> velocity=< 2, -1>")
    (uiop:slurp-stream-lines s)))

(defstruct light x y vx vy)

(defun read-points (&optional (input *d10/test*))
  (loop :for line :in input
     :collect (let ((x
                     (parse-integer line
                                    :start (+ 1 (position #\< line))
                                    :junk-allowed t))
                    (y
                     (parse-integer line
                                    :start (+ 1 (position #\, line))
                                    :junk-allowed t))
                    (vx
                     (parse-integer line
                                    :start (+ 1 (position #\< line :from-end t))
                                    :junk-allowed t))
                    (vy
                     (parse-integer line
                                    :start (+ 1 (position #\, line :from-end t))
                                    :junk-allowed t)))
                (make-light :x x :y y :vx vx :vy vy))))

(defun move-seconds (seconds light-list)
  (loop :repeat seconds
     :do (loop :for light :in light-list
            :do (with-slots (x y vx vy) light
                  (setf x (+ x vx)
                        y (+ y vy))))))

;;; VISUAL solution with McCLIM

(defun d10/summary ()
  (format t "Day 10: The Stars Align~%")
  (format t "  Puzzle 1: KFLBHXGK~%")
  (format t "  Puzzle 2: ~as~%" (+ 9 (* 15 10) (* 25 60) (* 15 600))))
