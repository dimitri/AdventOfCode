(in-package :advent/2018)

(defparameter *d13/input*
  (uiop:read-file-lines
   (asdf:system-relative-pathname :advent "2018/d13.input")))

(defparameter *d13/test*
  (with-input-from-string (s "
/->-\\
|   |  /----\\
| /-+--+-\\  |
| | |  | v  |
\\-+-/  \\-+--/
  \\-----/
")
    (rest (uiop:slurp-stream-lines s))))

(defparameter *d13/carts* '((#\^ :up   #\| -1 0)
                            (#\v :down #\| 1 0)
                            (#\< :left #\- 0 -1)
                            (#\> :right #\- 1 0)))

(defparameter *d13/intersection-choices* '(:left :straight :right :left))

(defstruct cart row col direction last-intersection-choice)
(defstruct track tracks carts)

(defun find-carts (&optional (input *d13/test*))
  (loop :for line :in input
     :while line
     :for r :from 0
     :append (loop :for char :across line
                :for c :from 0
                :for dir := (first (member char *d13/carts*
                                            :test #'char= :key #'first))
                :when dir
                :collect (make-cart :row r :col c :direction (second dir)))))

(defun read-track (&optional (input *d13/test*))
  (let ((carts (find-carts input))
        (track (coerce (loop :for line :in input
                          :collect (copy-seq line))
                       'vector)))
    (loop :for cart :in carts
       :do (setf (aref (aref track (cart-row cart)) (cart-col cart))
                 (third
                  (first
                   (member (cart-direction cart) *d13/carts* :key #'second)))))
    (make-track :tracks track :carts carts)))

(defun cart< (a b)
  (or (< (cart-row a) (cart-row b))
      (and (= (cart-row a) (cart-row b))
           (< (cart-col a) (cart-col b)))))

(defun cart-collide-p (a b)
  (and a b
       (= (cart-row a) (cart-row b))
       (= (cart-col a) (cart-col b))))

(defun collision-p (carts)
  (if (= 2 (length carts))
      (let ((cart1 (first carts))
            (cart2 (second carts)))
        (when (cart-collide-p cart1 cart2)
          (error 'collision-error :cart1 cart1 :cart2 cart2)))
      (let ((sorted-carts (sort (copy-seq carts) #'cart<)))
        (reduce (lambda (a b) (collision-p (list a b))) sorted-carts))))

(define-condition collision-error (error)
  ((cart1 :initarg :cart1 :accessor collision-error-cart1)
   (cart2 :initarg :cart2 :accessor collision-error-cart2))
  (:report
   (lambda (err stream)
     (with-slots (cart1) err
       (format stream "Collision detected at ~a,~a"
               (cart-col cart1) (cart-row cart1))))))

(defun move-one-tick (track &optional (debug t))
  (let ((carts (sort (copy-seq (track-carts track)) #'cart<)))
    (loop :for cart :in carts
       :do (when debug
             (format t "move ~a,~a ~a to "
                    (cart-row cart) (cart-col cart) (cart-direction cart)))
       :do (let ((dir (cart-direction cart)))
             (ecase dir
               (:left  (decf (cart-col cart)))
               (:right (incf (cart-col cart)))
               (:up    (decf (cart-row cart)))
               (:down  (incf (cart-row cart))))

             (setf (cart-direction cart)
                   (ecase (aref (aref (track-tracks track) (cart-row cart))
                                (cart-col cart))
                     (#\- (ecase dir
                            (:right :right)
                            (:left  :left)))
                     (#\| (ecase dir
                            (:up    :up)
                            (:down  :down)))
                     (#\\ (ecase dir
                            (:right :down)
                            (:down  :right)
                            (:left  :up)
                            (:up    :left)))
                     (#\/ (ecase dir
                            (:right :up)
                            (:down  :left)
                            (:left  :down)
                            (:up    :right)))
                     (#\+ (let* ((last-choice
                                  (or (cart-last-intersection-choice cart)
                                      :right))
                                 (choice
                                  (setf (cart-last-intersection-choice cart)
                                        (second
                                         (member last-choice
                                                 *d13/intersection-choices*)))))
                            (ecase choice
                              (:left  (ecase dir
                                        (:down  :right)
                                        (:up    :left)
                                        (:left  :down)
                                        (:right :up)))
                              (:right (ecase dir
                                        (:down  :left)
                                        (:up    :right)
                                        (:left  :up)
                                        (:right :down)))
                              (:straight dir)))))))
       :do (when debug
             (format t "~a,~a~%" (cart-row cart) (cart-col cart)))
       :do (collision-p (track-carts track)))
    track))

(defun find-first-crash (&optional (input *d13/test*))
  (loop :for track := (read-track input)
     :then
     (handler-case
         (move-one-tick track)
       (collision-error (e)
         (format t "~a~%" e)
         (format t "~{~a~^ ~}~%"
                 (mapcar (lambda (c)
                           (format nil "~a,~a" (cart-col c) (cart-row c)))
                         (sort (copy-seq (track-carts track)) #'cart<)))
         (let ((c1 (collision-error-cart1 e)))
          (return-from find-first-crash (cons (cart-col c1) (cart-row c1))))))))

(defun d13/p1 (&optional (input *d13/input*))
  )



(defun d13/p2 (&optional (input *d13/input*))
  )

(defun d13/summary ()
  (format t "Day 12: Subterranean Sustainability~%")
  (format t "  Puzzle 1: sum of indexes of pots with plants after 20 generations~%")
  (print-result (d13/p1))
  (format t "  Puzzle 2: sum of indexes of pots with plants after 50000000000 generations~%")
  (print-result (d13/p2)))
