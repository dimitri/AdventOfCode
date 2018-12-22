(in-package :advent/2018)

(defparameter *d9/input*
  (uiop:read-file-line
   (asdf:system-relative-pathname :advent "2018/d09.input") ))

(defparameter *d9/test* "9 players; last marble is worth 25 points")

#|

Then, each Elf takes a turn placing the lowest-numbered remaining marble
into the circle between the marbles that are 1 and 2 marbles clockwise of
the current marble. (When the circle is large enough, this means that there
is one marble between the marble that was just placed and the current
marble.) The marble that was just placed then becomes the current marble.

However, if the marble that is about to be placed has a number which is a
multiple of 23, something entirely different happens. First, the current
player keeps the marble they would have placed, adding it to their score. In
addition, the marble 7 marbles counter-clockwise from the current marble is
removed from the circle and also added to the current player's score. The
marble located immediately clockwise of the marble that was removed becomes
the new current marble.

[-] (0)
[1]  0 (1)
[2]  0 (2) 1
[3]  0  2  1 (3)
[4]  0 (4) 2  1  3
[5]  0  4  2 (5) 1  3
[6]  0  4  2  5  1 (6) 3
[7]  0  4  2  5  1  6  3 (7)
[8]  0 (8) 4  2  5  1  6  3  7
[9]  0  8  4 (9) 2  5  1  6  3  7
[1]  0  8  4  9  2(10) 5  1  6  3  7
[2]  0  8  4  9  2 10  5(11) 1  6  3  7
[3]  0  8  4  9  2 10  5 11  1(12) 6  3  7
[4]  0  8  4  9  2 10  5 11  1 12  6(13) 3  7
[5]  0  8  4  9  2 10  5 11  1 12  6 13  3(14) 7
[6]  0  8  4  9  2 10  5 11  1 12  6 13  3 14  7(15)
[7]  0(16) 8  4  9  2 10  5 11  1 12  6 13  3 14  7 15
[8]  0 16  8(17) 4  9  2 10  5 11  1 12  6 13  3 14  7 15
[9]  0 16  8 17  4(18) 9  2 10  5 11  1 12  6 13  3 14  7 15
[1]  0 16  8 17  4 18  9(19) 2 10  5 11  1 12  6 13  3 14  7 15
[2]  0 16  8 17  4 18  9 19  2(20)10  5 11  1 12  6 13  3 14  7 15
[3]  0 16  8 17  4 18  9 19  2 20 10(21) 5 11  1 12  6 13  3 14  7 15
[4]  0 16  8 17  4 18  9 19  2 20 10 21  5(22)11  1 12  6 13  3 14  7 15
[5]  0 16  8 17  4 18(19) 2 20 10 21  5 22 11  1 12  6 13  3 14  7 15
[6]  0 16  8 17  4 18 19  2(24)20 10 21  5 22 11  1 12  6 13  3 14  7 15
[7]  0 16  8 17  4 18 19  2 24 20(25)10 21  5 22 11  1 12  6 13  3 14  7 15

|#

;;;
;;; TODO: redo with a rope-like data structure for better perfs
;;;

(defstruct circle current-marble position size marble-list)

(defun read-input (&optional (input *d9/test*))
  ;; "9 players; last marble is worth 1618 points"
  (let* ((nb-players  (parse-integer input :junk-allowed t))
         (last-marble
          (parse-integer input
                         :start (position-if #'digit-char-p input
                                             :start (position #\; input))
                         :junk-allowed t)))
    (list nb-players last-marble)))

(defun new-circle ()
  (make-circle :current-marble 0 :position 0 :size 1 :marble-list (list 0)))

(defmethod debug-turn-circle ((circle circle) marble)
  (format t "[~d] ~{~a~^~}~%"
          marble
          (mapcar (lambda (n)
                    (if (= n (circle-current-marble circle))
                        (format nil "(~d)" n)
                        (format nil " ~d " n)))
                  (circle-marble-list circle))))

(defmethod insert-marble ((circle circle) marble)
  (if (= 1 (circle-size circle))
      (setf (circle-current-marble circle) 1
            (circle-position circle) 1
            (circle-size circle) 2
            (circle-marble-list circle) (list 0 1))

      (with-slots (current-marble position size marble-list)
          circle
        (cond ((= (+ 1 position) size)
               (setf current-marble marble
                     position       1
                     size           (+ 1 size)
                     marble-list    (list* (first marble-list)
                                           marble
                                           (rest marble-list))))

              (t
               (let* ((picksplit (min size (+ 2 position)))
                      (end       (if (= picksplit size) nil picksplit)))
                 (setf current-marble marble
                       position       (+ position 2)
                       size           (+ 1 size)
                       marble-list    (append
                                       (subseq marble-list 0 end)
                                       (list marble)
                                       (when end
                                         (subseq marble-list picksplit)))))))))
  circle)

(defmethod remove-marble-at-offset ((circle circle) offset)
  (with-slots (current-marble position size marble-list)
      circle
    (let* ((marble-position  (mod (+ position offset) size))
           (marble-to-remove (nth marble-position marble-list)))
      ;; the marble 7 marbles counter-clockwise from the current marble is
      ;; removed from the circle. The marble located immediately clockwise
      ;; of the marble that was removed becomes the new current marble.
      (setf current-marble (nth (mod (+ 1 marble-position) size) marble-list)
            position       marble-position
            size           (+ -1 size)
            marble-list    (append (subseq marble-list 0 marble-position)
                                   (let ((start (+ 1 marble-position)))
                                    (when (< start size)
                                      (subseq marble-list start)))))
      (values marble-to-remove circle))))

(defun play-rounds (nb-players rounds &optional debug)
  (let* ((players  (make-array nb-players))
         (plist    (loop :for i :from 1 :upto nb-players :collect i))
         (cplayers (nconc plist plist))
         (circle   (new-circle)))
    (loop :for marble :from 1 :upto rounds
       :for player :in cplayers
       :do (if (= 0 (mod marble 23))
               (incf (aref players (+ -1 player))
                     (+ marble (remove-marble-at-offset circle -7)))
               (insert-marble circle marble))
       :do (when debug
             (debug-turn-circle circle player)))

    (values (reduce #'max players) players)))

#|
10 players; last marble is worth 1618 points: high score is 8317
13 players; last marble is worth 7999 points: high score is 146373
17 players; last marble is worth 1104 points: high score is 2764
21 players; last marble is worth 6111 points: high score is 54718
30 players; last marble is worth 5807 points: high score is 37305
|#

(defun d9/p1/test ()
  (every #'identity (list (= 32     (play-rounds 9 25))
                          (= 8317   (play-rounds 10 1618))
                          (= 146373 (play-rounds 13 7999))
                          (= 2764   (play-rounds 17 1104))
                          (= 54718  (play-rounds 21 6111))
                          (= 37305  (play-rounds 30 5807)))))

(defun d9/p1 (&optional (input *d9/input*))
  (apply #'play-rounds (read-input input)))

#|

What would the new winning Elf's score be if the number of the last marble
were 100 times larger?

|#

(defstruct game position circle players scores current-player)
(defstruct marble value left right)

(defun create-game (players last-marble)
  (let ((marble  (make-marble :value 0 :left 0 :right 0))
        (vector  (make-array (+ 1 last-marble) :element-type '(or null marble)))
        (scores  (make-array players)))
    (setf (aref vector 0) marble)
    (make-game :position 0 :circle vector :players players :scores scores)))

(defmethod debug-turn ((game game) &optional (stream t))
  (with-slots (position circle current-player) game
    (format stream
            "[~d] ~{~a~}~%"
            (+ 1 current-player)
            (loop :for p := 0 :then (marble-right (aref circle p))
               :collect (if (= p position)
                            (format nil "(~d)" (marble-value (aref circle p)))
                            (format nil " ~d " (marble-value (aref circle p))))
               :until (zerop (marble-right (aref circle p)))))))

(defmethod next-player ((game game))
  (setf (game-current-player game)
        (if (game-current-player game)
            (mod (+ 1 (game-current-player game))
                 (game-players game))
            0)))

(defmethod position-at-offset ((game game) offset)
  (let ((branch (if (< offset 0) #'marble-left #'marble-right)))
    (loop :repeat (+ 1 (abs offset))
       :for position := (game-position game)
       :then (funcall branch (aref (game-circle game) position))
       :finally (return position))))

(defmethod insert-marble ((game game) value)
  (let* ((target-position (position-at-offset game 1))
         (target-marble   (aref (game-circle game) target-position))
         (left            target-position)
         (right           (marble-right target-marble))
         (new-marble      (make-marble :value value :left left :right right))
         (circle          (game-circle game)))

    (setf (game-position game) value)
    (setf (aref (game-circle game) value) new-marble)
    (setf (marble-right (aref circle left)) value)
    (setf (marble-left (aref circle right)) value)

    game))

(defmethod remove-marble-at-offset ((game game) offset)
  (let* ((target-position (position-at-offset game offset))
         (target-marble   (aref (game-circle game) target-position))
         (left            (marble-left target-marble))
         (right           (marble-right target-marble)))
    (setf (marble-right (aref (game-circle game) left)) right)
    (setf (marble-left (aref (game-circle game) right)) left)
    (setf (game-position game) right)
    (marble-value target-marble)))

(defun marble-mania (nb-players rounds &optional debug)
  (let ((game (create-game nb-players rounds)))
    (loop :for marble :from 1 :upto rounds
       :do (next-player game)
       :do (if (= 0 (mod marble 23))
               (incf (aref (game-scores game) (game-current-player game))
                     (+ marble (remove-marble-at-offset game -7)))
               (insert-marble game marble))
       :do (when debug (debug-turn game)))

    (reduce #'max (game-scores game))))

(defun d9/p1b/test ()
  (every #'identity (list (= 32     (marble-mania 9 25))
                          (= 8317   (marble-mania 10 1618))
                          (= 146373 (marble-mania 13 7999))
                          (= 2764   (marble-mania 17 1104))
                          (= 54718  (marble-mania 21 6111))
                          (= 37305  (marble-mania 30 5807)))))

(defun d9/p1b (&optional (input *d9/input*))
  (apply #'marble-mania (read-input input)))

(defun d9/p2 (&optional (input *d9/input*))
  ;; What would the new winning Elf's score be if the number of the last
  ;; marble were 100 times larger?
  (destructuring-bind (nb-players last-marble)
      (read-input input)
    (marble-mania nb-players (* 100 last-marble))))

(defun d9/summary ()
  (format t "Day 9: Marble Mania~%")
  (format t "  Puzzle 1: What is the winning Elf's score?~%")
  (print-result (d9/p1b))
  (format t "  Puzzle 2: What is the score with 100 times as many marbles?~%")
  (print-result (d9/p2)))


