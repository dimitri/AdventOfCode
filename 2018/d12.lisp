(in-package :advent/2018)

(defparameter *d12/input*
  (uiop:read-file-string
   (asdf:system-relative-pathname :advent "2018/d12.input")))

(defparameter *d12/test* "initial state: #..#.#..##......###...###

...## => #
..#.. => #
.#... => #
.#.#. => #
.#.## => #
.##.. => #
.#### => #
#.#.# => #
#.### => #
##.#. => #
##.## => #
###.. => #
###.# => #
####. => #
")

(defstruct potline first-index pots)
(defstruct rule input output)

(defun parse-input (&optional (input *d12/test*))
  (with-input-from-string (s input)
    (let ((initial-state (make-potline :first-index 0
                                       :pots (subseq (read-line s) 15))))
      (read-line s)
      (values initial-state
              (loop :for line := (read-line s nil nil)
                 :while line
                 :collect (make-rule :input (subseq line 0 5)
                                     :output (subseq line 9 10)))))))

(defun make-sequence-at-pos (pos potline)
  (let* ((line (potline-pots potline))
         (size (length line)))
    (cond ((<= 2 pos (+ -3 size))
           (subseq line (+ -2 pos) (+ 3 pos)))
          ((< pos 2)
           (concatenate 'string
                        (loop :repeat (- 2 pos) :collect #\.)
                        (subseq line 0 (- 5 (- 2 pos)))))
          ((<= (+ -2 size) pos)
           (concatenate 'string
                        (subseq line (+ -2 pos))
                        (loop :repeat (+ 1 (- 2 (- size pos)))
                           :collect #\.))))))

(defun apply-rules-at-pos (pos potline rules)
  (flet ((rule-matches-at-pos (pos potline rule)
           (string= (potline-pots potline)
                    (rule-input rule)
                    :start1 (- pos 2)
                    :end1 (+ pos 3))))
   (loop :for rule :in rules
      :when (rule-matches-at-pos pos potline rule)
      :do (return (rule-output rule)))))

(defun trim-pots (potline)
  (let* ((line (potline-pots potline))
         (leading-dots (loop :for pos :below 4
                         :while (char= (aref line pos) #\.)
                         :count t)))
    (if (< 0 leading-dots)
        (progn
          (incf (potline-first-index potline) leading-dots)
          (setf (potline-pots potline)
                (subseq line
                        leading-dots
                        (+ 1 (position #\# line :from-end t)))))
        ;; in any case clean-up on the right side
        (setf (potline-pots potline) (string-right-trim "." line)))
    potline))

(defun apply-rules (potline rules)
  (let* ((input   (make-potline :first-index (+ -4 (potline-first-index potline))
                                :pots (concatenate 'string
                                                   "...."
                                                   (potline-pots potline)
                                                   "....")))
         (size   (length (potline-pots input)))
         (output (make-string size :initial-element #\.)))

    (loop :for pos :from 2 :below (+ -2 size)
       :do (let ((pot (apply-rules-at-pos pos input rules)))
             (when (and pot (string= pot "#"))
               (setf (aref output pos) (aref pot 0)))))

    (trim-pots (make-potline :first-index (potline-first-index input)
                             :pots output))))

(defun simulate-n-generations (potline rules n &optional (debug nil))
  (loop :repeat (+ 1 n)
     :for c :from 0
     :for previous-score := nil :then score
     :for new-potline := potline :then (apply-rules new-potline rules)
     :for score := (compute-sum-of-pots-indexes new-potline)
     :do (when debug
           (format t "~2d: ~a~45t~2d~%"
                            c
                            (potline-pots new-potline)
                            (potline-first-index new-potline)))
     :finally (return new-potline)))

(defun compute-sum-of-pots-indexes (potline)
  (loop :for char :across (potline-pots potline)
     :for index :from (potline-first-index potline)
     :when (char= char #\#)
     :sum index))

(defun d12/p1 (&optional (input *d12/input*) (generations 20))
  (multiple-value-bind (potline rules)
      (parse-input input)
    (let ((useful-rules (remove "." rules :key #'rule-output :test #'string=)))
      (compute-sum-of-pots-indexes
       (simulate-n-generations potline useful-rules generations)))))

(defun find-cycle (potline rules &optional (debug nil))
  (loop
     :for c :from 0
     :for previous-score := nil :then score
     :for previous-diff := nil :then diff
     :for new-potline := potline :then (apply-rules new-potline rules)
     :for score := (compute-sum-of-pots-indexes new-potline)
     :for diff := (when previous-score (- score previous-score))
     :do (when debug
           (format t "~2d: ~a ~a ~a~%"
                   c score previous-score diff))
     :until (and previous-diff (= previous-diff diff))
     :finally (return (values c score diff))))

(defun d12/p2 (&optional (input *d12/input*) (generations 50000000000))
  (multiple-value-bind (potline rules)
      (parse-input input)
    (let ((useful-rules (remove "." rules :key #'rule-output :test #'string=)))
      (multiple-value-bind (cycle-start score diff)
          (find-cycle potline useful-rules)
        (+ score (* diff (- generations cycle-start)))))))

(defun d12/summary ()
  (format t "Day 12: Subterranean Sustainability~%")
  (format t "  Puzzle 1: sum of indexes of pots with plants after 20 generations~%")
  (print-result (d12/p1))
  (format t "  Puzzle 2: sum of indexes of pots with plants after 50000000000 generations~%")
  (print-result (d12/p2)))
