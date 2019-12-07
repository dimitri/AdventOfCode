(in-package :advent/2019)

(defparameter *input/d2*
  (uiop:read-file-string
   (asdf:system-relative-pathname :advent "2019/d02.input")))

(defparameter *test/input/d2*
  "1,9,10,3,2,3,11,0,99,30,40,50")

(defun read-program (program-string &optional (separator #\,))
  (coerce
   (loop :for start := 0 :then (when end (+ 1 end))
      :for end := (when start (position separator program-string :start start))
      :while start
      :collect (parse-integer program-string :start start :end end))
   'vector))

(defun execute-program (program)
  (declare (type vector program))
  (loop :for position := 0 :then (+ position 4)
     :for opcode := (aref program position)
     :until (= opcode 99)
     :do (let ((arg1 (aref program (aref program (+ 1 position))))
               (arg2 (aref program (aref program (+ 2 position))))
               (rpos (aref program (+ 3 position))))
           (setf (aref program rpos)
                 (case opcode
                   (1 (+ arg1 arg2))
                   (2 (* arg1 arg2))))))
  program)

(defun test/d1/p1 ()
  (every #'identity
         (list (equalp #(3500 9 10 70 2 3 11 0 99 30 40 50)
                       (execute-program (read-program *test/input/d2*)))
               (equalp #(2 0 0 0 99)
                       (execute-program (read-program "1,0,0,0,99")))
               (equap #(2 3 0 6 99)
                      (execute-program (read-program "2,3,0,3,99")))
               (equalp #(2 4 4 5 99 9801)
                       (execute-program (read-program "2,4,4,5,99,0")))
               (equalp #(30 1 1 4 2 5 6 0 99)
                       (execute-program (read-program "1,1,1,4,99,5,6,0,99"))))))

(defun d2/p1 ()
  (let ((program (read-program *input/d2*)))
    (setf (aref program 1) 12)
    (setf (aref program 2) 2)
    (execute-program program)
    (aref program 0)))

(defun run-program-with-input (program arg1 arg2)
  (let ((new-program (copy-seq program)))
    (setf (aref new-program 1) arg1)
    (setf (aref new-program 2) arg2)
    (execute-program new-program)
    (aref new-program 0)))

(defun find-input-pair-for-result (program result)
  (loop :for a :from 0 :upto 99
     :do (loop :for b :from 0 :upto 99
            :when (= result (run-program-with-input program a b))
            :do (return-from find-input-pair-for-result (+ (* 100 a) b)))))

(defun d2/p2 ()
  (find-input-pair-for-result (read-program *input/d2*) 19690720))

(defun d2/summary ()
  (format t "Day 2: 1202 Program Alarm~%")
  (format t "  Puzzle 1: fix and execute program~%")
  (print-result (d2/p1))
  (format t "  Puzzle 2: find pair of input for result 19690720~%")
  (print-result (d2/p2)))
