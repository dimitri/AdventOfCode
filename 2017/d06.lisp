(in-package :advent/2017)

(defparameter *d6/input*
  (with-open-file (stream
                   (asdf:system-relative-pathname :advent "2017/d06.input"))
    (read-from-string
     (concatenate 'string "#(" (read-line stream nil nil) ")"))))

(defparameter *d6/test* #(0 2 7 0))

#|

The reallocation routine operates in cycles. In each cycle, it finds the
memory bank with the most blocks (ties won by the lowest-numbered memory
bank) and redistributes those blocks among the banks. To do this, it removes
all of the blocks from the selected bank, then moves to the next (by index)
memory bank and inserts one of the blocks. It continues doing this until it
runs out of blocks; if it reaches the last memory bank, it wraps around to
the first one.

The debugger would like to know how many redistributions can be done before
a blocks-in-banks configuration is produced that has been seen before.

|#

(defun find-target-block (memory)
  (let ((size   (length memory))
        (target 0))
    (loop :for i :from 1 :below size
       :do (when (< (aref memory target) (aref memory i))
             (setf target i)))
    target))

(defun reallocate-block (memory address)
  (let* ((size   (length memory))
         (result (make-array size :initial-contents memory))
         (count  (aref result address)))
    (setf (aref result address) 0)
    (loop :for block := (mod (+ 1 address) size) :then (mod (+ 1 block) size)
       :while (< 0 count)
       :do (incf (aref result block))
       :do (decf count))
    result))

(defun count-redistribution-before-cycle (memory)
  (let ((configurations (make-hash-table :test 'equalp)))
    (loop
       :for config := memory :then (let ((target (find-target-block config)))
                                     (reallocate-block config target))
       :while (= 1 (incf (gethash config configurations 0)))
       :count t :into loops
       :finally (return (values loops config)))))

(defun d6/p1/test ()
  (= 5 (count-redistribution-before-cycle *d6/test*)))

(defun d6/p1 ()
  (count-redistribution-before-cycle *d6/input*))

#|

Out of curiosity, the debugger would also like to know the size of the loop:
starting from a state that has already been seen, how many block
redistribution cycles must be performed before that same state is seen
again?

In the example above, 2 4 1 2 is seen again after four cycles, and so the
answer in that example would be 4.

How many cycles are in the infinite loop that arises from the configuration
in your puzzle input?
|#

(defun count-cyclicity (memory)
  (multiple-value-bind (count config)
      (count-redistribution-before-cycle memory)
    (declare (ignore count))
    (count-redistribution-before-cycle config)))

(defun d6/p2/test ()
  (= 4 (count-cyclicity *d6/test*)))

(defun d6/p2 ()
  (count-cyclicity *d6/input*))

(defun d6/summary ()
  (format t "Day 6: Memory Reallocation~%")
  (format t "  Puzzle 1: count redistribution cycles~%")
  (print-result (d6/p1))
  (format t "  Puzzle 2: cyclicity~%")
  (print-result (d6/p2)))
