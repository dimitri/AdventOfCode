(in-package :advent/2018)

(defparameter *d8/input*
  (uiop:read-file-line
   (asdf:system-relative-pathname :advent "2018/d08.input") ))

(defparameter *d8/test*  "2 3 0 3 10 11 12 1 1 0 1 99 2 1 1 2")

#|

Specifically, a node consists of:

  - A header, which is always exactly two numbers:
    - The quantity of child nodes.
    -  The quantity of metadata entries.
  - Zero or more child nodes (as specified in the header).
  - One or more metadata entries (as specified in the header).

Each child node is itself a node that has its own header, child nodes, and
metadata. For example:

2 3 0 3 10 11 12 1 1 0 1 99 2 1 1 2
A----------------------------------
    B----------- C-----------
                     D-----

The first check done on the license file is to simply add up all of the
metadata entries. In this example, that sum is 1+1+2+10+11+12+2+99=138.

|#

(defstruct node children-count metadata-count children metadata)

(defun string-to-number-vector (string)
  (with-input-from-string (s string)
    (let ((size (length string))
          (pos  0))
      (coerce
       (loop
          :for n := (multiple-value-bind (number position)
                        (parse-integer string :start pos :junk-allowed t)
                      (setf pos (+ 1 position))
                      number)
          :collect n
          :while (< pos size))
       'vector))))

(defun read-node (node numbers position)
  (setf (node-children-count node) (aref numbers position))
  (setf (node-metadata-count node) (aref numbers (+ 1 position)))

  (let ((metadata-start-pos
         (if (< 0 (node-children-count node))
             (let ((c-pos (+ 2 position)))
               (loop :for c :below (node-children-count node)
                  :for child := (make-node)
                  :do (setf c-pos (read-node child numbers c-pos))
                  :do (push child (node-children node))
                  :finally (progn
                             (setf (node-children node)
                                   (reverse (node-children node)))
                             (return c-pos))))
             (+ 2 position))))

    (setf (node-metadata node)
          (loop :for i :below (node-metadata-count node)
             :collect (aref numbers (+ i metadata-start-pos))))

    (+ metadata-start-pos (node-metadata-count node))))

(defun read-nodes (&optional (input *d8/test*))
  (let* ((numbers (string-to-number-vector input))
         (root    (make-node)))
    (read-node root numbers 0)
    root))

(defun sum-metadata (root)
  (+ (reduce #'+ (node-metadata root))
     (reduce #'+ (node-children root) :key #'sum-metadata)))

(defun d8/p1 (&optional (input *d8/input*))
  (sum-metadata (read-nodes input)))

#|

If a node has no child nodes, its value is the sum of its metadata entries.

However, if a node does have child nodes, the metadata entries become
indexes which refer to those child nodes.

|#

(defun node-value (node)
  (if (= 0 (node-children-count node))
      (reduce #'+ (node-metadata node))
      (loop :for index :in (node-metadata node)
         :for idx := (+ -1 index)
         :sum (if (< idx (node-children-count node))
                  (node-value (nth (+ -1 index) (node-children node)))
                  0))))

(defun d8/p2 (&optional (input *d8/input*))
  (node-value (read-nodes input)))

(defun d8/summary ()
  (format t "Day 8: Memory Maneuver~%")
  (format t "  Puzzle 1: read software license file~%")
  (print-result (d8/p1))
  (format t "  Puzzle 2: value of the root node~%")
  (print-result (d8/p2)))


