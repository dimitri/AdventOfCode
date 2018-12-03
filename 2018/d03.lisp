(in-package :advent/2018)

#|

All claims have an ID and consist of a single rectangle with edges parallel
to the edges of the fabric. Each claim's rectangle is defined as follows:

  - The number of inches between the left edge of the fabric and the left
    edge of the rectangle.

  - The number of inches between the top edge of the fabric and the top edge
    of the rectangle.

The width of the rectangle in inches.
The height of the rectangle in inches.

#1 @ 1,3: 4x4
#2 @ 3,1: 4x4
#3 @ 5,5: 2x2
|#

(defparameter *input/d3/p1*
  (asdf:system-relative-pathname :advent "2018/d03.input"))

(defstruct claim id left top width height)

(defun parse-claim (claim-string)
  "Parse a claim string and returns a claim structure instance."
  (declare (type string claim-string))
  (multiple-value-bind (id position)
      (parse-integer claim-string :start 1 :junk-allowed t)
    ;; now skip the space-at-space characters
    (let* ((next-position  (+ 3 position))
           (comma-position (position #\, claim-string :start next-position))
           (colon-position (position #\: claim-string :start comma-position))
           (x-position     (position #\x claim-string :start colon-position)))
      (make-claim :id id
                  :left (parse-integer claim-string
                                       :start next-position
                                       :end comma-position)
                  :top (parse-integer claim-string
                                      :start (+ 1 comma-position)
                                      :end colon-position)
                  :width (parse-integer claim-string
                                        :start (+ 1 colon-position)
                                        :end x-position)
                  :height (parse-integer claim-string
                                         :start (+ 1 x-position))))))

(defun update-fabric-claims (fabric claim)
  (loop :for row :from (claim-top claim) :repeat (claim-height claim)
     :do (loop :for col :from (claim-left claim) :repeat (claim-width claim)
            :do (incf (gethash (cons row col) fabric 0)))))

(defun count-overclaimed-squares (fabric)
  (loop :for count-claims :being :the :hash-value :of fabric
     :count (< 1 count-claims)))

(defun map-claim-input (thunk filename)
  (with-open-file (input filename
                         :direction :input
                         :element-type 'character)
    (loop :for line := (read-line input nil nil)
       :while line
       :do (let ((claim (parse-claim line)))
             (funcall thunk claim)))))

(defun make-fabric-from-claims-stream (filename)
  (let ((fabric (make-hash-table :test 'equal)))
    (map-claim-input (lambda (claim) (update-fabric-claims fabric claim))
                     filename)
    fabric))

(defun test/d3/p1 ()
  (= 4
     (with-input-from-string (s "#1 @ 1,3: 4x4
#2 @ 3,1: 4x4
#3 @ 5,5: 2x2
")
       (count-overclaimed-squares (make-fabric-from-claims-stream s)))))

(defun d3/p1 ()
  (count-overclaimed-squares (make-fabric-from-claims-stream *input/d3/p1*)))

(defun claimed-only-once (fabric claim)
  (loop :with all-ones := t
     :for row :from (claim-top claim) :repeat (claim-height claim)
     :do (loop :for col :from (claim-left claim) :repeat (claim-width claim)
            :do (setf all-ones (and all-ones
                                    (= 1 (gethash (cons row col) fabric)))))
     :finally (return all-ones)))

(defun d3/p2 ()
  (let ((fabric (make-fabric-from-claims-stream *input/d3/p1*)))
    (map-claim-input (lambda (claim)
                       (when (claimed-only-once fabric claim)
                         (return-from d3/p2 (claim-id claim))))
                     *input/d3/p1*)))

(defun d3/summary ()
  (format t "Day 3: No Matter How You Slice It~%")
  (format t "  Puzzle 1: find squares claimed more than once~%")
  (print-result (d3/p1))
  (format t "  Puzzle 2: find the only claim asked of only once~%")
  (print-result (d3/p2)))
