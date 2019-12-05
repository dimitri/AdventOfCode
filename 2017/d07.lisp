(in-package :advent/2017)

(defparameter *d7/input*
  (uiop:read-file-lines
   (asdf:system-relative-pathname :advent "2017/d07.input")))

(defparameter *d7/test*
  (with-input-from-string (s
                           "pbga (66)
xhth (57)
ebii (61)
havc (66)
ktlj (57)
fwft (72) -> ktlj, cntj, xhth
qoyq (66)
padx (45) -> pbga, havc, qoyq
tknk (41) -> ugml, padx, fwft
jptl (61)
ugml (68) -> gyxo, ebii, jptl
gyxo (61)
cntj (57)")
    (uiop:slurp-stream-lines s)))

#|

One program at the bottom supports the entire tower. It's holding a large
disc, and on the disc are balanced several more sub-towers. At the bottom of
these sub-towers, standing on the bottom disc, are other programs, each
holding their own disc, and so on. At the very tops of these
sub-sub-sub-...-towers, many programs stand simply keeping the disc below
them balanced but with no disc of their own.

In this example, tknk is at the bottom of the tower (the bottom program),
and is holding up ugml, padx, and fwft. Those programs are, in turn, holding
up other programs; in this example, none of those programs are holding up
any other programs, and are all the tops of their own towers. (The actual
tower balancing in front of you is much larger.)

|#

(defstruct program name weight sub-weight sub-programs)

(defun parse-program-line (line)
  (let* ((size   (length line))
         (space  (position #\Space line))
         (name   (subseq line 0 space))
         (close  (position #\) line :start space))
         (weight (parse-integer line :start (+ space 2) :end close))
         (arrow  (position #\> line :start close))
         (subps  (when arrow
                   (loop
                      :for start := (+ 2 arrow) :then (+ 2 end)
                      :for end := (or (position #\, line :start start) size)
                      :collect (subseq line start end)
                      :while (< end size)))))
    (make-program :name name :weight weight :sub-programs subps)))

(defun program-sub-program-p (p1 p2)
  (member (program-name p1) (program-sub-programs p2) :test #'string=))

(defun find-tower-root (from-program program-list)
  (when from-program
    (let ((candidate
           (find from-program program-list :test #'program-sub-program-p)))
      (or (find-tower-root candidate program-list) candidate))))

(defun propagate-programs (tower-root program-list)
  (setf (program-sub-programs tower-root)
        (loop :for name :in (program-sub-programs tower-root)
           :collect (let ((subp (find name
                                      program-list
                                      :key #'program-name
                                      :test #'string=)))
                      (propagate-programs subp program-list))))
  tower-root)

(defun build-tower (program-lines)
  (let* ((programs   (mapcar #'parse-program-line program-lines))
         (tower-root (find-tower-root (first programs) programs)))
    (propagate-programs tower-root programs)))

(defun d7/p1/test ()
  (string= "tknk" (program-name (build-tower *d7/test*))))

(defun d7/p1 ()
  (program-name (build-tower *d7/input*)))

#|

For any program holding a disc, each program standing on that disc forms a
sub-tower. Each of those sub-towers are supposed to be the same weight, or
the disc itself isn't balanced. The weight of a tower is the sum of the
weights of the programs in that tower.

Given that exactly one program is the wrong weight, what would its weight
need to be to balance the entire tower?

|#

(defun count-programs (tower)
  (reduce #'+
          (program-sub-programs tower)
          :key #'count-programs
          :initial-value 1))

(defun program-total-weight (program)
  (setf (program-sub-weight program)
        (reduce #'+
                (program-sub-programs program)
                :key #'program-total-weight
                :initial-value (program-weight program))))

(defun program-overweight (tower)
  (setf (program-sub-programs tower)
        (sort (program-sub-programs tower) #'> :key #'program-total-weight))

  (let* ((first  (first (program-sub-programs tower)))
         (second (second (program-sub-programs tower)))
         (extra  (- (program-sub-weight first) (program-sub-weight second))))
    (values (- (program-weight first) extra)
            extra
            (program-name first))))

(defun d7/p2/test ()
  (= 60 (program-overweight (build-tower *d7/test*))))

(defun d7/p2 ()
  (program-overweight (build-tower *d7/input*)))

(defun d7/summary ()
  (format t "Day 7: Recursive Circus~%")
  (format t "  Puzzle 1: What is the name of the bottom program?~%")
  (print-result (d7/p1))
  (format t "  Puzzle 2: Balance the tower weights~%")
  (print-result (d7/p2)))
