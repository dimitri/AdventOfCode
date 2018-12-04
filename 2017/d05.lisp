(in-package :advent/2017)

(defparameter *input/d5/p1*
  (asdf:system-relative-pathname :advent "2017/d05.input"))

#|

The message includes a list of the offsets for each jump. Jumps are
relative: -1 moves to the previous instruction, and 2 skips the next one.
Start at the first instruction in the list. The goal is to follow the jumps
until one leads outside the list.

In addition, these instructions are a little strange; after each jump, the
offset of that instruction increases by 1. So, if you come across an offset
of 3, you would move three instructions forward, but change it to a 4 for
the next time it is encountered.

For example, consider the following list of jump offsets:

0
3
0
1
-3

Positive jumps ("forward") move downward; negative jumps move upward. For
legibility in this example, these offset values will be written all on one
line, with the current instruction marked in parentheses. The following
steps would be taken before an exit is found:

  -(0) 3  0  1  -3  - before we have taken any steps.

  -(1) 3  0  1  -3  - jump with offset 0 (that is, don't jump at all).
                      Fortunately, the instruction is then incremented to 1.

  - 2 (3) 0  1  -3  - step forward because of the instruction we just modified.
                      The first instruction is incremented again, now to 2.

  - 2  4  0  1 (-3) - jump all the way to the end; leave a 4 behind.

  - 2 (4) 0  1  -2  - go back to where we just were; increment -3 to -2.

  - 2  5  0  1  -2  - jump 4 steps forward, escaping the maze.

In this example, the exit is reached in 5 steps.

How many steps does it take to reach the exit?
|#

(defun read-instruction-vector (stream)
  (coerce
   (loop :for line := (read-line stream nil nil)
      :while line
      :collect (parse-integer line))
   'vector))

(defun follow-the-jumps (instruction-vector)
  (let ((size (length instruction-vector)))
    (loop
       :for pos := 0 :then (+ pos offset)
       :while (< -1 pos size)
       :for offset := (aref instruction-vector pos)
       :count t
       :do (incf (aref instruction-vector pos)))))

(defun test/d5/p1 ()
  (= 5
     (with-input-from-string (s "0
3
0
1
-3
")
       (follow-the-jumps (read-instruction-vector s)))))

(defun d5/p1 ()
  (with-open-file (input *input/d5/p1*
                         :direction :input
                         :element-type 'character)
    (follow-the-jumps (read-instruction-vector input))))

#|

Now, the jumps are even stranger: after each jump, if the offset was three
or more, instead decrease it by 1. Otherwise, increase it by 1 as before.

Using this rule with the above example, the process now takes 10 steps, and
the offset values after finding the exit are left as 2 3 2 3 -1.

How many steps does it now take to reach the exit?
|#

(defun follow-the-jumps-p2 (instruction-vector)
  (let ((size (length instruction-vector)))
    (loop
       :for pos := 0 :then (+ pos offset)
       :while (< -1 pos size)
       :for offset := (aref instruction-vector pos)
       :count t
       :do (if (<= 3 offset)
               (decf (aref instruction-vector pos))
               (incf (aref instruction-vector pos))))))

(defun test/d5/p1 ()
  (= 10
     (with-input-from-string (s "0
3
0
1
-3
")
       (follow-the-jumps-p2 (read-instruction-vector s)))))

(defun d5/p2 ()
  (with-open-file (input *input/d5/p1*
                         :direction :input
                         :element-type 'character)
    (follow-the-jumps-p2 (read-instruction-vector input))))

(defun d5/summary ()
  (format t "Day 5: A Maze of Twisty Trampolines, All Alike~%")
  (format t "  Puzzle 1: instructions and offsets~%")
  (print-result (d5/p1))
  (format t "  Puzzle 2: instructions and offsets, incf/decf~%")
  (print-result (d5/p2)))
