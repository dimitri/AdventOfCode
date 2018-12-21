(in-package :advent/2018)

(defparameter *d7/input*
  (uiop:read-file-lines
   (asdf:system-relative-pathname :advent "2018/d07.input") ))

(defparameter *d7/test*
  (with-input-from-string (s
                           "Step C must be finished before step A can begin.
Step C must be finished before step F can begin.
Step A must be finished before step B can begin.
Step A must be finished before step D can begin.
Step B must be finished before step E can begin.
Step D must be finished before step E can begin.
Step F must be finished before step E can begin.
")
    (uiop:slurp-stream-lines s)))

(defstruct step name depends-on start-time end-time)

(defun read-steps (lines)
  (let (steps)
    (loop :for line :in lines
       :do (let* ((space (position #\Space line :start 5))
                  (name  (subseq line 5 space))
                  (pos   (+ 5 (search "step" line :from-end t)))
                  (dep   (subseq line pos (position #\Space line :start pos)))
                  (step-1 (find name steps :key #'step-name :test #'string=))
                  (step-2 (find dep steps :key #'step-name :test #'string=)))
             (unless step-1
               (push (make-step :name name) steps))
             (if step-2
                 (push name (step-depends-on step-2))
                 (push (make-step :name dep :depends-on (list name)) steps))))
    steps))

(defun find-available-steps (steps)
  (sort (remove-duplicates (remove-if-not #'null steps :key #'step-depends-on)
                           :key #'step-name
                           :test #'string=)
        #'string<
        :key #'step-name))

(defun remove-step (steps next-step)
  (let ((step-name (step-name next-step)))
   (loop :for step :in steps
      :do (setf (step-depends-on step)
                (remove step-name (step-depends-on step) :test #'string=))))
  (remove next-step steps))

(defun do-next-step (steps)
  (let* ((next-step (first (find-available-steps steps))))
    (list (step-name next-step) (remove-step steps next-step))))

(defun do-steps (steps)
  (when steps
    (destructuring-bind (first-step remaining-steps)
        (do-next-step steps)
      (list* first-step (do-steps remaining-steps)))))

(defun d7/p1/test (&optional (input *d7/test*))
  (apply #'concatenate 'string (do-steps (read-steps input))))

(defun d7/p1 (&optional (input *d7/input*))
  (apply #'concatenate 'string (do-steps (read-steps input))))

#|

As you're about to begin construction, four of the Elves offer to help. "The
sun will set soon; it'll go faster if we work together." Now, you need to
account for multiple people working on steps simultaneously. If multiple
steps are available, workers should still begin them in alphabetical order.

Each step takes 60 seconds plus an amount corresponding to its letter: A=1,
B=2, C=3, and so on. So, step A takes 60+1=61 seconds, while step Z takes
60+26=86 seconds. No time is required between steps.

To simplify things for the example, however, suppose you only have help from
one Elf (a total of two workers) and that each step takes 60 fewer
seconds (so that step A takes 1 second and step Z takes 26 seconds). Then,
using the same instructions as above, this is how each second would be
spent:

Second   Worker 1   Worker 2   Done
   0        C          .
   1        C          .
   2        C          .
   3        A          F       C
   4        B          F       CA
   5        B          F       CA
   6        D          F       CAB
   7        D          F       CAB
   8        D          F       CAB
   9        D          .       CABF
  10        E          .       CABFD
  11        E          .       CABFD
  12        E          .       CABFD
  13        E          .       CABFD
  14        E          .       CABFD
  15        .          .       CABFDE

Each row represents one second of time. The Second column identifies how
many seconds have passed as of the beginning of that second. Each worker
column shows the step that worker is currently doing (or . if they are
idle). The Done column shows completed steps.

Note that the order of the steps has changed; this is because steps now take
time to finish and multiple workers can begin multiple steps simultaneously.

In this example, it would take 15 seconds for two workers to complete these
steps.

With 5 workers and the 60+ second step durations described above, how long
will it take to complete all of the steps?

|#

(defparameter *d7/step-duration* 60)

(defmethod step-duration ((step step) &optional (duration *d7/step-duration*))
  (let ((step-char (aref (step-name step) 0)))
    (+ 1 duration (- (char-code step-char) (char-code #\A)))))

(defmethod compute-end-time ((step step) start-time)
  (setf (step-start-time step) start-time
        (step-end-time step)   (+ (step-duration step) start-time)))

(defmethod step-finished-p ((step step) current-time)
  (and (step-end-time step)
       (<= (step-end-time step) current-time)))

(defun assign-step-to-worker (workers w step steps current-time)
  (compute-end-time step current-time)
  (setf (aref workers w) step)
  (remove step steps))

(defun do-steps-concurrently (steps nb-workers &optional debug)
  (let* ((remaining-steps (copy-list steps))
         (workers         (make-array nb-workers
                                      :initial-element nil
                                      :element-type '(or null step)))
         (done            '()))
    (loop
       :for clock :from 0
       :do (loop :for w :below nb-workers
              :do (let ((w-step (aref workers w)))
                    (when (and w-step (step-finished-p w-step clock))
                      (push w-step done)
                      (remove-step remaining-steps w-step)
                      (setf (aref workers w) nil)
                      (setf w-step nil))

                    (when (null w-step)
                      (let ((next-step
                             (first (find-available-steps remaining-steps))))
                        (when next-step
                          (setf remaining-steps
                                (assign-step-to-worker workers w
                                                       next-step
                                                       remaining-steps
                                                       clock)))))))
       :do (when debug
             (format t "~2d    ~{~a~^    ~}      ~a~%"
                     clock
                     (mapcar (lambda (sw) (if sw (step-name sw) "."))
                             (coerce workers 'list))
                     (apply #'concatenate 'string
                            (mapcar #'step-name (reverse done)))))

       :until (notany #'identity workers)
       :finally (return clock))))

(defun d7/p2/test (&optional
                     (input *d7/test*) (nb-workers 2) (*d7/step-duration* 0))
  (do-steps-concurrently (read-steps input) nb-workers t))

(defun d7/p2 (&optional
                (input *d7/input*) (nb-workers 5) (*d7/step-duration* 60))
  (do-steps-concurrently (read-steps input) nb-workers))

(defun d7/summary ()
  (format t "Day 7: The Sum of Its Parts~%")
  (format t "  Puzzle 1: order instruction steps (dependency graph)~%")
  (print-result (d7/p1))
  (format t "  Puzzle 2: time to complete steps in parallel with 5 workers~%")
  (print-result (d7/p2)))
