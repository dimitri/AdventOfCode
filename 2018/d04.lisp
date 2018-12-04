(in-package :advent/2018)

#|

For example, consider the following records, which have already been
organized into chronological order:

[1518-11-01 00:00] Guard #10 begins shift
[1518-11-01 00:05] falls asleep
[1518-11-01 00:25] wakes up
[1518-11-01 00:30] falls asleep
[1518-11-01 00:55] wakes up
[1518-11-01 23:58] Guard #99 begins shift
[1518-11-02 00:40] falls asleep
[1518-11-02 00:50] wakes up
[1518-11-03 00:05] Guard #10 begins shift
[1518-11-03 00:24] falls asleep
[1518-11-03 00:29] wakes up
[1518-11-04 00:02] Guard #99 begins shift
[1518-11-04 00:36] falls asleep
[1518-11-04 00:46] wakes up
[1518-11-05 00:03] Guard #99 begins shift
[1518-11-05 00:45] falls asleep
[1518-11-05 00:55] wakes up

Note that guards count as asleep on the minute they fall asleep, and they
count as awake on the minute they wake up. For example, because Guard #10
wakes up at 00:25 on 1518-11-01, minute 25 is marked as awake.

Strategy 1: Find the guard that has the most minutes asleep. What minute
does that guard spend asleep the most?

What is the ID of the guard you chose multiplied by the minute you
chose? (In the above example, the answer would be 10 * 24 = 240.)

|#

(defparameter *input/d4/p1*
  (asdf:system-relative-pathname :advent "2018/d04.input"))

(defparameter *input/d4/test*
  "[1518-11-01 00:00] Guard #10 begins shift
[1518-11-01 00:05] falls asleep
[1518-11-01 00:25] wakes up
[1518-11-01 00:30] falls asleep
[1518-11-01 00:55] wakes up
[1518-11-01 23:58] Guard #99 begins shift
[1518-11-02 00:40] falls asleep
[1518-11-02 00:50] wakes up
[1518-11-03 00:05] Guard #10 begins shift
[1518-11-03 00:24] falls asleep
[1518-11-03 00:29] wakes up
[1518-11-04 00:02] Guard #99 begins shift
[1518-11-04 00:36] falls asleep
[1518-11-04 00:46] wakes up
[1518-11-05 00:03] Guard #99 begins shift
[1518-11-05 00:45] falls asleep
[1518-11-05 00:55] wakes up")

(defstruct record
  (date       0 :type fixnum)
  (minute     0 :type fixnum)
  (guard-id nil :type (or null fixnum))
  (event    nil :type (or null symbol)))

(defstruct guard
  id records reversed minutes-asleep periods-asleep most-sleepy-minute)

(defun parse-record (line)
  (let* ((date  (parse-integer (remove #\- (subseq line 1 11))))
         (hour  (parse-integer (subseq line 12 14)))
         (min   (parse-integer (subseq line 15 17)))
         (desc  (subseq line 19))
         (guard (when (search "Guard" desc :test #'string=)
                  (parse-integer line :start 26 :junk-allowed t)))
         (event (cond ((search "falls asleep" line :start2 19 :test #'string=)
                       :asleep)
                      ((search "wakes up" line :start2 19 :test #'string=)
                       :awaken))))
    (make-record :date (+ (* 10000 date) (* 100 hour) min)
                 :minute min
                 :guard-id guard
                 :event (or event (when guard :shift)))))

(defun load-records (stream)
  (loop :for line := (read-line stream nil nil)
     :while line
     :collect (parse-record line)))

(defun records-per-guard (records)
  "Returns a hash table with a guard ID as key and an ordered list of
   records as value."
  (let ((guards   (make-hash-table :test 'eql))
        (records  (sort records #'< :key #'record-date)))
    (loop :for record :in records
       :for current-guard-id := (or (record-guard-id record) current-guard-id)
       :do (let ((guard (gethash current-guard-id guards)))
             (if guard
                 (push record (guard-records guard))
                 (setf (gethash current-guard-id guards)
                       (make-guard :id current-guard-id
                                   :records nil
                                   :reversed t
                                   :minutes-asleep 0)))))
    (loop :for guard :being :the :hash-values :of guards
       :do (multiple-value-bind (periods minutes)
               (compute-minutes-asleep guard)
             (setf (guard-periods-asleep guard) periods
                   (guard-minutes-asleep guard) minutes)))
    guards))

(defun compute-minutes-asleep (guard)
  (when (guard-reversed guard)
    (setf (guard-records guard)   (reverse (guard-records guard))
          (guard-reversed guard) nil))
  (loop :for (start end)
     :on (loop :for record :in (guard-records guard)
            :when (member (record-event record) '(:asleep :awaken))
            :collect (record-minute record))
     :by #'cddr
     :collect (cons start (+ -1 end)) :into periods
     :sum (- end start 1) :into minutes
     :finally (return (values periods minutes))))

(defun compute-most-sleepy-minute (guard)
  (let ((most-frequent (cons -1 -1))
        (frequencies   (make-hash-table :test 'eql)))
    (loop :for (start . end) :in (guard-periods-asleep guard)
       :do (loop :for minute :from start :upto end
              :do (incf (gethash minute frequencies 0))))
    (maphash (lambda (minute freq)
               (when (< (cdr most-frequent) freq)
                 (setf most-frequent (cons minute freq))))
             frequencies)
    most-frequent))

(defun find-most-asleep-guard (guards)
  (let (guards-minutes)
    (maphash (lambda (guard-id guard)
               (push (cons guard-id (guard-minutes-asleep guard))
                     guards-minutes))
             guards)
    (let* ((most-asleep-guard-id
            (car
             (reduce (lambda (acc atom)
                       (if (> (cdr atom) (cdr acc)) atom acc))
                     guards-minutes
                     :initial-value '(not-found . 0))))
           (most-asleep-guard (gethash most-asleep-guard-id guards)))
      (setf (guard-most-sleepy-minute most-asleep-guard)
            (compute-most-sleepy-minute most-asleep-guard))
      most-asleep-guard)))

(defun compute-p1-answer (guard)
  (* (guard-id guard) (car (guard-most-sleepy-minute guard))))

(defun d4/p1/test ()
  (with-input-from-string (s *input/d4/test*)
    (find-most-asleep-guard (records-per-guard (load-records s)))))

(defun d4/p1 ()
  (with-open-file (input *input/d4/p1*
                         :direction :input
                         :element-type 'character)
    (compute-p1-answer
     (find-most-asleep-guard (records-per-guard (load-records input))))))

#|
--- Part Two ---

Strategy 2: Of all guards, which guard is most frequently asleep on the same
minute?

In the example above, Guard #99 spent minute 45 asleep more than any other
guard or minute - three times in total. (In all other cases, any guard spent
any minute asleep at most twice.)

What is the ID of the guard you chose multiplied by the minute you
chose? (In the above example, the answer would be 99 * 45 = 4455.)

|#

(defun find-most-asleep-minute (guards)
  (let ((guard-id-minute-freq (list nil nil -1)))
    (maphash (lambda (guard-id guard)
               (destructuring-bind (guard-minute . guard-freq)
                   (setf (guard-most-sleepy-minute guard)
                         (compute-most-sleepy-minute guard))
                 (when (< (third guard-id-minute-freq) guard-freq)
                   (setf guard-id-minute-freq
                         (list guard-id guard-minute guard-freq)))))
             guards)
    guard-id-minute-freq))

(defun compute-p2-answer (guard-id minute frequency)
  (declare (ignore frequency))
  (* guard-id minute))

(defun d3/p2/test ()
  (with-input-from-string (s *input/d4/test*)
    (let ((most-asleep-minute
           (find-most-asleep-minute (records-per-guard (load-records s)))))
      (values (apply #'compute-p2-answer most-asleep-minute)
              most-asleep-minute))))

(defun d4/p2 ()
  (with-open-file (input *input/d4/p1*
                         :direction :input
                         :element-type 'character)
    (apply #'compute-p2-answer
           (find-most-asleep-minute (records-per-guard (load-records input))))))

(defun d4/summary ()
  (format t "Day 4: Repose Record~%")
  (format t "  Puzzle 1: Most Sleepy Guard, Strategy 1~%")
  (print-result (d4/p1))
  (format t "  Puzzle 2: Minute Most Slept by a single guard, Strategy 2~%")
  (print-result (d4/p2)))
