(in-package :advent/2018)

(defparameter *d16/input*
  (uiop:read-file-string
   (asdf:system-relative-pathname :advent "2018/d16.input")))

(defparameter *d16/test* "
Before: [3, 2, 1, 1]
9 2 1 2
After:  [3, 2, 2, 1]

Before: [0, 3, 0, 2]
13 0 0 3
After:  [0, 3, 0, 0]
")

(defstruct vm
  (regs (make-array 4 :element-type 'fixnum :initial-element 0)))

(defstruct (instr (:constructor create-instr (opcode a b c)))
  opcode a b c)

(defvar *opcodes* #(addr addi mulr muli banr bani borr bori
                    setr seti gtir gtri gtrr eqir eqri eqrr))

(defun vm-exec (vm opcode a b c)
  (macrolet ((reg (x) `(aref (vm-regs vm) ,x)))
    (setf (reg c)
          (ecase opcode
            (addr (+ (reg a) (reg b)))
            (addi (+ (reg a) b))
            (mulr (* (reg a) (reg b)))
            (muli (* (reg a) b))
            (banr (logand (reg a) (reg b)))
            (bani (logand (reg a) b))
            (borr (logior (reg a) (reg b)))
            (bori (logior (reg a) b))
            (setr (reg a))
            (seti a)
            (gtir (if (> a (reg b)) 1 0))
            (gtri (if (> (reg a) b) 1 0))
            (gtrr (if (> (reg a) (reg b)) 1 0))
            (eqir (if (= a (reg b)) 1 0))
            (eqri (if (= (reg a) b) 1 0))
            (eqrr (if (= (reg a) (reg b)) 1 0))))
    vm))

(defun read-cpu-sample (string)
  (flet ((read-array-notation (line first-start)
           (loop :for start := first-start :then (+ 1 end)
              :while (< start (length line))
              :for (val end) := (multiple-value-list
                                 (parse-integer line
                                                :start start
                                                :junk-allowed t))
              :collect val)))
    (with-input-from-string (s string)
      (loop :for line := (read-line s nil nil)
         :while line
         :for x := (cond ((or (search "Before: " line)
                              (search "After:  " line))
                          (let* ((regs (read-array-notation line 9)))
                           (make-vm :regs (make-array 4 :initial-contents regs))))
                         ((< 0 (length line))
                          (apply #'create-instr (read-array-notation line 0))))
         :when x :collect x))))

(defun compatible-opcode-p (vm-before vm-after instr opcode-patch)
  (let* ((vm-copy (make-vm :regs (copy-seq (vm-regs vm-before))))
         (result (vm-exec vm-copy
                          opcode-patch
                          (instr-a instr)
                          (instr-b instr)
                          (instr-c instr))))
    (equalp result vm-after)))

(defun try-all-opcodes (vm-before vm-after instr)
  (loop :for opcode :below (length *opcodes*)
     :for compatible := (compatible-opcode-p vm-before vm-after instr
                                             (aref *opcodes* opcode))
     :when compatible
     :collect (aref *opcodes* opcode)))

(defun count-samples-matching-at-least-n-opcodes (&optional
                                                    (input *d16/input*)
                                                    (n 3))
  (loop :for (before instr after more?) :on (read-cpu-sample input) :by #'cdddr
     :while (and (vm-p before) (vm-p after))
     :count (<= n (length (try-all-opcodes before after instr)))
     :while more?))

(defun d16/p1 (&optional (input *d16/input*))
  (count-samples-matching-at-least-n-opcodes input 3))

(defun read-samples (&optional (input *d16/input*))
  (loop
     :for (before instr after more?) :on (read-cpu-sample input) :by #'cdddr
     :while (and (vm-p before) (vm-p after))
     :collect (list before instr after)
     :while more?))

(defun read-program (&optional (input *d16/input*))
  (loop
     :for (before instr after more?) :on (read-cpu-sample input) :by #'cdddr
     :unless (and (vm-p before) (vm-p after))
     :append (list before instr after)
     :while more?))

(defun try-unknown-opcodes (vm-before vm-after instr found-opcodes)
  (remove nil
          (loop :for opcode :below (length *opcodes*)
             :for op := (aref *opcodes* opcode)
             :unless (find op found-opcodes)
             :collect (when (compatible-opcode-p vm-before vm-after instr op)
                        op))))

(defun find-next-opcodes (samples
                          &optional (found-opcodes
                                     (make-array 16
                                                 :element-type 'symbol
                                                 :initial-element nil)))
  (loop :for (before instr after) :in samples
     :do (let ((matching-opcodes
                (try-unknown-opcodes before after instr found-opcodes)))
           (when (= 1 (length matching-opcodes))
             (if (null (aref found-opcodes (instr-opcode instr)))
                 (setf (aref found-opcodes (instr-opcode instr))
                       (first matching-opcodes))
                 (unless (eq (aref found-opcodes (instr-opcode instr))
                             (first matching-opcodes))
                   (error "Two instructions found with same opcode: ~a ~a ~a"
                          (instr-opcode instr)
                          (first matching-opcodes)
                          (aref found-opcodes (instr-opcode instr))))))))
  found-opcodes)

(defun find-opcodes (samples)
  (let ((opcodes (make-array 16 :element-type 'symbol :initial-element nil)))
    (loop :while (some #'null opcodes)
       :do (find-next-opcodes samples opcodes))
    opcodes))

(defun run-vm-program (program opcodes)
  (let ((vm (make-vm)))
    (loop :for instr :in program
       :do (vm-exec vm
                    (aref opcodes (instr-opcode instr))
                    (instr-a instr)
                    (instr-b instr)
                    (instr-c instr)))
    vm))

(defun d16/p2 (&optional (input *d16/input*))
  (let* ((samples (read-samples input))
         (program (read-program input))
         (opcodes (find-opcodes samples)))
    (aref (vm-regs (run-vm-program program opcodes)) 0)))

(defun d16/summary ()
  (format t "Day 16: Chronal Classification~%")
  (format t "  Puzzle 1: Count samples matching exactly 3 opcodes~%")
  (print-result (d16/p1))
  (format t "  Puzzle 2: Run given program with found opcodes~%")
  (print-result (d16/p2)))
