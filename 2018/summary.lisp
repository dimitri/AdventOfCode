(in-package #:advent/2018)

(defun summary ()
  (loop :for day-summary :in (list #'d1/summary
                                   #'d2/summary)
     :do (funcall day-summary)
     :do (format t "~%")))
