(in-package #:advent/2018)

(defun summary ()
  (loop :for day-summary :in (list #'d1/summary
                                   #'d2/summary
                                   #'d3/summary
                                   #'d4/summary
                                   #'d5/summary
                                   #'d6/summary
                                   #'d7/summary
                                   #'d8/summary)
     :do (funcall day-summary)
     :do (format t "~%")))
