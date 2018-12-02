(in-package #:advent/2018)

(defmacro timing (&body forms)
  "return both how much real time was spend in body and its result"
  (let ((start (gensym))
	(end (gensym))
	(result (gensym)))
    `(let* ((,start (get-internal-real-time))
	    (,result (progn ,@forms))
	    (,end (get-internal-real-time)))
       (values ,result (/ (- ,end ,start) internal-time-units-per-second)))))

(defmacro print-result (&body forms)
  `(multiple-value-bind (res secs)
       (timing (progn ,@forms))
     (format t "  ~7,3fms~10T~a~%" (* 1000 secs) res)))
