;;;
;;; Visualization of https://adventofcode.com/2018/day/3
;;;

(in-package :advent/2018/viz)

(define-application-frame aoc-stars ()
  ((input  :initform advent/2018::*d10/input* :reader aoc-stars-input)
   (stars  :initform (advent/2018::read-points advent/2018::*d10/input*)
           :reader aoc-stars-stars)
   (ts     :initform 0 :accessor aoc-ts))
  (:menu-bar menubar-command-table)
  (:panes (display :application
                   :display-function 'draw-area
                   :background +black+
                   :foreground +white+
                   :text-style (clim:make-text-style :serif :bold 24)
                   :text-cursor nil
                   :scroll-bars nil)
          (int :interactor
               :background +black+
               :foreground +white+
               :height 10
               :text-style (clim:make-text-style :serif :roman 14)
               :end-of-page-action :scroll
               :scroll-bars t))
  (:layouts
   (:default (vertically ()
                 (scrolling (:height 650 :width 650 :scroll-bars t)
                   display)
                 int))))

(defun draw-star (star stream &key (filled t) (color +yellow+) (size 4))
  "Draw a claim in our McCLIM application"
  (let ((border (+ size -1)))
    (labels ((zoom (pos)     (* size pos))
             (map-pos (pos)  (zoom (+ 1 pos))))
      (clim:draw-rectangle* stream
                            (map-pos  (advent/2018::light-x star))
                            (map-pos  (advent/2018::light-y star))
                            (+ border (map-pos (advent/2018::light-x star)))
                            (+ border (map-pos (advent/2018::light-y star)))
                            :filled filled
                            :ink color))))

(defmethod draw-area ((aoc-stars aoc-stars) stream &key max-width max-height)
  (declare (ignore max-width max-height))

  (when (= (aoc-ts aoc-stars) 0)
    (let ((init-ts (+ 9 (* 15 10) (* 25 60) (* 15 600))))
      (format *trace-output* "init to ~as~%" init-ts)
      (advent/2018::move-seconds init-ts (aoc-stars-stars *application-frame*))
      (setf (aoc-ts aoc-stars) init-ts)))

  (loop :for star :in (aoc-stars-stars aoc-stars)
     :do (draw-star star stream)))

(define-aoc-stars-command (com-next-60 :name t) ()
  (advent/2018::move-seconds 60 (aoc-stars-stars *application-frame*))
  (incf (aoc-ts *application-frame*) 60))

(define-aoc-stars-command (com-next-10 :name t) ()
  (advent/2018::move-seconds 10 (aoc-stars-stars *application-frame*))
  (incf (aoc-ts *application-frame*) 10))

(define-aoc-stars-command (com-next :name t) ()
  (advent/2018::move-seconds 1 (aoc-stars-stars *application-frame*))
  (incf (aoc-ts *application-frame*)))

(define-aoc-stars-command (com-quit :name t) ()
  (clim:frame-exit clim:*application-frame*))

(make-command-table 'menubar-command-table
		    :errorp nil
		    :menu '(("Next" :command com-next)
                            ("Next 10" :command com-next-10)
                            ("Next 60" :command com-next-60)
                            ("Quit" :command com-quit)))

(defun viz-stars ()
  (run-frame-top-level (make-application-frame 'aoc-stars)))
