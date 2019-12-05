;;;
;;; Visualization of https://adventofcode.com/2018/day/3
;;;

(in-package :advent/2018/viz)

(define-application-frame aoc-carts ()
  ((input  :initform advent/2018::*d13/input* :reader aoc-carts-input)
   (track  :initform (advent/2018::read-track advent/2018::*d13/input*)
           :reader aoc-carts-track)
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

(defparameter *path-patterns*
  (list (cons #\- (make-pattern #2a#((0 0 1 0)
                                     (0 0 1 0)
                                     (0 0 1 0)
                                     (0 0 1 0))
                                (list +transparent-ink+ +green4+)))
        (cons #\| (make-pattern #2a#((0 0 0 0)
                                     (0 0 0 0)
                                     (1 1 1 1)
                                     (0 0 0 0))
                                (list +transparent-ink+ +green4+)))
        (cons #\+ (make-pattern #2a#((0 0 1 0)
                                     (0 0 1 0)
                                     (1 1 1 1)
                                     (0 0 1 0))
                                (list +transparent-ink+ +green4+)))
        (cons #\\ (make-pattern #2a#((0 0 0 0)
                                     (0 0 0 0)
                                     (0 1 1 1)
                                     (0 1 0 0))
                                (list +transparent-ink+ +green4+)))
        (cons #\/ (make-pattern #2a#((0 0 0 0)
                                     (0 0 0 0)
                                     (1 1 1 0)
                                     (0 0 1 0))
                                (list +transparent-ink+ +green4+)))))

(defun draw-path (path stream &key row col (size 4))
  (labels ((zoom (pos)     (* size pos))
           (map-pos (pos)  (zoom (+ 1 pos))))
    (let ((pattern (cdr (assoc path *path-patterns* :test #'char=))))
      (when pattern
        (draw-pattern* stream
                       (cdr (assoc path *path-patterns* :test #'char=))
                       (map-pos row)
                       (map-pos col))))))

(defun draw-cart (cart stream &key (filled t) (color +blue+) (size 4))
  "Draw a claim in our McCLIM application"
  (let ((border (+ size -1)))
    (labels ((zoom (pos)     (* size pos))
             (map-pos (pos)  (zoom (+ 1 pos))))
      (clim:draw-rectangle* stream
                            (map-pos  (advent/2018::cart-row cart))
                            (map-pos  (advent/2018::cart-col cart))
                            (+ border (map-pos (advent/2018::cart-row cart)))
                            (+ border (map-pos (advent/2018::cart-col cart)))
                            :filled filled
                            :ink color))))

(defmethod draw-area ((aoc-carts aoc-carts) stream &key max-width max-height)
  (declare (ignore max-width max-height))

  (loop :for line :across (advent/2018::track-tracks (aoc-carts-track aoc-carts))
     :for row :from 0
     :do (loop :for char :across line
            :for col :from 0
            :do (draw-path char stream :row row :col col)))

  (loop :for cart :in (advent/2018::track-carts (aoc-carts-track aoc-carts))
     :do (draw-cart cart stream)))

(define-aoc-carts-command (com-next :name t) ()
  (advent/2018::move-one-tick (aoc-carts-track *application-frame*)))

(define-aoc-carts-command (com-quit :name t) ()
  (clim:frame-exit clim:*application-frame*))

(make-command-table 'menubar-command-table
		    :errorp nil
		    :menu '(("Next" :command com-next)
                            ("Quit" :command com-quit)))

(defun viz-stars ()
  (run-frame-top-level (make-application-frame 'aoc-carts)))
