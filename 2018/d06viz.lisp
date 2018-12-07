;;;
;;; Visualization of https://adventofcode.com/2018/day/3
;;;

(in-package :advent/2018/viz)

(defparameter *color-list*
  (flet ((make-rgb-hex-color (r g b)
           (make-rgb-color (/ r 256.0) (/ g 256.0) (/ b 256.0))))
    (list
     (make-rgb-hex-color 1 0 103)
     (make-rgb-hex-color 213 255 0)
     (make-rgb-hex-color 255 0 86)
     (make-rgb-hex-color 158 0 142)
     (make-rgb-hex-color 14 76 161)
     (make-rgb-hex-color 255 229 2)
     (make-rgb-hex-color 0 95 57)
     (make-rgb-hex-color 0 255 0)
     (make-rgb-hex-color 149 0 58)
     (make-rgb-hex-color 255 147 126)
     (make-rgb-hex-color 164 36 0)
     (make-rgb-hex-color 0 21 68)
     (make-rgb-hex-color 145 208 203)
     (make-rgb-hex-color 98 14 0)
     (make-rgb-hex-color 107 104 130)
     (make-rgb-hex-color 0 0 255)
     (make-rgb-hex-color 0 125 181)
     (make-rgb-hex-color 106 130 108)
     (make-rgb-hex-color 0 174 126)
     (make-rgb-hex-color 194 140 159)
     (make-rgb-hex-color 190 153 112)
     (make-rgb-hex-color 0 143 156)
     (make-rgb-hex-color 95 173 78)
     (make-rgb-hex-color 255 0 0)
     (make-rgb-hex-color 255 0 246)
     (make-rgb-hex-color 255 2 157)
     (make-rgb-hex-color 104 61 59)
     (make-rgb-hex-color 255 116 163)
     (make-rgb-hex-color 150 138 232)
     (make-rgb-hex-color 152 255 82)
     (make-rgb-hex-color 167 87 64)
     (make-rgb-hex-color 1 255 254)
     (make-rgb-hex-color 255 238 232)
     (make-rgb-hex-color 254 137 0)
     (make-rgb-hex-color 189 198 255)
     (make-rgb-hex-color 1 208 255)
     (make-rgb-hex-color 187 136 0)
     (make-rgb-hex-color 117 68 177)
     (make-rgb-hex-color 165 255 210)
     (make-rgb-hex-color 255 166 254)
     (make-rgb-hex-color 119 77 0)
     (make-rgb-hex-color 122 71 130)
     (make-rgb-hex-color 38 52 0)
     (make-rgb-hex-color 0 71 84)
     (make-rgb-hex-color 67 0 44)
     (make-rgb-hex-color 181 0 255)
     (make-rgb-hex-color 255 177 103)
     (make-rgb-hex-color 255 219 102)
     (make-rgb-hex-color 144 251 146)
     (make-rgb-hex-color 126 45 210)
     (make-rgb-hex-color 189 211 147)
     (make-rgb-hex-color 229 111 254)
     (make-rgb-hex-color 222 255 116)
     (make-rgb-hex-color 0 255 120)
     (make-rgb-hex-color 0 155 255)
     (make-rgb-hex-color 0 100 1)
     (make-rgb-hex-color 0 118 255)
     (make-rgb-hex-color 133 169 0)
     (make-rgb-hex-color 0 185 23)
     (make-rgb-hex-color 120 130 49)
     (make-rgb-hex-color 0 255 198)
     (make-rgb-hex-color 255 110 65)
     (make-rgb-hex-color 232 94 190)))
  "http://godsnotwheregodsnot.blogspot.com/2012/09/color-distribution-methodology.html")

(define-application-frame aoc-areas ()
  ((input  :initform *d6/input* :reader aoc-areas-input)
   (psize  :initform 2 :reader aoc-areas-pixel-size)
   (coords :initform nil :accessor coords)
   (b-box  :initform nil :accessor b-box)
   (grid   :initform nil :accessor grid)
   (manhattan-sum-grid :initform nil :accessor manhattan-sum-grid)
   (manhattan-threshold :initform 10000 :accessor manhattan-threshold))
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

(defmethod initialize-area ((aoc-areas aoc-areas))
  (unless (coords aoc-areas)
    (let* ((coords       (read-coordinates (aoc-areas-input aoc-areas)))
           (bounding-box (bounding-box coords))
           (grid         (compute-closest-coords coords bounding-box)))

      (compute-finite-p coords bounding-box grid)

      (loop :for ink :in *color-list*
         :for coord :in coords
         :do (setf (coord-color coord) ink))

      (setf (coords aoc-areas) coords)
      (setf (b-box aoc-areas) bounding-box)
      (setf (grid aoc-areas) grid))))

(defun draw-coord (x y stream &key finite-p (filled t) color min-x min-y)
  "Draw a claim in our McCLIM application"
  (let* ((pixel-size (aoc-areas-pixel-size *application-frame*))
         (border     (+ pixel-size (if finite-p -1 0))))
    (labels ((zoom (pos)        (* pixel-size pos))
             (map-pos (pos min) (zoom (- (+ 1 pos) min))))
      (clim:draw-rectangle* stream
                            (map-pos x min-x)
                            (map-pos y min-y)
                            (+ border (map-pos x min-x))
                            (+ border (map-pos y min-y))
                            :filled filled
                            :ink color))))

(defmethod draw-area ((aoc-areas aoc-areas) stream &key max-width max-height)
  (declare (ignore max-width max-height))

  (initialize-area aoc-areas)

  (destructuring-bind (min-x min-y max-x max-y)
      (b-box *application-frame*)

    (declare (ignore max-x max-y))

    ;; add a border to coords
    (loop :for coord :in (coords aoc-areas)
       :do (draw-coord (coord-x coord) (coord-y coord) stream
                       :filled t
                       :color +white+
                       :min-x min-x
                       :min-y min-y))

    (loop :for (x . y) :being :the :hash-key :of (grid aoc-areas)
       :for reference-coord := (gethash (cons x y) (grid aoc-areas))
       :unless (and (= x (coord-x reference-coord))
                    (= y (coord-y reference-coord)))
       :do (draw-coord x y stream
                       :finite-p (coord-finite-p reference-coord)
                       :color (coord-color reference-coord)
                       :min-x min-x
                       :min-y min-y))

    (when (manhattan-sum-grid aoc-areas)
      (let ((safe-color (compose-in +white+ (make-opacity 0.6))))
        (loop :for (x . y)
           :being :the :hash-key :of (manhattan-sum-grid aoc-areas)
           :when (< (gethash (cons x y) (manhattan-sum-grid aoc-areas))
                    (manhattan-threshold aoc-areas))
           :do (draw-coord x y stream
                           :color safe-color
                           :min-x min-x
                           :min-y min-y))))))

(define-aoc-areas-command (com-reset :name t) ()
  (initialize-area *application-frame*))

(define-aoc-areas-command (com-safe :name t) ()
  (setf (manhattan-sum-grid *application-frame*)
        (make-manhattan-sum-grid (aoc-areas-input *application-frame*)
                                 (coords *application-frame*))))

(define-aoc-areas-command (com-quit :name t) ()
  (clim:frame-exit clim:*application-frame*))

(make-command-table 'menubar-command-table
		    :errorp nil
		    :menu '(("Reset" :command com-reset)
                            ("Safe"  :command com-safe)
                            ("Quit" :command com-quit)))

(defun viz-areas ()
  (run-frame-top-level (make-application-frame 'aoc-areas)))
