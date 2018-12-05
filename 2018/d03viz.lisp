;;;
;;; Visualization of https://adventofcode.com/2018/day/3
;;;

(in-package :advent/2018/viz)

(define-application-frame aocfabric ()
  ((fabric :initform nil :accessor fabric)
   (claimed-once :initform nil :accessor claimed-once))
  (:menu-bar menubar-command-table)
  (:panes (display :application
                   :display-function 'draw-fabric
                   :background +black+
                   :foreground +white+
                   :text-style (clim:make-text-style :serif :bold 24)
                   :text-cursor nil
                   :scroll-bars nil)
          (int :interactor
               :background +black+
               :foreground +white+
               :height 50
               :text-style (clim:make-text-style :serif :roman 14)
               :end-of-page-action :scroll
               :scroll-bars t))
  (:layouts
   (:default (vertically ()
                 (scrolling (:height 600 :width 1020 :scroll-bars t)
                   display)
                 int))))

(defmethod initialize-fabric ((aocfabric aocfabric))
  (unless (fabric aocfabric)
    (let ((fabric (make-fabric-from-claims-stream *input/d3/p1*)))
      (setf (fabric aocfabric) fabric)
      (map-claim-input (lambda (claim)
                         (when (claimed-only-once fabric claim)
                           (setf (claimed-once aocfabric) claim)
                           (return-from initialize-fabric)))
                       *input/d3/p1*))))

(defun draw-claim (claim stream &key (filled t) ink line-dashes)
  "Draw a claim in our McCLIM application"
  (clim:draw-rectangle* stream
                        (claim-left claim)
                        (claim-top claim)
                        (+ (claim-left claim) (claim-width claim))
                        (+ (claim-top claim) (claim-height claim))
                        :filled filled
                        :line-dashes line-dashes
                        :ink ink))

(defmethod draw-fabric ((aocfabric aocfabric) stream &key max-width max-height)
  (declare (ignore max-width max-height))
  ;; draw all claims
  (map-claim-input (lambda (claim)
                     (flet ((random-component ()
                              (/ (+ 10 (random 90)) 100.0)))
                       (let ((random-color
                              (compose-in
                               (clim:make-rgb-color (random-component)
                                                    (random-component)
                                                    (random-component))
                               (make-opacity 0.5))))
                         (draw-claim claim stream :ink random-color))))
                   *input/d3/p1*)

  ;; draw the one claim claimed only once
  (when (claimed-once aocfabric)
    (draw-claim (claimed-once aocfabric) stream :ink +white+)))

(define-aocfabric-command (com-find :name t) ()
  (initialize-fabric *application-frame*))

(define-aocfabric-command (com-reset :name t) ())

(define-aocfabric-command (com-quit :name t) ()
  (clim:frame-exit clim:*application-frame*))

(make-command-table 'menubar-command-table
		    :errorp nil
		    :menu '(("Find" :command com-find)
                            ("Reset" :command com-reset)
                            ("Quit" :command com-quit)))

(defun viz ()
  (run-frame-top-level (make-application-frame 'aocfabric)))
