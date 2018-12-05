;;;
;;; Visualization of https://adventofcode.com/2018/day/3
;;;

(in-package :advent/2018/viz)

(define-application-frame aocfabric ()
  ()
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
  (let ((ink      (clim:compose-in clim:+green3+ (clim:make-opacity 0.3)))
        (ink-once (clim:compose-in clim:+yellow+ (clim:make-opacity 0.6)))
        (fabric   (make-fabric-from-claims-stream *input/d3/p1*)))
    ;; draw all claims
    (map-claim-input (lambda (claim) (draw-claim claim stream :ink ink))
                     *input/d3/p1*)

    ;; draw claims claimed only once
    (map-claim-input (lambda (claim)
                       (when (claimed-only-once fabric claim)
                         (draw-claim claim stream :ink ink-once)))
                     *input/d3/p1*)))

(define-aocfabric-command (com-quit :name t) ()
  (clim:frame-exit clim:*application-frame*))

(make-command-table 'menubar-command-table
		    :errorp nil
		    :menu '(("Quit" :command com-quit)))

(defun viz ()
  (run-frame-top-level (make-application-frame 'aocfabric)))