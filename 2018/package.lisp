(defpackage #:advent-of-code-2018
  (:nicknames :advent/2018)
  (:use #:cl #:advent)
  (:export #:summary
           #:d1/summary
           #:d2/summary
           #:d3/summary
           #:d1/p1
           #:d1/p2
           #:d2/p1
           #:d3/p1
           #:d3/p2

           #:make-fabric-from-claims-stream
           #:map-claim-input
           #:claimed-only-once
           #:*input/d3/p1*
           #:claim-left
           #:claim-width
           #:claim-top
           #:claim-height

           #:*d6/test*
           #:*d6/input*
           #:read-coordinates
           #:bounding-box
           #:compute-closest-coords
           #:compute-area-sizes
           #:compute-finite-p
           #:make-manhattan-sum-grid
           #:make-coord
           #:coord-x
           #:coord-y
           #:coord-finite-p
           #:coord-area-size
           #:coord-color))

(defpackage #:advent/2018/viz
  (:use #:clim #:clim-lisp #:advent/2018)
  (:export #:viz
           #:viz-areas))

