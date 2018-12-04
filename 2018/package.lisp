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
           #:d3/p2))

(defpackage #:advent/2018/viz
  (:use #:clim #:clim-lisp #:advent/2018)
  (:import-from :advent/2018
                :make-fabric-from-claims-stream
                :map-claim-input
                :claimed-only-once
                :*input/d3/p1*
                :claim-left
                :claim-width
                :claim-top
                :claim-height))
