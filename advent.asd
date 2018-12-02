;;;; advent.asd

(asdf:defsystem #:advent
  :serial t
  :description "Advent Of code"
  :author "Dimitri Fontaine <dim@tapoueh.org>"
  :license "WTFPL"
  :components ((:file "package")
               (:file "utils")
               (:module "2017"
                        :components ((:file "package")
                                     (:file "d03p1")
                                     (:file "d03p2")))
               (:module "2018"
                        :components
                        ((:file "package")
                         (:file "d01")
                         (:file "d02")
                         (:file "summary")))))


