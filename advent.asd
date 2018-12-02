;;;; advent.asd

(asdf:defsystem #:advent
  :serial t
  :description "Advent Of code"
  :author "Dimitri Fontaine <dim@tapoueh.org>"
  :license "WTFPL"
  :components ((:module "2018"
                        :components
                        ((:file "package")
                         (:file "utils")
                         (:file "d01")
                         (:file "d02")
                         (:file "summary")))))


