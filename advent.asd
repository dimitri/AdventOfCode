;;;; advent.asd

(asdf:defsystem #:advent
  :serial t
  :description "Advent Of code"
  :author "Dimitri Fontaine <dim@tapoueh.org>"
  :license "ISC"
  :depends-on (#:mcclim)
  :components ((:file "package")
               (:file "utils")
               (:module "2017"
                        :components ((:file "package")
                                     (:file "d01")
                                     (:file "d02")
                                     (:file "d03p1")
                                     (:file "d03p2")
                                     (:file "d04")
                                     (:file "d05")
                                     (:file "d06")
                                     (:file "summary")))
               (:module "2018"
                        :components
                        ((:file "package")
                         (:file "d01")
                         (:file "d02")
                         (:file "d03")
                         (:file "d03viz")
                         (:file "d04")
                         (:file "d05")
                         (:file "d06")
                         (:file "d06viz")
                         (:file "summary")))))


