(eval-when (:compile-toplevel :load-toplevel :execute)
  (require :asdf)
  )
(defpackage :scheme-system
  (:use :common-lisp :asdf))

(in-package :scheme-system)

(defsystem :gps
  :name "GPS"
  :author "Peter Novig"
  :maintainer "Seiji Koide <koide@ontolonomy.co.jp>"
  :version "0.0.1"
  :license "PAIP"
  :description "GPS program from PAIP, but modernized by Seiji"
  :long-description "Modified version of GPS in Common Lisp from 'Paradigms of Artificiall Intelligence Programming' by Peter Norvig. This is modernized according to modern lisp by Seiji Koide."
  :components
  ((:file "gpsaux")
   (:file "gps1")
   (:file "gps")))
