;;;; etoh.cfr27.asd

(asdf:defsystem #:etoh.cfr27
  :description "etoh.cfr27: A library to interface with US Title 27"
  :author "Elliott Johnson <elliott@elliottjohnson.net>"
  :license "LLGPL"
  :depends-on ("unit-formulas" "etoh.units" "local-time" "prove")
  :serial t
  :components ((:file "package")
               (:file "etoh.cfr27")
	       (:file "part5")
	       (:file "part30")))

