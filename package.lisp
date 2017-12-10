;;;; package.lisp

(defpackage #:etoh.cfr27
  (:use #:cl #:unit-formulas #:etoh.units #:local-time #:prove #:prove.suite)
  (:nicknames #:cfr27)

  (:export


   ;; part 30
   #:true-percent-proof ; hydrometer correction using table 30.61
   #:volume-by-weight-and-proof ; returns volume of an ethanol/water mix by weight and proof using table 30.62
   ;;#:
   ))



