;;;; package.lisp

(defpackage #:etoh.cfr27
  (:use #:cl #:unit-formulas #:etoh.units #:local-time #:prove #:prove.suite
	#:drakma #:xmls)
  (:nicknames #:cfr27)

  (:export
   ;;;; Part 30
   ;;;  Subpart A
   ;;   30.1
   ;;;  Subpart B
   ;;   30.11
   #:administrator ; TODO move these the first sections they are referenced.
   #:bulk-conveyance
   #:denatured-spirits
   #:denatured-alcohol
   ;;;  Subpart C
   ;;   30.21
   ;;   30.22
   #:*precision-spirit-hydrometer-table*
   #:*precision-spirit-hydrometer-temperature*
   #:proof-to-hydrometer-designation
   #:*precision-thermometer-table*
   ;;   30.23
   #:*hydrometer-reading-precision*
   #:*thermometer-reading-precision*
   ;;   30.24
   #:*precision-specific-gravity-hydrometer-table*
   #:*precision-specific-gravity-hydrometer-temperature*
   #:*precision-specific-gravity-hydrometer-tollerance*
   #:document ; TODO move this to a definitions area
   #:certification ; TODO move this to a definitions area
   #:certificate-of-accuracy ; TODO move this to a definitions area.
   ;;   30.25

   ;;   30.61
   #:30.61-table-lookup
   #:true-percent-proof ; hydrometer correction using table 30.61
   ;;   30.62
   #:30.62-table-lookup
   #:30.62-volume-by-weight-and-proof
   ;;   30.63
   #:30.63-table-lookup
   #:30.63-volume-by-weight-and-proof
   ;;   30.64
   #:30.64-table-lookup
   #:30.64-volume-by-weight-and-proof
   #:mass-of-water-for-dilution
   ;;   30.65
   #:30.65-table-lookup
   #:mass-per-volume
   #:volume-to-mass
   ;;   30.66
   #:30.66-table-lookup
   #:volume-of-water-for-dilution
   #:33.66-volume-by-weight-and-proof

   #:volume-by-weight-and-proof
   #:volume-per-mass-ratio
   #:mass-per-volume))



