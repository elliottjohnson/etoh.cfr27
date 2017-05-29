(in-package :etoh.cfr27)

;;; Subpart A - Scope of Regulations
;;;; 30.1 Gauging of distilled spirits.

;;; Subpart B - Definitions
;;;; 30.11 Meaning of terms
(defclass administer (person)
  ()
  (:documentation "The Administrator, Alcohol and Tobacco Tax and Trade Bureau, Department of the Treasury, Washington, DC."))

;; Appropriate TTB officer. An officer or employee of the Alcohol and Tobacco Tax and Trade Bureau (TTB) authorized to perform any functions relating to the administration or enforcement of this part by TTB Order 1135.30, Delegation of the Administrator's Authorities in 27 CFR Part 30, Gauging Manual.

(defclass bulk-conveyance (container bulk)
 ()
  (:documentation "Any tank car, tank truck, tank ship, tank barge, or other similar container approved by the appropriate TTB officer, authorized for the conveyance of spirits (including denatured spirits) in bulk."))

;; CFR. The Code of Federal Regulations.
;; Container. Any receptacle, vessel, or form of package, bottle, tank, or pipeline used, or capable of use, for holding, storing, transferring or conveying distilled spirits.

(defclass denatured-spirits (spirits)
  ()
  (:documentation "Spirits to which denaturants have been added pursuant to formulas prescribed in 27 CFR Part 21."))

;; Gallon or wine gallon. The liquid measure equivalent to the volume of 231 cubic inches.
;; I.R.C. The Internal Revenue Code of 1954, as amended.
;; package: Any cask, barrel, drum, or similar container approved under the provisions of this chapter.
;; Proof. The ethyl alcohol content of a liquid at 60 degrees Fahrenheit, stated as twice the percent of ethyl alcohol by volume.
;; Proof gallon. A United States gallon of proof spirits, or the alcoholic equivalent thereof.
;; Proof spirits. That liquid which contains one-half its volume of ethyl alcohol of a specific gravity of seven thousand nine hundred and thirty-nine ten-thousandths (0.7939) in vacuum at 60 degrees Fahrenheit referred to water at 60 degrees Fahrenheit as unity.
;; Spirits, spirituous liquor, or distilled spirits. That substance known as ethyl alcohol, ethanol, or spirits of wine in any form, including all dilutions and mixtures thereof, from whatever source or by whatever process produced, but not denatured spirits unless specifically stated. For the sole purpose of gauging wine and alcoholic flavoring materials on the bonded premises of a distilled spirits plant, such alcoholic ingredients shall have the same meaning described herein to spirits, spirituous liquor, or distilled spirits.

;;; Subpart C - Gauging instruments
;;;; 30.21 Requirements
;;;; 30.22 Hydrometers and thermometers
;;;; 30.23 Use of precision hydrometers and thermometers
;;;; 30.24 Specific gravity hydrometers
;;;; 30.25 Use of precision specific gravity hydrometers

;;; Subpart D - Gauging Procedures
;;;; 30.31 Determination of proof.
;;;; 30.32 Determination of proof obscuration.
;;; Determination of Quantity
;;;; 30.36 General requirements
;;; Determination of Quantity by Weight
;;;; 30.41 Bulk spirits.
;;;; 30.42 Denatured spirits
;;;; 30.43 Packed spirits
;;;; 30.44 Weighing containers.
;;;; 30.45 Withdrawal gauge for packages.
;;; Determination of Quantity by Volume
;;;; 30.51 Procedure for measurement of bulk spirits.
;;;; 30.52 Procedure for measurement of cased spirits.

;;; Subpart E - Prescribed Tables
;;;; 30.61 Table 1, true % proof spirit for any indication of the 
;;;;    hydrometer at temperatures between zero and 100 degrees Fahrenheit.

(defvar *30.61-true-percent-proof-table* nil
  "Table 1, showing the true percent of proof spirit for any indication of the 
hydrometer at temperatures between zero and 100 degrees Fahrenheit.  The
function TRUE-PERCENT-PROOF will lazy load this value.")

(defun load-30.61-table-array (&optional (force nil))
  (when (or force (not *30.61-true-percent-proof-table*))
    (setf *30.61-true-percent-proof-table*
	  (with-open-file
	      (array (merge-pathnames "part30.61.tbl1.array.lisp"
		      (current-directory)))
	    (read array)))
    t))

(defun %true-percent-proof (proof temperature)
  (let ((array-dimensions (array-dimensions *30.61-true-percent-proof-table*))
	(proof-floor (floor proof))
	(proof-ceiling (ceiling proof))
	(temp-floor (floor temperature))
	(temp-ceiling (ceiling temperature)))
    (let ((table-value
	   (if (and (<= proof-ceiling (first array-dimensions))
		    (>= proof-floor 0)
		    (<= temp-ceiling (second array-dimensions))
		    (>= proof-floor 0))
	       (if (and (= proof-floor proof-ceiling)
			(= temp-floor temp-ceiling))
		   ;; if we have integer values then we can access the table directly.
		   (aref *30.61-true-percent-proof-table* proof-floor temp-floor)
		   ;; if we have a decimal value then we interpolate according to §30.23.
		   (let ((proof-temp-floor (aref *30.61-true-percent-proof-table* proof-floor temp-floor))
			 (hydro-ceiling
			  (aref *30.61-true-percent-proof-table* proof-ceiling temp-floor))
			 (temp-ceiling
			  (aref *30.61-true-percent-proof-table* proof-floor temp-ceiling)))
		     (if (some #'(lambda (x) (= -1 x)) (list proof-temp-floor hydro-ceiling temp-ceiling))
			 -1
			 (let* ((diff-hydro (- hydro-ceiling proof-temp-floor))
				(diff-temp (- temp-ceiling proof-temp-floor))
				(hydro (* diff-hydro (- proof proof-floor)))
				(temp (* diff-temp (- temperature temp-floor))))
			   (+ proof-temp-floor hydro temp)))))
	       -1)))
      (cond ((/= table-value -1)
	     ;; If we have a value, then return it as a unit of proof.
	     (reduce-unit (list table-value 'proof)))
	    (t
	     ;; If we don't have a value, return nil with a message that might be captured.
	     ;;   I think this is prime for signaling a condition... 
	     (values nil
		     (format nil "No table values for ~A proof at ~A fahrenheit" proof-floor temp-floor)))))))

(defgeneric true-percent-proof (proof temperature)
  (:documentation
   "Returns the actual percent proof provided by proof and temperature or
NIL if the values fall outside of the known ranges.")
  (:method :before (proof temperature)
    (unless *30.61-true-percent-proof-table*
      (load-30.61-table-array)))
  (:method ((proof unit) (temperature unit))
    (let ((pf (convert-unit proof 'proof))
	  (temp (convert-unit temperature 'fahrenheit)))
      (%true-percent-proof pf temp)))
  (:method ((proof list) temperature)
    (true-percent-proof (reduce-unit proof) temperature))
  (:method (proof (temperature list))
    (true-percent-proof proof (reduce-unit temperature))))

;;;; 30.62 Table 2, wine gallons and proof gallons by weight.
;;;; 30.63 Table 3, determining the # of proof gallons from the weight
;;;;    and proof of spirituous liquor
;;;; 30.64 Table 4, showing the fractional part of a gallon per pound
;;;;    at each percent and each tenth percent of proof of spirituous liquor
;;;; 30.65 Table 5, showing the weight per wine gallon (at 60 dF) and proof
;;;;    gallon at each percent of proof of spirituous liquor.
;;;; 30.66 Table 6, showing respective volumes of alcohol and water and the
;;;;    specific gravity in both air and vacuum of spirituous liquor.
;;;; 30.67 Table 7, for correction of volume of spirituous liquors to 60 dF.

;;; Subpart F - Optional Gauging Procedures
;;;; 30.71 Optional method for determination of proof of spirits containing
;;;;    solids of 400 mg or less per 100 mL.
;;;; 30.72 Recording obscuration by proprietors using the optional method
;;;;    for determination of proof.
