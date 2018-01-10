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

(defmacro lazy-load-table (table-variable filename force)
  `(when (or ;; FORCE gives us a way to force an update.
	     ,force
	     ;; If we don't lazy load, we force load.
	     (not (lazy-load-tables-p *default-site-configuration*))
	     ;; finally if we are lazy, but don't have a value
	     ;;  we get off the couch and LOAD IT!
	     (not ,table-variable))
     (setf ,table-variable
	   (with-open-file (array
			    (merge-pathnames ,filename
					     (current-directory)))
	     (read array)))))

(defun load-30.61-table-array (&optional (force nil))
  (lazy-load-table *30.61-true-percent-proof-table*
		   "part30.61.tbl1.array.lisp"
		   force))

;; dimensions is a list of max array dimensions.
(defun multi-dimensional-array-interpolation (arg1-list
					      arg2-list
					      table-function
					      datum
					      table-null)
  "Reference table values using table-function and interpolate the results.  
ARG1-LIST and ARG2-LIST are of the form:
     (actual-value floor-value ceiling-value)
DATUM is an error message string that should accept 4 string arguments."
  (let ((return-value table-null))
    (destructuring-bind (arg1 arg1-floor arg1-ceiling)
	arg1-list
      (destructuring-bind (arg2 arg2-floor arg2-ceiling)
	  arg2-list
	(handler-case
	    (if (and (= arg1 arg1-floor arg1-ceiling)
		     (= arg2 arg2-floor arg2-ceiling))
		(setf return-value
		      (funcall table-function
			       arg1-floor
			       arg2-floor))
		(let ((arg1-arg2-table-floor
		       (funcall table-function arg1-floor arg2-floor))
		      (arg1-table-ceiling
		       (funcall table-function arg1-ceiling arg2-floor))
		      (arg2-table-ceiling
		       (funcall table-function arg1-floor arg2-ceiling))
		      (arg1-range (- arg1-ceiling arg1-floor))
		      (arg2-range (- arg2-ceiling arg2-floor))
		      (arg1-span (- arg1 arg1-floor))
		      (arg2-span (- arg2 arg2-floor)))
		  (unless (some #'(lambda (x) (equal table-null x))
				(list arg1-arg2-table-floor
				      arg1-table-ceiling
				      arg2-table-ceiling))
		    (let* ((diff-arg1 (- arg1-table-ceiling
					 arg1-arg2-table-floor))
			   (diff-arg2 (- arg2-table-ceiling
					 arg1-arg2-table-floor))
			   (percent-arg1 (if (zerop arg1-range)
					     0
					     (/ arg1-span arg1-range)))
			   (percent-arg2 (if (zerop arg2-range)
					     0
					     (/ arg2-span arg2-range)))
			   (arg1-contribution (* diff-arg1 percent-arg1))
			   (arg2-contribution (* diff-arg2 percent-arg2)))
		      (setf return-value (+ arg1-arg2-table-floor
					    arg1-contribution
					    arg2-contribution))))))
	  (type-error ()
	    (message "~%Invalid index reference (~A,~A), using table-null value: ~A."
		     arg1 arg2 table-null)
	    (setf return-value table-null)))
	;; Do some error checking.
	(unless (not (equal return-value table-null))
	  (when datum
	    (signal datum arg1-floor arg1-ceiling arg2-floor arg2-ceiling)))))
    ;; return our found value.
    return-value))

(defun %true-percent-proof (proof temperature)
  (multi-dimensional-array-interpolation
   (list proof
	 (floor proof)
	 (ceiling proof))
   (list temperature
	 (floor temperature)
	 (ceiling temperature))
   #'(lambda (arg1 arg2)
       (aref *30.61-true-percent-proof-table* arg1 (1- arg2)))
   *30.61-true-percent-proof-table-error-datum*
   -1))

(defvar *30.61-true-percent-proof-table-error-datum*
  "No table values for ~A-~A proof at ~A-~A fahrenheit"
  "If true and a string error datum, an error will be generated with restarts.")

(defgeneric true-percent-proof (proof temperature)
  (:documentation
   "Returns the actual percent proof provided by proof and temperature or
NIL if the values fall outside of the known ranges.")
  (:method :before (proof temperature)
    (unless *30.61-true-percent-proof-table*
      (load-30.61-table-array)))
  (:method ((proof unit) (temperature unit))
    (let* ((pf (convert-unit proof 'proof))
	   (temp (convert-unit temperature 'fahrenheit))
	   (%proof (%true-percent-proof pf temp)))
      (unless (= -1 %proof)
	(reduce-unit (list %proof 'proof)))))  
  (:method ((proof list) temperature)
    (true-percent-proof (reduce-unit proof) temperature))
  (:method (proof (temperature list))
    (true-percent-proof proof (reduce-unit temperature)))
  (:method ((proof number) temperature)
    (true-percent-proof (list proof 'proof)
			temperature))
  (:method (proof (temperature number))
    (true-percent-proof proof (list temperature 'fahrenheit))))

;;;; 30.62 Table 2, wine gallons and proof gallons by weight.

(defvar *30.62-wine-gallons-and-proof-gallons-by-weight* nil
  "Table 2, showing wine gallons and proof gallons by weight.")

(defun load-30.62-table-assoc (&optional force)
  (lazy-load-table *30.62-wine-gallons-and-proof-gallons-by-weight*
		   "part30.62.tbl2.assoc.lisp"
		   force))

(defun %volume-by-weight-and-proof (weight proof)
  (multi-dimensional-array-interpolation
   (list weight ;; in 0.5 increments
	 (/ (floor (* 2 weight) 1) 2)
	 (/ (ceiling (* 2 weight) 1) 2))
   (list proof
	 (floor proof)
	 (ceiling proof))
   #'(lambda (w p)
       (assoc w (cdr (assoc p *30.62-wine-gallons-and-proof-gallons-by-weight*))))
   nil
   nil))

(defun default-column-type (&optional (config *default-site-configuration*))
  (table-column-type config))

(defgeneric volume-by-weight-and-proof (weight proof &key column)
  (:documentation
   "Returns the actual percent proof provided by proof and temperature or
NIL if the values fall outside of the known ranges.")
  (:method :before (weight proof &key column)
    (declare (ignore weight proof column))
    (unless *30.62-wine-gallons-and-proof-gallons-by-weight*
      (load-30.62-table-assoc)))
  (:method ((weight unit) (proof unit) &key (column (default-column-type)))
    (let* ((pf (convert-unit proof 'proof))
	   (pounds (convert-unit weight 'pounds))
	   (%gallons (let ((%gal (%volume-by-weight-and-proof pounds pf)))
		       (cond ((eq column :wine) (second %gal))
			     ((eq column :proof) (third %gal))
			     (t (error "Unknown proof or wine gallon!"))))))
      (unless (null %gallons)
	(reduce-unit (list %gallons 'gallons)))))  
  (:method ((weight list) proof &key (column (default-column-type)))
    (volume-by-weight-and-proof (reduce-unit weight) proof :column column))
  (:method (weight (proof list) &key (column (default-column-type)))
    (volume-by-weight-and-proof weight (reduce-unit proof) :column column))
  (:method ((weight number) proof &key (column (default-column-type)))
    (volume-by-weight-and-proof (reduce-unit (list weight 'pounds)) proof
				:column column))
  (:method (weight (proof number) &key (column (default-column-type)))
    (volume-by-weight-and-proof weight (reduce-unit (list proof 'proof))
				:column column)))

;;;; 30.63 Table 3, determining the # of proof gallons from the weight
;;;;    and proof of spirituous liquor

(defvar *30.63-proof-gallons-by-weight-and-proof* nil
  "Table 3, Determination of the # of proof gallons from the weight
and proof of spirituous liquor")

(defun load-30.63-table-array (&optional force)
  (lazy-load-table *30.63-proof-gallons-by-weight-and-proof*
		   "part30.63.tbl3.array.lisp"
		   force))

(defun %30.63-pounds-to-funcall-index (function pounds)
  (1- (cond ((>= 1000 pounds)
	     (funcall function pounds 100))
	    ((>= 10000 pounds)
	     (+ 9 (funcall function pounds 1000)))
	    ((>= 70000 pounds)
	     (+ 18 (funcall function pounds 10000)))
	    (t (error "pounds beyond range of table: ~A" pounds)))))

(defun 30.63-pounds-to-floor-index (pounds)
  (%30.63-pounds-to-funcall-index #'floor pounds))
(defun 30.63-pounds-to-ceiling-index (pounds)
  (%30.63-pounds-to-funcall-index #'ceiling pounds))

(defun %30.63-pounds-to-funcall-pounds (function pounds)
  (cond ((>= 1000 pounds)
	 (* (funcall function pounds 100) 100))
	((>= 10000 pounds)
	 (* (funcall function pounds 1000) 1000))
	((>= 70000 pounds)
	 (* (funcall function pounds 10000) 10000))
	(t (error "pounds beyond the range of table pounds: ~A" pounds))))

(defun 30.63-pounds-to-floor-pounds (pounds)
  (%30.63-pounds-to-funcall-pounds #'floor pounds))
(defun 30.63-pounds-to-ceiling-pounds (pounds)
  (%30.63-pounds-to-funcall-pounds #'ceiling pounds))

(defun %proof-gallons-by-weight-and-proof (weight proof)
  (multi-dimensional-array-interpolation
   (list weight
	 (30.63-pounds-to-floor-pounds weight)
	 (30.63-pounds-to-ceiling-pounds weight))
   (list proof
	 (floor proof)
	 (ceiling proof))
   #'(lambda (w p)
       (aref *30.63-proof-gallons-by-weight-and-proof*
			(1- p)
			(30.63-pounds-to-floor-index w)))
   nil
   -1))

(defgeneric proof-gallons-by-weight-and-proof (weight proof)
  (:documentation
   "Returns the number of proof gallons by the provided
WEIGHT and PROOF as units.  Returns nil otherwise.")
  (:method :before (weight proof)
    (declare (ignore weight proof))
    (unless *30.63-proof-gallons-by-weight-and-proof*
      (load-30.63-table-array)))
  (:method ((weight unit) (proof unit))
    (let* ((pf (convert-unit proof 'proof))
	   (pounds (convert-unit weight 'pounds))
	   (%pgs (%proof-gallons-by-weight-and-proof pounds pf)))
      (unless (= -1 %pgs)
	(reduce-unit (list %pgs 'gallons)))))  
  (:method ((weight list) proof)
    (proof-gallons-by-weight-and-proof (reduce-unit weight) proof))
  (:method (weight (proof list))
    (proof-gallons-by-weight-and-proof weight (reduce-unit proof)))
  (:method (weight (proof symbol))
    (proof-gallons-by-weight-and-proof weight (reduce-unit proof)))
  (:method ((weight number) proof)
    (proof-gallons-by-weight-and-proof (reduce-unit (list weight 'pounds))
				       proof))
  (:method (weight (proof number))
    (proof-gallons-by-weight-and-proof weight
				       (reduce-unit (list proof 'proof)))))

(defvar pgs/proof-lbs 'alias "Fn alias for proof-gallons-by-weight-and-proof")
(setf (symbol-function pgs/proof-lbs) #'proof-gallons-by-weight-and-proof)
;;;; 30.64 Table 4, showing the fractional part of a gallon per pound
;;;;    at each percent and each tenth percent of proof of spirituous liquor

(defvar *30.64-gallons-per-pound-table* nil
  "Table No. 4, showing the fractional part of a gallon per pound at each percent and each tenth percent of proof of spirituous liquor.")

(defun load-30.64-table-array (&optional (force nil))
  (lazy-load-table *30.64-gallons-per-pound-table*
		   "part30.64.tbl4.array.lisp"
		   force))

(defun %gallons-per-pound (proof-ref column-number)
  (assert (and (or (= column-number 1)
		   (= column-number 2))
	       (<= 0 proof-ref 1990)))
  (multi-dimensional-array-interpolation
   (list proof-ref
	 (floor proof-ref)
	 (ceiling proof-ref))
   (list column-number
	 column-number
	 column-number)
   #'(lambda (p c)
       (aref *30.64-gallons-per-pound-table* p c))
   nil
   nil))

(defun column-id (column-keyword)
  (ecase column-keyword
    (:wine 1)
    (:proof 2)
    ((or 1 2) column-keyword)))

(defgeneric volume-per-mass-ratio (proof column)
  (:documentation
   "Returns the gallons per pound ratio for a given
concentration (PROOF,etc).  COLUMN can be one of
:WINE or :PROOF.")
  (:method :before (proof column)
    (declare (ignore proof column))
    (unless *30.64-gallons-per-pound-table*
      (load-30.64-table-array)))
  (:method ((proof unit) column)
    (let* ((pf (convert-unit proof 'proof))
	   (ratio (%gallons-per-pound (- (* 10 pf) 10) (column-id column))))
      (when ratio
	(reduce-unit (list ratio '(/ gallons pound))))))
  (:method ((proof list) column)
    (volume-per-mass-ratio (reduce-unit proof) column))
  (:method ((proof symbol) column)
    (volume-per-mass-ratio (reduce-unit proof) column))
  (:method ((proof number) column)
    (volume-per-mass-ratio (reduce-unit `(,proof proof)) column)))

(defvar v/m-ratio 'alias "Fn alias for volume-per-mass-ratio")
(setf (symbol-function 'v/m-ratio) #'volume-per-mass-ratio)

;;;; 30.65 Table 5, showing the weight per wine gallon (at 60 dF) and proof
;;;;    gallon at each percent of proof of spirituous liquor.

(defvar *30.65-pounds-per-gallon-table* nil
  "Table No. 5, showing the weight per wine gallon (at 60 F) and proof gallon at each percent of proof of spirituous liquor.")

(defun load-30.65-table-array (&optional (force nil))
  (lazy-load-table *30.65-pounds-per-gallon-table*
		   "part30.65.tbl5.array.lisp"
		   force))

(defun %pounds-per-gallon (proof-index column-number)
  (assert (and (or (= column-number 1)
		   (= column-number 2))
	       (<= 0 proof-index 199)))
  (multi-dimensional-array-interpolation
   (list proof-index (floor proof-index) (ceiling proof-index))
   (list column-number column-number column-number)
   #'(lambda (p c)
       (aref *30.65-pounds-per-gallon-table* p c))
   nil
   -1))

(defgeneric mass-per-volume (proof column)
  (:documentation
   "Returns the mass/volume for a given concentration (PROOF, etc).
COLUMN can be one of :WINE or :PROOF.")
  (:method :before (proof column)
    (declare (ignore proof column))
    (unless *30.65-pounds-per-gallon-table*
      (load-30.65-table-array)))
  (:method ((proof unit) column)
    (let* ((proof (convert-unit proof 'proof))
	   (w/v (%pounds-per-gallon (- proof 1) (column-id column))))
      (when w/v
	(reduce-unit (list w/v '(/ pounds gallon))))))
  (:method ((proof list) column)
    (mass-per-volume (reduce-unit proof) column))
  (:method ((proof symbol) column)
    (mass-per-volume (reduce-unit proof) column))
  (:method ((proof number) column)
    (mass-per-volume (reduce-unit `(,proof proof)) column)))

(defvar m/v 'alias "Fn alias for mass-per-volume.")
(setf (symbol-function 'm/v) #'mass-per-volume)

;;;; 30.66 Table 6, showing respective volumes of alcohol and water and the
;;;;    specific gravity in both air and vacuum of spirituous liquor.
;;;; 30.67 Table 7, for correction of volume of spirituous liquors to 60 dF.

;;; Subpart F - Optional Gauging Procedures
;;;; 30.71 Optional method for determination of proof of spirits containing
;;;;    solids of 400 mg or less per 100 mL.
;;;; 30.72 Recording obscuration by proprietors using the optional method
;;;;    for determination of proof.
