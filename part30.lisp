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

(intern "DENATURED-ALCOHOL" *package*)
(setf (find-class 'denatured-alcohol)
      (find-class 'denatured-spirits))

;; Gallon or wine gallon. The liquid measure equivalent to the volume of 231 cubic inches.
;; I.R.C. The Internal Revenue Code of 1954, as amended.
;; package: Any cask, barrel, drum, or similar container approved under the provisions of this chapter.
;; Proof. The ethyl alcohol content of a liquid at 60 degrees Fahrenheit, stated as twice the percent of ethyl alcohol by volume.
;; Proof gallon. A United States gallon of proof spirits, or the alcoholic equivalent thereof.
;; Proof spirits. That liquid which contains one-half its volume of ethyl alcohol of a specific gravity of seven thousand nine hundred and thirty-nine ten-thousandths (0.7939) in vacuum at 60 degrees Fahrenheit referred to water at 60 degrees Fahrenheit as unity.
;; Spirits, spirituous liquor, or distilled spirits. That substance known as ethyl alcohol, ethanol, or spirits of wine in any form, including all dilutions and mixtures thereof, from whatever source or by whatever process produced, but not denatured spirits unless specifically stated. For the sole purpose of gauging wine and alcoholic flavoring materials on the bonded premises of a distilled spirits plant, such alcoholic ingredients shall have the same meaning described herein to spirits, spirituous liquor, or distilled spirits.
;; This chapter. Title 27, Code of Federal Regulations, Chapter I (27 CFR Chapter I).
;; U.S.C. The United States Code.

;;; Subpart C - Gauging instruments
;;;; 30.21 Requirements
;;;; 30.22 Hydrometers and thermometers
(defvar *precision-spirit-hydrometer-table*
  (labels ((add-hydrometer (char lower upper precision)
	     `(,char
	       ,(reduce-unit `(,lower proof))
	       ,(reduce-unit `(,upper proof))
	       ,(reduce-unit `(,precision proof))))
	   (add-hydrometers (hydrometers)
	     (loop for (char lower upper precision) in hydrometers
		collect (add-hydrometer char lower upper precision))))
    (add-hydrometers
     '((#\F   0  20 0.2)
       (#\G  20  40 0.2)
       (#\H  40  60 0.2)
       (#\I  60  80 0.2)
       (#\K  75  95 0.2)
       (#\L  90 110 0.2)
       (#\M 105 125 0.2)
       (#\N 125 145 0.2)
       (#\P 145 165 0.2)
       (#\Q 165 185 0.2)
       (#\R 185 206 0.2))))
  "A table that holds information about which designation of hydrometer
should be used for a given proof range.  See PROOF-TO-HYDROMETER-DESIGNATION.
The fields in this table are DESIGNATION-CHARACTER, LOWER-PROOF-LIMIT,
UPPER-PROOF-LIMIT and PRECISION.  These are represented as units of PROOF.
See section §30.22")

(defvar *precision-spirit-hydrometer-temperature*
  (reduce-unit '(60 fahrenheit))
  "The calibration temperature for precision hydrometers.  See the hydrometer's
Certificate of Accuracy for information on temperature correction.  See section §30.22")

(defgeneric proof-to-hydrometer-designation (proof)
  (:documentation
   "Given a proof, selects a designation of hydrometer based upon the 
approved range for the designation.  Returns multiple values of the
character representation for the designation and the precision.
Consult your hydrometer's certificate of accuracy for more info.")
  (:method ((proof number))
    (loop for (designation lower upper precision)
       in *precision-spirit-hydrometer-table*
       when (<= (convert-unit lower 'proof)
		proof
		(convert-unit upper 'proof))
       return (values designation precision)))
  (:method ((proof unit))
    (proof-to-hydrometer-designation (convert-unit proof 'proof))))

(defvar *precision-thermometer-table*
  '(("Pencil type" 10 100 1)
    ("V-back" 10 100 1)
    ("Glass shell (earlier model)" 40 100 0.5)
    ("Glass shell (later model)" 40 100 0.25))
  "A table that represents the types of precision thermometers.  The
fields represent the DESCRIPTION, LOWER-FAHRENHEIT-RANGE, 
UPPER-FAHRENHEIT-RANGE and SUBDIVISION.  See §30.22")

(defun decimal-round (number decimal-place &optional (divisor 1))
  "Perform rounding of decimal numbers using the methods the TTB requires.
NUMBER is rounded to the DECIMAL-PLACE position using optional DIVISOR."
  (let ((factor (expt 10 decimal-place)))
    (multiple-value-bind (quotient remainder)
	(round (* number factor) divisor)
      ;; default behavior of CL:ROUND is to round down 0.5 remainders if the
      ;; quotient is even.  This is not the TTB required behavior!
      (when (and (= remainder 0.5) (evenp quotient))
	(setf quotient (1+ quotient)
	      remainder (* -1 remainder)))
      ;; scale values back to the correct decimal place.
      (setf quotient (float (/ quotient factor))
	    remainder (float (/ remainder factor)))
      ;; return our values as if we were a typical call to ROUND.      
      (values quotient remainder))))

;;;; 30.23 Use of precision hydrometers and thermometers

(defvar *hydrometer-reading-precision*
  (reduce-unit '(0.05 proof))
  "The precision to which each hydrometer reading should be made.  See section § 30.23")

(defvar *thermometer-reading-precision*
  (reduce-unit '(0.1 fahrenheit))
  "The precision to which each themometer reading should be made.  See section § 30.23")

;;;; 30.24 Specific gravity hydrometers

(defvar *precision-specific-gravity-hydrometer-table*
  `((1.0000 1.0500 0.0005)
    (1.0500 1.1000 0.0005)
    (1.1000 1.1500 0.0005)
    (1.1500 1.2000 0.0005)
    (1.2000 1.2500 0.0005))
  "A table of the ranges of specific gravity that precision grade hydrometers
can read.  The fields are LOWER-SPECIFIC-GRAVITY, UPPER-SPECIFIC-GRAVITY, and
SUBDIVISION.  See section § 30.24")

(defvar *precision-specific-gravity-hydrometer-temperature*
  (reduce-unit '(60 fahrenheit))
  "The calibration temperature for precision grade specific gravity hydrometers.
See section § 30.24.")

;; TODO - make specific gravity units?
(defvar *precision-specific-gravity-hydrometer-tollerance*
  0.0005
  "The specific gravity tollerance for precision specific gravity hydrometers.
See section § 30.24.")

(defclass document ()
  ()
  (:documentation "The parent class of all documents."))

(defclass certification (document)
  ()
  (:documentation
   "A certification is a document that bestows a qualification on something."))

(defclass certificate-of-accuracy (certification)
  ()
  (:documentation
   "A certificate of accuracy belongs to a laboratory device and certifies
the accuracy of its measurement under a specific set of conditions and
operating procedures."))

;;; Incorporation by reference (TODO: make a system to track these?)
;; Standard Specification for ASTM, Hydrometers E 100-72 (1978)
;;   It might be possible to purchase this in the case it provides info
;;   on temperature correcting hydrometers, which is necessary before
;;   finding the true proof using table #1.

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
  "Uses the default-site-configuration to load Table No. 1."
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
	    (message
	     "~%Invalid index reference (~A,~A), using table-null value: ~A."
	     arg1 arg2 table-null)
	    (setf return-value table-null)))
	;; Do some error checking.
	(when (and (equal return-value table-null)
		   datum)
	  (signal datum arg1-floor arg1-ceiling arg2-floor arg2-ceiling))))
    ;; return our found value.
    return-value))

(defun 30.61-table-lookup (proof temperature)
  "Directly accesses Table No. 1 and returns a value in PROOF corrected by
TEMPERATURE.  Inputs are values in the range of 0 to 200 for PROOF and
0 to 100 degrees Fahrenheit for TEMPERATURE.  This is a helper function
for TRUE-PERCENT-PROOF"
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
    (declare (ignore proof temperature))
    (load-30.61-table-array))
  (:method ((proof unit) (temperature unit))
    (let* ((pf (convert-unit proof 'proof))
	   (temp (convert-unit temperature 'fahrenheit))
	   (%proof (30.61-table-lookup pf temp)))
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
  "Uses the default-site-configuration to load Table No. 2."
  (lazy-load-table *30.62-wine-gallons-and-proof-gallons-by-weight*
		   "part30.62.tbl2.assoc.lisp"
		   force))

(defun column-id (column-keyword)
  "Converts a keyword like :WINE or :PROOF into numerical column indexes."
  (ecase column-keyword
    (:wine 1)
    (:proof 2)
    (:alcohol 1)
    (:water 2)
    (:specific-gravity-air 3)
    (:specific-gravity-vacuum 4)
    ((or 1 2) column-keyword)))

(defun 30.62-table-lookup (lbs proof volume-type-index)
  "Directly accesses Table No. 2 and returns a value in gallons at 60 deg F.
Inputs are LBS in standard pounds and PROOF.  VOLUME-TYPE-INDEX is either 1 for
WINE-GALLONS to be returned and 2 for PROOF-GALLONS.  This is a helper
function for VOLUME-BY-WEIGHT-AND-PROOF."
  (assert (or (= volume-type-index 1)
	      (= volume-type-index 2)))
  (multi-dimensional-array-interpolation
   (list lbs ;; in 0.5 increments
	 (float (/ (floor (* 2 lbs) 1) 2))
	 (float (/ (ceiling (* 2 lbs) 1) 2)))
   (list proof
	 (floor proof)
	 (ceiling proof))
   #'(lambda (w p)
       (when (= w (round w))
	 (setf w (round w)))
       (nth volume-type-index
	    (assoc w
		   (cdr
		    (assoc p
			   *30.62-wine-gallons-and-proof-gallons-by-weight*)))))
   nil
   nil))

(defgeneric 30.62-volume-by-weight-and-proof (weight proof &key volume-type)
  (:documentation
   "Returns the a UNIT object containing the volume (at 60 F.) by 
WEIGHT and PROOF or NIL if the values fall outside of table ranges.")
  (:method :before (weight proof &key volume-type)
    (declare (ignore weight proof volume-type))
    (load-30.62-table-assoc))
  (:method ((weight unit)(proof unit) &key (volume-type (default-volume-type)))
    (let* ((pf (convert-unit proof 'proof))
	   (pounds (convert-unit weight 'pounds))
	   (volume-type-index (column-id volume-type))
	   (%gallons (30.62-table-lookup pounds pf volume-type-index)))
      (when %gallons
	(reduce-unit (list %gallons 'gallons)))))
  (:method ((weight list) proof &key (volume-type (default-volume-type)))
    (30.62-volume-by-weight-and-proof (reduce-unit weight)
				       proof
				       :volume-type volume-type))
  (:method (weight (proof list) &key (volume-type (default-volume-type)))
    (30.62-volume-by-weight-and-proof weight
				       (reduce-unit proof)
				       :volume-type volume-type))
  (:method ((weight number) proof &key (volume-type (default-volume-type)))
    (30.62-volume-by-weight-and-proof (reduce-unit (list weight 'pounds))
				       proof
				       :volume-type volume-type))
  (:method (weight (proof number) &key (volume-type (default-volume-type)))
    (30.62-volume-by-weight-and-proof weight
				       (reduce-unit (list proof 'proof))
				       :volume-type volume-type)))

;; TODO add in temperature correction.

;;;; 30.63 Table 3, determining the # of proof gallons from the weight
;;;;    and proof of spirituous liquor

(defvar *30.63-proof-gallons-by-weight-and-proof* nil
  "Table 3, Determination of the # of proof gallons from the weight
and proof of spirituous liquor")

(defun load-30.63-table-array (&optional force)
  "Uses the default-site-configuration to load Table No. 3."
  (lazy-load-table *30.63-proof-gallons-by-weight-and-proof*
		   "part30.63.tbl3.array.lisp"
		   force))

(defvar *30.63-table-pound-columns*
  '(70000 60000 50000 40000 30000 20000 10000 9000 8000 7000 6000
    5000 4000 3000 2000 1000 900 800 700 600 500 400 300 200 100)
  "A list of columns from table no. 3.  We'll break a number into it's power 
of 10s and find the correct combination of columns to make the number.")

(defun %parse-number-to-array-values (number)
  "Deconstructs NUMBER in to a set of columns from table no. 3 and a 
remainder that can't be looked up on this table.  The TTB approved 
method uses this approach rather than direct interpolation."
  (assert (<= 0 number (first *30.63-table-pound-columns*)))
  (loop for column-value in *30.63-table-pound-columns*
     with remainder = number
     with columns = ()
     counting column-value into column
     when (/= remainder (rem remainder column-value))
     do (setf remainder (rem remainder column-value)
	      columns (cons (- (length *30.63-table-pound-columns*)
			       column)
			    columns))
     finally (return (list columns remainder))))

(defun %30.63-table-lookup (proof column-index)
  "Performs a lookup of the table no. 3 by PROOF and COLUMN-INDEX."
  (assert (and (<= 1 proof 200) (<= 0 column-index 24)))
  (aref *30.63-proof-gallons-by-weight-and-proof*
	(truncate (1- proof))
	column-index))

(defun 30.63-table-lookup (weight proof)
  "Directly accesses Table No. 3 and returns a value in proof gallons at 60 
deg F.  Inputs are LBS in standard pounds and PROOF.  This is a 
helper function for VOLUME-BY-WEIGHT-AND-PROOF."
  (when (and (<= 0 proof 200)
	     (<= 0 weight 70000))
    (flet ((optionally-add (columns factor)
	     (let ((column (car columns)))
	       (if column
		   (decimal-round (/ (%30.63-table-lookup proof column) factor)
				  1)
		   0))))
    (destructuring-bind (columns remainder)
	(%parse-number-to-array-values weight)
      (+ (loop for column in columns
	    sum (%30.63-table-lookup proof column))
	 (destructuring-bind (10s-columns 10s-remainder)
	     (%parse-number-to-array-values (* 10 remainder))
	   (destructuring-bind (1s-columns 1s-remainder)
	       (%parse-number-to-array-values (* 10 10s-remainder))
	     (destructuring-bind (dp-columns dp-remainder)
		 (%parse-number-to-array-values (* 10 1s-remainder))
	       (declare (ignore dp-remainder))
	       (+ (optionally-add 10s-columns 10)
		  (optionally-add 1s-columns 100)
		  (optionally-add dp-columns 1000))))))))))

(defgeneric 30.63-volume-by-weight-and-proof (weight proof &key volume-type)
  (:documentation
   "Returns the volume provided by WEIGHT and PROOF as a UNIT.  Returns nil 
otherwise.")
  (:method :before (weight proof &key volume-type)
    (declare (ignore weight proof volume-type))
    (load-30.63-table-array))
  (:method ((weight unit) (proof unit)
	    &key (volume-type (default-volume-type)))
    (let* ((pf (convert-unit proof 'proof))
	   (pounds (convert-unit weight 'pounds))
	   (pgs (30.63-table-lookup pounds pf)))
      (when pgs
	(when (eql :wine volume-type)
	  (setf pgs (* 100 (/ pgs pf))))
	(reduce-unit (list (decimal-round pgs 1) 'gallons)))))
  (:method ((weight list) proof &key (volume-type (default-volume-type)))
    (30.63-volume-by-weight-and-proof (reduce-unit weight)
				      proof
				      :volume-type volume-type))
  (:method (weight (proof list) &key (volume-type (default-volume-type)))
    (30.63-volume-by-weight-and-proof weight
				      (reduce-unit proof)
				      :volume-type volume-type))
  (:method (weight (proof symbol) &key (volume-type (default-volume-type)))
    (30.63-volume-by-weight-and-proof weight
				      (reduce-unit proof)
				      :volume-type volume-type))
  (:method ((weight number) proof &key (volume-type (default-volume-type)))
    (30.63-volume-by-weight-and-proof (reduce-unit (list weight 'pounds))
				      proof
				      :volume-type volume-type))
  (:method (weight (proof number) &key (volume-type (default-volume-type)))
    (30.63-volume-by-weight-and-proof weight
				      (reduce-unit (list proof 'proof))
				      :volume-type volume-type)))

;; TODO - add temperature correction to this.

;;;; 30.64 Table 4, showing the fractional part of a gallon per pound
;;;;    at each percent and each tenth percent of proof of spirituous liquor

(defvar *30.64-gallons-per-pound-table* nil
  "Table No. 4, showing the fractional part of a gallon per pound at each percent and each tenth percent of proof of spirituous liquor.")

(defun load-30.64-table-array (&optional (force nil))
  "Uses the default-site-configuration to load Table No. 4."
  (lazy-load-table *30.64-gallons-per-pound-table*
		   "part30.64.tbl4.array.lisp"
		   force))

(defun 30.64-table-lookup (proof column-number)
  "Directly accesses Table No. 4 and returns a value as a ratio in gallons
per pound.  Inputs are PROOF as degrees proof and COLUMN-NUMBER that
is the index of the column to return results from.  1 represents :WINE
and 2 for :PROOF."
  (assert (and (or (= column-number 1)
		   (= column-number 2))
	       (<= 1 proof 200)))
  (let ((proof-ref (- (* 10 proof) 10)))
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
     nil)))

(defgeneric volume-per-mass-ratio (proof volume-type)
  (:documentation
   "Returns the volume per mass ratio for a given concentration (PROOF,etc) 
as a UNIT.  VOLUME-TYPE can be one of :WINE or :PROOF.")
  (:method :before (proof volume-type)
    (declare (ignore proof volume-type))
    (load-30.64-table-array))
  (:method ((proof unit) volume-type)
    (let* ((pf (convert-unit proof 'proof))
	   (ratio (30.64-table-lookup pf (column-id volume-type))))
      (when ratio
	(reduce-unit (list ratio '(/ gallons pound))))))
  (:method ((proof list) volume-type)
    (volume-per-mass-ratio (reduce-unit proof) volume-type))
  (:method ((proof symbol) volume-type)
    (volume-per-mass-ratio (reduce-unit proof) volume-type))
  (:method ((proof number) volume-type)
    (volume-per-mass-ratio (reduce-unit `(,proof proof)) volume-type)))

(defgeneric 30.64-volume-by-weight-and-proof (weight proof &key volume-type)
  (:documentation
   "Returns the volume specified by WEIGHT and PROOF.  VOLUME-TYPE
can be one of :WINE and :PROOF.")
  (:method ((weight unit) (proof unit) &key (volume-type (default-volume-type)))
    (let* ((pf (convert-unit proof 'proof))
	   (ratio (volume-per-mass-ratio pf volume-type)))
      (when ratio
	(reduce-unit `(* ,weight ,ratio)))))
  (:method ((weight list) proof &key (volume-type (default-volume-type)))
    (30.64-volume-by-weight-and-proof (reduce-unit weight)
				      proof
				      :volume-type volume-type))
  (:method ((weight symbol) proof &key (volume-type (default-volume-type)))
    (30.64-volume-by-weight-and-proof (reduce-unit weight)
				      proof
				      :volume-type volume-type))
  (:method ((weight number) proof &key (volume-type (default-volume-type)))
    (30.64-volume-by-weight-and-proof (reduce-unit `(,weight pounds))
				      proof
				      :volume-type volume-type))
  (:method (weight (proof list) &key (volume-type (default-volume-type)))
    (30.64-volume-by-weight-and-proof weight
				      (reduce-unit proof)
				      :volume-type volume-type))
  (:method (weight (proof symbol) &key (volume-type (default-volume-type)))
    (30.64-volume-by-weight-and-proof weight
				      (reduce-unit proof)
				      :volume-type volume-type))
  (:method (weight (proof number) &key (volume-type (default-volume-type)))
    (30.64-volume-by-weight-and-proof weight
				      (reduce-unit `(,proof proof))
				      :volume-type volume-type)))

;; This table may also be used for ascertaining the quantity of water required
;; to reduce to a given proof.  To do this, divide the proof gallons of spirit
;; to be reduced by the fractional part of a proof gallon per pound of spirits
(defmacro unit-cond (unit &rest clauses)
  "A helper to dispatch functionality based upon matching units."
  `(cond ,@ (loop for clause in clauses
	       collect (destructuring-bind (test-form &rest forms)
			   clause
			 `((same-unit-p ,unit ',test-form)
			   ,@forms)))
	    (t "Unmatched unit in unit-cond: ~A" ,unit)))

(defgeneric mass-of-water-for-dilution (amount from-proof to-proof &key volume-type)
  (:documentation
   "Uses table 30.64 to calculate the mass of water for use in diluting
AMOUNT of spirits FROM-PROOF to TO-PROOF.  AMOUNT can be a mass of spirit
at proof FROM-PROOF or VOLUME-TYPE gallons of spirit at FROM-PROOF.  
If just a number is provided for AMOUNT, the type is assumed to be VOLUME-TYPE
gallons.")
  (:method ((amount unit) (from-proof unit) (to-proof unit)
	    &key (volume-type (default-volume-type)))
    (reduce-unit
     `(-
       (/
	,(unit-cond amount
	   (lbs (30.64-volume-by-weight-and-proof amount
						  from-proof
						  :volume-type :proof))
	   (gallons (if (eql :proof volume-type)
			amount
			(convert-to-proof-gallons amount from-proof))))
	,(volume-per-mass-ratio to-proof :proof))
       ,(unit-cond amount
	  (lbs amount)
	  (gallons (volume-to-mass amount
				   from-proof
				   :volume-type volume-type))))))
  (:method ((amount list) from-proof to-proof
	    &key (volume-type (default-volume-type)))
    (mass-of-water-for-dilution (reduce-unit amount)
				from-proof
				to-proof
				:volume-type volume-type))
  (:method ((amount symbol) from-proof to-proof
	    &key (volume-type (default-volume-type)))
    (mass-of-water-for-dilution (reduce-unit amount)
				from-proof
				to-proof
				:volume-type volume-type))
  (:method ((amount number) from-proof to-proof
	    &key (volume-type (default-volume-type)))
    (mass-of-water-for-dilution (reduce-unit `(,amount gallons))
				from-proof
				to-proof
				:volume-type volume-type))
  (:method (amount (from-proof list) to-proof
	    &key (volume-type (default-volume-type)))
    (mass-of-water-for-dilution amount
				(reduce-unit from-proof)
				to-proof
				:volume-type volume-type))
  (:method (amount (from-proof symbol) to-proof
	    &key (volume-type (default-volume-type)))
    (mass-of-water-for-dilution amount
				(reduce-unit from-proof)
				to-proof
				:volume-type volume-type))
  (:method (amount (from-proof number) to-proof
	    &key (volume-type (default-volume-type)))
    (mass-of-water-for-dilution amount
				(reduce-unit `(,from-proof proof))
				to-proof
				:volume-type volume-type))
  (:method (amount from-proof (to-proof list)
	    &key (volume-type (default-volume-type)))
    (mass-of-water-for-dilution amount
				from-proof
				(reduce-unit to-proof)
				:volume-type volume-type))
  (:method (amount from-proof (to-proof symbol)
	    &key (volume-type (default-volume-type)))
    (mass-of-water-for-dilution amount
				from-proof
				(reduce-unit to-proof)
				:volume-type volume-type))
  (:method (amount from-proof (to-proof number)
	    &key (volume-type (default-volume-type)))
    (mass-of-water-for-dilution amount
				from-proof
				(reduce-unit `(,to-proof proof))
				:volume-type volume-type)))

;;;; 30.65 Table 5, showing the weight per wine gallon (at 60 dF) and proof
;;;;    gallon at each percent of proof of spirituous liquor.

(defvar *30.65-pounds-per-gallon-table* nil
  "Table No. 5, showing the weight per wine gallon (at 60 F) and proof gallon at each percent of proof of spirituous liquor.")

(defun load-30.65-table-array (&optional (force nil))
  "Uses the default-site-configuration to load Table No. 5"
  (lazy-load-table *30.65-pounds-per-gallon-table*
		   "part30.65.tbl5.array.lisp"
		   force))

(defun 30.65-table-lookup (proof column-number)
  "Directly acceses Table No. 5 and returns a value in pounds.  Inputs are
PROOF in degrees proof and COLUMN-NUMBER, which represent which column
values will be returned from.  1 represents :WINE and 2 for :PROOF"
  (assert (and (or (= column-number 1)
		   (= column-number 2))
	       (<= 1 proof 200)))
  (decf proof) ; Our array is 0 indexed.
  (multi-dimensional-array-interpolation
   (list proof (floor proof) (ceiling proof))
   (list column-number column-number column-number)
   #'(lambda (p c)
       (aref *30.65-pounds-per-gallon-table* p c))
   nil
   -1))

(defgeneric mass-per-volume (proof &key volume-type)
  (:documentation
   "Returns the mass/volume for a given concentration (PROOF, etc).
VOLUME-TYPE can be one of :WINE or :PROOF.")
  (:method :before (proof &key volume-type)
    (declare (ignore proof volume-type))
    (load-30.65-table-array))
  (:method ((proof unit) &key (volume-type (default-volume-type)))
    (let* ((pf (convert-unit proof 'proof))
	   (w/v (30.65-table-lookup pf (column-id volume-type))))
      (when w/v
	(reduce-unit (list w/v '(/ pounds gallon))))))
  (:method ((proof list) &key (volume-type (default-volume-type)))
    (mass-per-volume (reduce-unit proof) :volume-type volume-type))
  (:method ((proof symbol) &key (volume-type (default-volume-type)))
    (mass-per-volume (reduce-unit proof) :volume-type volume-type))
  (:method ((proof number) &key (volume-type (default-volume-type)))
    (mass-per-volume (reduce-unit `(,proof proof)) :volume-type volume-type)))

(defgeneric volume-to-mass (volume proof &key volume-type)
  (:documentation
   "Converts VOLUME of type VOLUME-TYPE (:PROOF or :WINE) to a UNIT of mass.")
  (:method ((volume unit) (proof unit)
	    &key (volume-type (default-volume-type)))
    (reduce-unit `(* ,volume
		     ,(mass-per-volume proof :volume-type volume-type))))
  (:method ((volume list) proof &key (volume-type (default-volume-type)))
    (volume-to-mass (reduce-unit volume) proof :volume-type volume-type))
  (:method ((volume symbol) proof &key (volume-type (default-volume-type)))
    (volume-to-mass (reduce-unit volume) proof :volume-type volume-type))
  (:method ((volume number) proof &key (volume-type (default-volume-type)))
    (volume-to-mass (reduce-unit `(,volume gallons))
		    proof
		    :volume-type volume-type))
  (:method (volume (proof list) &key (volume-type (default-volume-type)))
    (volume-to-mass volume (reduce-unit proof) :volume-type volume-type))
  (:method (volume (proof symbol) &key (volume-type (default-volume-type)))
    (volume-to-mass volume (reduce-unit proof) :volume-type volume-type))
  (:method (volume (proof number) &key (volume-type (default-volume-type)))
    (volume-to-mass volume
		    (reduce-unit `(,proof proof))
		    :volume-type volume-type)))


;; TODO build the correct decimal rounding into each method..

;;;; 30.66 Table 6, showing respective volumes of alcohol and water and the
;;;;    specific gravity in both air and vacuum of spirituous liquor.

(defvar *30.66-volumes-and-specific-gravity* nil
  "Table No. 6: Showing respective volumes of alcohol and water and the specific gravity in both air and vacuum of spiritous liquor.")

(defun load-30.66-table-array (&optional (force nil))
  "Uses the default-site-configuration to load Table No. 6."
  (lazy-load-table *30.66-volumes-and-specific-gravity*
		   "part30.66.tbl6.array.lisp"
		   force))

(defun 30.66-table-lookup (proof column-number)
  "Directly accesses Table No. 6 and returns a value that maybe 
parts alcohol, parts water, specific gravity in air and specific
gravity in a vacuum.  Depending on the request column-number"
  (assert (and (or (= column-number 1)
		   (= column-number 2)
		   (= column-number 3)
		   (= column-number 4))
	       (<= 1 proof 200)))
  (decf proof)
  (multi-dimensional-array-interpolation
   (list proof (floor proof) (ceiling proof))
   (list column-number column-number column-number)
   #'(lambda (p c)
       (aref *30.66-volumes-and-specific-gravity* p c))
   nil
   nil))

(defun %diff-parts-water (from-proof to-proof)
  "Returns the gallons of water per gallon of FROM-PROOF spirit.  A helper
function for VOLUME-OF-WATER-FOR-DILUTION."
  (assert (and (<= 1 from-proof 200)
	       (<= 1 to-proof 200)
	       (> from-proof to-proof)))
  (/ (- (* (/ (30.66-table-lookup from-proof (column-id :alcohol))
	      (30.66-table-lookup to-proof (column-id :alcohol)))
	   (30.66-table-lookup to-proof (column-id :water)))
	(30.66-table-lookup from-proof (column-id :water)))
     100))

(defgeneric volume-of-water-for-dilution (amount from-proof to-proof
					  &key volume-type)
  (:documentation
   "Uses table 30.66 to calculate the volume of water for use in diluting 
AMOUNT of spirits FROM-PROOF to TO-PROOF.  AMOUNT can be a mass of spirit
at proof FROM-PROOF or VOLUME-TYPE gallons of spirit at FROM-PROOF.
If just a number is provided for AMOUNT, the type is assume to be 
VOLUME-TYPE gallons.")
  (:method :before (amount from-proof to-proof &key volume-type)
    (declare (ignore amount from-proof to-proof volume-type))
    (load-30.66-table-array))
  (:method ((amount unit) (from-proof unit) (to-proof unit)
	    &key (volume-type (default-volume-type)))
    (reduce-unit
     `(* ,(%diff-parts-water (convert-unit from-proof 'proof)
			     (convert-unit to-proof 'proof))
	 ,(unit-cond  amount 
		      (lbs (volume-by-weight-and-proof amount
						       from-proof
						       :volume-type
						       volume-type))
		      (gallons (if (eql :proof volume-type)
				   (convert-from-proof-gallons amount
							       from-proof)
				   amount))))))
  (:method ((amount list) from-proof to-proof
	    &key (volume-type (default-volume-type)))
    (volume-of-water-for-dilution (reduce-unit amount) from-proof to-proof
				  :volume-type volume-type))
  (:method ((amount symbol) from-proof to-proof
	    &key (volume-type (default-volume-type)))
    (volume-of-water-for-dilution (reduce-unit amount) from-proof to-proof
				  :volume-type volume-type))
  (:method ((amount number) from-proof to-proof
	    &key (volume-type (default-volume-type)))
    (volume-of-water-for-dilution (reduce-unit `(,amount gallons))
				  from-proof
				  to-proof
				  :volume-type volume-type))
  (:method (amount (from-proof list) to-proof
	    &key (volume-type (default-volume-type)))
    (volume-of-water-for-dilution amount (reduce-unit from-proof) to-proof
				  :volume-type volume-type))
  (:method (amount (from-proof symbol) to-proof
	    &key (volume-type (default-volume-type)))
    (volume-of-water-for-dilution amount (reduce-unit from-proof) to-proof
				  :volume-type volume-type))
  (:method (amount (from-proof number) to-proof
	    &key (volume-type (default-volume-type)))
    (volume-of-water-for-dilution amount
				  (reduce-unit `(,from-proof proof))
				  to-proof
				  :volume-type volume-type))
  (:method (amount from-proof (to-proof list)
	    &key (volume-type (default-volume-type)))
    (volume-of-water-for-dilution amount from-proof (reduce-unit to-proof)
				  :volume-type volume-type))
  (:method (amount from-proof (to-proof symbol)
	    &key (volume-type (default-volume-type)))
    (volume-of-water-for-dilution amount from-proof (reduce-unit to-proof)
				  :volume-type volume-type))
  (:method (amount from-proof (to-proof number)
	    &key (volume-type (default-volume-type)))
    (volume-of-water-for-dilution amount
				  from-proof
				  (reduce-unit `(,to-proof proof))
				  :volume-type volume-type)))


(defvar *weight-of-one-gallon-water-in-air*
  (reduce-unit '(8.32823 gallons)))

(defgeneric 30.66-volume-by-weight-and-proof (weight proof &key volume-type)
  (:documentation
   "Returns the volume specified by WEIGHT and PROOF.  VOLUME-TYPE
can be one of :WINE and :PROOF")
  (:method ((weight unit) (proof unit) &key (volume-type (default-volume-type)))
    (let ((gallons
	   (reduce-unit
	    `(,(/ (convert-unit weight 'lbs)
		  (* (convert-unit *weight-of-one-gallon-water-in-air* 'gallons)
		     (30.66-table-lookup (convert-unit proof 'proof)
					 (column-id :specific-gravity-air))))
	       gallons))))
      (if (eql :proof volume-type)
	  (convert-to-proof-gallons gallons proof)
	  gallons)))
  (:method ((weight list) proof &key (volume-type (default-volume-type)))
    (30.66-volume-by-weight-and-proof (reduce-unit weight) proof
				      :volume-type volume-type))
  (:method ((weight symbol) proof &key (volume-type (default-volume-type)))
    (30.66-volume-by-weight-and-proof (reduce-unit weight) proof
				      :volume-type volume-type))
  (:method ((weight number) proof &key (volume-type (default-volume-type)))
    (30.66-volume-by-weight-and-proof (reduce-unit `(,weight lbs)) proof
				      :volume-type volume-type))
  (:method (weight (proof list) &key (volume-type (default-volume-type)))
    (30.66-volume-by-weight-and-proof weight (reduce-unit proof)
				      :volume-type volume-type))
  (:method (weight (proof symbol) &key (volume-type (default-volume-type)))
    (30.66-volume-by-weight-and-proof weight (reduce-unit proof)
				      :volume-type volume-type))
  (:method (weight (proof number) &key (volume-type (default-volume-type)))
    (30.66-volume-by-weight-and-proof weight (reduce-unit `(,proof proof))
				      :volume-type volume-type)))

(defun volume-by-weight-and-proof (weight proof &key (volume-type
						      (default-volume-type)))
  "A function that calls through several VOLUME-BY-WEIGHT-AND-PROOF
routines until one returns a value.  WEIGHT can be a UNIT,UNIT-
DESCRIPTOR, or NUMBER that will be interpreted as a value in pounds.
PROOF can be a UNIT, UNIT-DESCRIPTOR, or NUMBER that will be
interpreted as a value in PROOF.  VOLUME-TYPE can be one of :WINE
or :PROOF for use in dealing with the desired expression of volume."
  (or (30.62-volume-by-weight-and-proof weight
					proof
					:volume-type volume-type)
      (30.63-volume-by-weight-and-proof weight
					proof
					:volume-type volume-type)
      (30.64-volume-by-weight-and-proof weight
					proof
					:volume-type volume-type)
      ;; TODO need to organize these from most specific to most general.
      (30.66-volume-by-weight-and-proof weight
					proof
					:volume-type volume-type)))


;;;; 30.67 Table 7, for correction of volume of spirituous liquors to 60 dF.

(defvar *30.67-temperature-correction-table* nil
  "Table No. 7:  For correction of volume of sprituous liquors to 60 deg F.")

(defun load-30.67-table-array (&optional (force nil))
  "Loads the temperature correction table for § 30.67.  The optional FORCE
 argument forces the reloading of this table.  See *default-site-configuration*
to always force reloading of tables."
  (lazy-load-table *30.67-temperature-correction-table*
		   "part30.67.tbl7.array.lisp"
		   force))

(defun 30.67-table-lookup (proof fahrenheit)
  "Performs a multi-dimensional array lookup and interpolation on table 7
from § 30.67.  PROOF should be a numerical value between 0 and 200.  FAHRENHEIT
should be a numerical value between 18 and 100."
  (assert (and (<= 18 fahrenheit 100)
	       (<= 0 proof 200)))
  (let ((proof-index (/ proof 5))
	(temp-index (/ (- fahrenheit 18) 2)))
    (multi-dimensional-array-interpolation
     (list proof-index
	   (floor proof-index)
	   (ceiling proof-index))
     ;; TODO the code states that we need to interpolate +60 and -60 temps
     ;;  differently.  For some reason I get the correct answer here, so
     ;;  maybe everything just works, but I have a feeling something is off here
     (list temp-index
	   (floor temp-index)
	   (ceiling temp-index))
     #'(lambda (p f)
	 (aref *30.67-temperature-correction-table* p f))
     nil
     nil)))

(defgeneric temperature-correct (volume proof temperature &key convert)
  (:documentation
   "Returns the VOLUME of PROOF spirit adjusted by TEMPERATURE.  See § 30.67.
CONVERT can be one of :TO-60F or :FROM-60F indicating if the conversion
should convert VOLUME at TEMPERATURE TO a volume at 60F or if the
conversion should convert VOLUME at TEMPERATURE FROM a volume at 60F.
In ihe method given by §30.67 :TO-60F is a multiplication operation
and :FROM-60F is division.")
  (:method :before (volume proof temperature &key (convert :to-60f))
    (declare (ignore volume proof temperature convert))
    (unless *30.67-temperature-correction-table*
      (load-30.67-table-array)))
  (:method ((volume unit) (proof unit) (temperature unit) &key (convert :to-60f))
    (let ((fn (ecase convert (:to-60f '*) (:from-60f '/)))
	  (factor (30.67-table-lookup (convert-unit proof 'proof)
				      (convert-unit temperature 'fahrenheit))))
      (reduce-unit `(,fn ,volume ,factor))))
  (:method ((volume list) proof temperature &key (convert :to-60f))
    (temperature-correct (reduce-unit volume)
			 proof
			 temperature
			 :convert convert))
  (:method ((volume symbol) proof temperature &key (convert :to-60f))
    (temperature-correct (reduce-unit volume)
			 proof
			 temperature
			 :convert convert))
  (:method ((volume number) proof temperature &key (convert :to-60f))
    (temperature-correct (reduce-unit (list volume 'gallons))
		       proof
		       temperature
		       :convert convert))
  (:method (volume (proof list) temperature &key (convert :to-60f))
    (temperature-correct volume
			 (reduce-unit proof)
			 temperature
			 :convert convert))
  (:method (volume (proof symbol) temperature &key (convert :to-60f))
    (temperature-correct volume
			 (reduce-unit proof)
			 temperature
			 :convert convert))
  (:method (volume (proof number) temperature &key (convert :to-60f))
    (temperature-correct volume
			 (reduce-unit (list proof 'proof))
			 temperature
			 :convert convert))
  (:method (volume proof (temperature list) &key (convert :to-60f))
    (temperature-correct volume
			 proof
			 (reduce-unit temperature)
			 :convert convert))
  (:method (volume proof (temperature symbol) &key (convert :to-60f))
    (temperature-correct volume
			 proof
			 (reduce-unit temperature)
			 :convert convert))
  (:method (volume proof (temperature number) &key (convert :to-60f))
    (temperature-correct volume
			 proof
			 (reduce-unit (list temperature 'fahrenheit))
			 :convert convert)))
;; Testing
;;  TODO optionally run these tests in the future.

;; 30.23
;; From the example section.
(ok (= 190.0
       (decimal-round (convert-unit (true-percent-proof '(192.82 proof)
							'(72.15 fahrenheit))
				    'proof)
		      1)))

;; Table 1
;; Example No. 1
(ok (= 189.5
       (decimal-round
	(convert-unit (true-percent-proof 193 75) 'proof)
	1)))

;; Table 2
;; Example No. 1:
(ok (= 42.8
       (decimal-round
	(convert-unit (30.62-volume-by-weight-and-proof '(334 lbs)
							'(96 proof))
		      'gallons)
	1)))
;; Example No. 2:
(ok (= 51.5
       (decimal-round
	(convert-unit (30.62-volume-by-weight-and-proof '(395 lbs)
							'(113 proof))
		      'gallons)
	1)))

;; Table 3
;; Example No. 1:  A tank car of spirits of 190 degrees of proof weighed
;;   60,378 pounds net.  We find --
(ok (= 16884.1
       (decimal-round
	(convert-unit (30.63-volume-by-weight-and-proof '(60378 lbs)
							'(190 proof)
							:volume-type :proof)
		      'gallons)
	1)))
;; Example No. 2: A package of spirits at 86 proof weighed 321.5 lbs net.
;;   We find --
(ok (= 35.1
       (decimal-round
	(convert-unit (30.63-volume-by-weight-and-proof '(321.5 lbs)
							'(86 proof)
							:volume-type :proof)
		      'gallons)
	1)))

;; Table 4
;; Example No. 1:
(ok (= 11921.6
       (decimal-round
	(transform-units '(81000 lbs)
			 'gallons
			 (list (volume-per-mass-ratio 190 :wine)))
	1)))

;; Example No. 2:
(ok (= 22650.8
       (decimal-round
	(transform-units '(81000 lbs)
			 'gallons
			 (list (volume-per-mass-ratio 190 :proof)))
	1)))

;; Example No. 3:
;;  It is desired to ascertain the quantity of water needed to reduce 1000 lbs
;;  of of 200 proof spirits, 302.58 proof gallons, to 190 proof.

(ok (= 82.03
       (decimal-round
	(convert-unit
	 (mass-of-water-for-dilution '(1000 lbs) '(200 proof) '(190 proof))
	 'lbs)
	2)))

;; TODO implement Example No. 4:
;;   Is there a need to have both an input and output volume-type added to
;;   volume-by-weight-and-proof?

;; Table 5
;; Example No. 1:  It is desired to ascertain the weight of 100 wine
;;   gallons of 190 proof spirits:
(ok (= 679.43
       (decimal-round
	(convert-unit
	 (volume-to-mass '(100 gallons) '(190 proof) :volume-type :wine)
	 'lbs)
	2)))

;; Example No. 2: It is desired to ascertain the weight of 100 proof
;;    gallons of 190 proof spirits:
(ok (= 357.60
       (decimal-round
	(convert-unit
	 (volume-to-mass '(100 gallons) '(190 proof) :volume-type :proof)
	 'lbs)
	2)))

;; Table 6
;; Example No. 1:  It is desired to reduce spirits of 191 proof to 188 proof.
;;   We find that 191 proof spirits contain 95.5 parts alcohol and 5.59 parts
;;   water, and 188 proof spirits contains 94.0 parts alcohol and 7.36 parts
;;   water.
(ok (= 1.89 ; The example lists this as 1.84, due to a truncation in step #1
       (decimal-round
	(convert-unit
	 (volume-of-water-for-dilution '(100 gallons)
				       '(191 proof)
				       '(188 proof)
				       :volume-type :wine)
	 'gallons)
	2)))

;; Example No. 2
(ok (= 12.43 ; The example lists this as 12.42, due to truncation in step #1
       (decimal-round
	(convert-unit
	 (volume-of-water-for-dilution '(100 gallons)
				       '(112 proof)
				       '(100 proof)
				       :volume-type :wine)
	 'gallons)
	2)))

;; Example No. 3
(ok (= 54.05
       (decimal-round
	(convert-unit
	 (30.66-volume-by-weight-and-proof '(400 lbs)
					   '(141 proof)
					   :volume-type :wine)
	 'gallons)
	2)))

;; Example No. 4
(ok (= 76.21
       (decimal-round
	(convert-unit
	 (30.66-volume-by-weight-and-proof '(400 lbs)
					   '(141 proof)
					   :volume-type :proof)
	 'gallons)
	2)))


;; Table 7
;; Example 1 - Ascertain the volume at 60 degrees Fahrenheit of 1000 wine
;;   gallons of 190 proof spirits at 76 degrees Fahrenheit.

(ok (= 991
       (decimal-round
	(convert-unit
	 (temperature-correct '(1000 gallons)
			      '(190 proof)
			      '(76 fahrenheit)
			      :convert :to-60f)
	 'gallons)
	0)))

;; Example 2 - Ascertain the capacity of a container of 190 proof spirits
;;    at 76 degrees Fahrenheit, shown by Table 2 to contain 55.1 wine gallons
;;    at 60 degrees Fahrenheit
(ok (= 55.6
       (decimal-round
	(convert-unit
	 (temperature-correct '(55.1 gallons)
			      '(190.0 proof)
			      '(76 fahrenheit)
			      :convert :from-60f)
	 'gallons)
	1)))
;; Example 3 - It is desired to correct spirits of 180 proof at 51 degrees F.
;;   temperature:
(ok (= 1.0055
       (decimal-round
	(30.67-table-lookup 180 51)
	4)))
;; Example 4 - It is desired to correct spirits of 180 proof at 53 degrees F.
(ok (= 1.004
       (decimal-round
	(30.67-table-lookup 180 53)
	3)))

;; Example 5 - It is desired to ascertain the correction factor for spirits of
;;   112 proof at 45 degrees F.
(ok (= 1.0059
       (decimal-round
	(30.67-table-lookup 112 47)
	4)))

;; Example 6 - It is desired to ascertain the correction factor for spirits of
;;   97 proof at 93 degrees F.
(ok (= 0.9851
       (decimal-round
	(30.67-table-lookup 97 93)
	4)))
;;; TODO - check this... The above "works", but the method differs.
;;    Looking closer at the description of the method, it seems that
;;    interpolation factors for temperatures above 60 deg F should be
;;    subtracted from the factor for the next lower proof!
;;    Maybe this what I have, but I need to take a closer look.

;;; Subpart F - Optional Gauging Procedures
;;;; 30.71 Optional method for determination of proof of spirits containing
;;;;    solids of 400 mg or less per 100 mL.
;;;; 30.72 Recording obscuration by proprietors using the optional method
;;;;    for determination of proof.
