;;; part5.lisp

(in-package #:etoh.cfr27)

;;; Subpart A - Scope
;; 5.1 - General.
;; 5.2 - Related Regulations
;; 5.3 - Forms prescribed.
;; 5.4 - Delegations of the Administration
;;; Subpart B - Definitions
;; 5.11 - Meaning of terms.

(defclass advertisement () ()
  (:documentation "See §5.62 for meaning of term as used in subpart H."))

(defclass ttb-officer (person) ()
  (:documentation
   "An officer or employee of the Alcohol and Tobacco Tax and Trade
Bureau (TTB) authorized to perform any functions relating to
the administration or enforcement of this part by TTB order 1135.5."))

(defclass container ()
  ((material :accessor material :initarg :material)
   (volume :accessor volume :initarg :volume)
   (uses :accessor uses :initform 0 :initarg :uses)
   (max-uses :accessor max-uses :initarg :max-uses))
  (:documentation "The parent class of all containers."))

(defgeneric new-p (object)
  (:documentation "Returns T while the object has zero uses.")
  (:method ((object container))
    (when (= 0 (uses object)) t)))

(defgeneric remaining-uses (object)
  (:documentation
   "Returns the number of uses remaining or T if unlimited uses are allowed.")
  (:method :around ((object container))
	   (if (slot-boundp object 'max-uses)
	       (call-next-method)
	       ;; If we don't have a max-uses then we have unlimited uses.
	       t))
  (:method ((object container))
    (let ((uses-left (- (max-uses object) (uses object))))
      (when (< 0 uses-left)
	uses-left))))

;; Might be room for a macro here to reduce repetition of code.
(defgeneric percent-used (object)
  (:documentation
   "Returns the percentage of usage an object has received.  Results are as a UNIT of percent.")
  (:method :around ((object container))
	   (if (slot-boundp object 'max-uses)
	       (call-next-method)
	       t))
  (:method ((object container))
    (with-accessors ((max-uses max-uses)
		     (uses uses))
	object
      (reduce-unit `(,(* (/ uses max-uses) 100) percent)))))

;; We start with physics ;)  This definition goes deep.. maybe this definition is better
;; suited for the top level etoh.cfr27.lisp file...
(defclass material ()
  ((name :accessor name :initarg :name)))
(defclass color ()
  ((value :accessor value :initarg :value))
  (:documentation
   "The parent class of all colors."))
(defclass lovibond (color)
  ()
  (:documentation
   "A color measurement used here primarily to measure wort color.. represent as a unit?"))
(defclass srm (color)
  ()
  (:documentation
   "A color standard.  Need flushing out of this.. represent as a unit?"))

;;; Oak material (use in containers and as a treament).
(defclass oak (material)
  () ; TODO go into grain, toast, surface area, 
  (:default-initargs :name "oak"))

(defclass bottle (container)
  ()
  (:documentation
   "Any container, irrespective of the material from which made, used
for the sale of distilled spirits at retail.")
  (:default-initargs :max-uses 1) #| I assume this is true based on 
  old rules about bottle reuse being a crime.  TODO verify this is true. |#)
(defclass oak-container (container)
  ((toast-level :accessor toast-level :initarg :toast-level))
  (:default-initargs :material (make-instance 'oak)))
(defclass charred-oak-container (oak-container)
  ()
  (:default-initargs :toast-level :charred))
(defclass charcoal (material)
  ()
  (:default-initargs :name "charcoal"))
(defclass grain (material)
  ()
  (:default-initargs :name "grain"))
(defclass barley (grain)
  ()
  (:default-initargs :name "barley"))
(defclass corn (grain)
  ()
  (:default-initargs :name "corn"))
(defclass rye (grain)
  ()
  (:default-initargs :name "rye"))
(defclass wheat (grain)
  ()
  (:default-initargs :name "wheat"))
(defclass malt (treatment)
  ;; TODO flush out these classes... ASBC I think will be the standard here.
  ((flavor :accessor flavor :initarg :flavor)
   (aroma :accessor aroma :initarg :aroma)
   (fan :accessor fan :initarg :fan
	:documentation "Free Amino Acid Nitrogen.")
   (diastatic-power :accessor dp :initarg :diastatic-power)
   (kolbach-index :accessor kolbach-index :initarg :kolbach-index)
   (color :accessor color :initarg :color))
  (:default-initargs :name "malted grain"))
(defclass malted-barley (malt barley)
  ()
  (:default-initargs :name "malted barley"
    :material (make-instance 'barley)))
(defclass malted-rye (malt rye)
  ()
  (:default-initargs :name "malted rye"))

(defclass production-stage ()
  ((proof :accessor proof :initarg :proof)))

(defclass distillation (production-stage)
  ((still :accessor still :initarg :still)
   (wine-gallons :accessor wine-gallons :initarg :wine-gallons)
   (proof-gallons :accessor proof-gallons :initarg :proof-gallons)))
(defclass bottling (production-stage)
  ((bottle :accessor bottle :initarg :bottle)))
(defclass treatment (production-stage)
  ((material :accessor material :initarg :material)
   (duration :accessor duration :initarg :duration)
   (amount :accessor amount :initarg :amount)
   (max-amount :accessor max-ammount :initarg :max-amount))
  (:documentation "A class of treatments."))
(defclass storage (production-stage)
  ((container :accessor container :initarg :container)
   (duration :accessor duration :initarg :duration)))
(defclass source (production-stage)
  ((material :accessor material :initarg :material)
   (amount :accessor amount :initarg :amount)))

(defclass spirits ()
  ()
  (:documentation "The parent class of all spirits."))

(defclass distilled-spirits (spirits)
  ((distillations :accessor distillations :initarg :distillations)
   (bottlings :accessor bottlings :initarg :bottlings)
   (storages :accessor storages :initarg :storages)
   (sources :accessor sources :initarg :sources)
   (treatments :accessor treatments :initarg :treatments))
  (:documentation
   "Ethyl alcohol, hydrated oxide of ethyl, spirits of wine, whiskey, rum,
brandy, gin, and other distilled spirits, including all dilutions and
mixtures thereof, for non-industrial use.  The term 'distilled spirits'
shall not include mixtures containing wine, bottled at 48 degrees of
proof or less, if the mixture contains more than 50 percent wine on a
proof gallon basis."))

;; 1 Gallon is defined as 231 cubic-inches @ 60F &
;;    unit-formulas with SBCL gives 230.999,
;;    I think close enough.

;; (bulk-p '(10 liter)) -> T
(defclass bulk ()
  ()
  (:documentation "A class for all bulk items."))
(defun bulk-p (capacity)
  (> (convert-unit capacity 'gallons) 1))

;; Interstate or foreign commerce.. it would be nice to have a
;;    geolocation service here to allow for long/lat

;; 1 Liter / litre = 33.814 US fl oz, which is what
;;    unit-formulas gives with SBCL, yay!

(defclass permittee (person) ()
  (:documentation
   "Any person holding a basic permit under the Federal Alcohol 
Administration act."))

(defclass person ()
  ((name :accessor name))
  (:documentation
   "Any individual, partnership, join stock company, business trust,
association, coporation, or other form of business enterprise,
including a receiver, trustee, or liquidating agent and including
as officer of employee of any agency of a State or political
subdivision thereof; and the term 'trade buyer' means any person
who is a wholesale or retailer."))

;; Produced at, means the composite proof of the spirits after
;; completion of distillation and before reduction in proof.

(unit-formulas::defformulae* convert-to-proof-gallons
    ((volume gallons)
     (proof proof)
     (comp-proof proof 100))
  (/ (* volume proof) comp-proof))
(unit-formulas::defformulae* convert-from-proof-gallons
    ((proof-gallons gallons)
     (proof proof)
     (comp-proof proof 100))
  (* (/ proof-gallons proof) comp-proof))

(unit-formulas::define-units (proof-gallon proof-gallons pg pgs)
    (formula :convert-to convert-to-proof-gallons
	     :convert-from convert-from-proof-gallons))

;; CFR27> (reduce-unit '((1 gallon) (100 proof) pgs))
;; #<UNIT 0.0037854117647058826d0 m^3>
;; CFR27> (convert-unit * 'gallons)
;; 1.0d0
;; CFR27> (convert-unit ** 'shot)
;; 85.33486360957208d0

;;; "Proof" that 1 US proof-gallon is equal to 85.3 US proof-shots

;; CFR27> (reduce-unit '((1 shot) (100 proof) pgs))
;; #<UNIT 4.4359498622099745d-5 m^3>
;; CFR27> (convert-unit * 'gallons)
;; 0.011718539852307553d0
;; CFR27> (* * 85.3)
;; 0.9995914851639798d0

;; Season
(defun season (timestamp)
  (with-decoded-timestamp (:month month :day day)
      timestamp
    (let ((monthday (+ (* month 100) day)))
      (if (< 100 monthday 631)
	  :spring
	  :fall))))
(defun springp (timestamp)
  (when (eql :spring (season timestamp))
    t))
(defun fallp (timestamp)
  (when (eql :fall (season timestamp))
    t))

;; United states = States + Territories + District of Columbia
;;   It would be nice to write a predicate that tests if a location is
;;   foreign or domestic.

(defgeneric check-ok (production-stage function desc)
  (:method ((ps production-stage) (fn function) desc)
    (ok (funcall fn ps)	desc))
  (:method ((ps-list list) (fn function) desc)
    (loop for ps in ps-list always (check-ok ps fn desc)))
  (:method ((ps production-stage) (fn-list list) desc)
    (loop for fn in fn-list always (check-ok ps fn desc)))
  (:method ((ps-list list) (fn-list list) desc)
    (loop for ps in ps-list always (check-ok ps fn-list desc)))
  (:method ((ps (eql :unbound)) fn desc)
    ;; If the production-stage is unbound, then we aren't using it.
    ;;   For instance we aren't bottling something, so therefore we
    ;;   do not discriminate based upon this attribute.
    t)
  ;; The null method isn't defined and that should error.  I think
  ;; this maybe useful to detect when things have gone wrong, but maybe
  ;; I'm fooling myself and really there should not be any unbound slots.
  )

(defgeneric check-proof (object fn proof &optional desc)
  (:documentation
   "Returns true if Production-stages' proof is checked against proof using function fn.")
  (:method ((ps production-stage) (fn function) (proof number) &optional desc)
    (check-ok ps
	      #'(lambda (p) (funcall fn
				     (convert-unit (proof p) 'proof)
				     proof))
	      desc))
  (:method ((ps production-stage) (fn function) (proof unit) &optional desc)
    (check-proof ps fn (convert-unit proof 'proof) desc))
  (:method ((stages list) (fn function) proof &optional desc)
    (loop for stage in stages
	 do (check-proof stage fn proof desc))))

;;   (check-percent (sources bourbon-whisky) #'> 51 'corn)
(defgeneric check-percent (object fn percent class &optional desc)
  (:documentation
   "Returns true if object contains a CLASS with a PERCENT amount determined by function FN.")
  (:method ((ps production-stage) (fn function) (percent number) (class symbol) &optional desc)
    (check-ok (cond ((listp (material ps))
		     (let ((amount-total 0)
			   (class-members ()))
		       (loop for obj in (material ps)
			  summing (amount obj) into amount-total
			  when (and (typep (material obj) class))
			  do (push (material obj) class-members))
		       (loop for member in class-members
			  when (funcall fn (/ (amount member) amount-total) percent)
			  return member)))
		    ((atom ps)
		     (when (and (funcall fn (amount (material ps)) percent)
				(typep (material ps) class))
		       ps))
		    (t nil))
	      (format nil "Checking ~A to see if it is made of ~A% ~A" ps percent class))))

(defgeneric check-types (production-stage type &optional desc)
  (:method (ps (type symbol) &optional desc)
    (is-type ps type) desc)
  (:method ((ps-list list) (type symbol) &optional desc)
    (loop for ps in ps-list always (check-types ps type desc)))
  (:method ((ps production-stage) (type-list list) &optional desc)
    (loop for type in type-list always (check-types ps type desc)))
  (:method ((ps-list list) (type-list list) &optional desc)
    (loop for ps in ps-list always (check-proof ps type-list desc)))
  (:method ((ps (eql :unbound)) fn &optional desc)
    t))

;;; Subpart C - Standards of Identity for Distilled Spirits
;; 5.21 - Application of standards.
;; 5.22 - The standards of identity.
;;   Class 1: neutral spirits
(defclass neutral-spirits (distilled-spirits)
  ()
  (:documentation
   "'Neutral Spirits' or 'alcohol' are distilled spirits produced from
any material at or above 190 degrees proof, and, if bottled, bottled
at not less than 80 degrees proof."))

(defgeneric verify (object)
  (:method-combination progn)
  (:method :around (object)
    (handler-bind ((unbound-slot #'(lambda (c)
				     (declare (ignore c))
				     (invoke-restart 'use-value :unbound))))
      (let ((*suite* (make-instance 'suite)))
	(call-next-method)
	(values (zerop (failed *suite*))
		(reports *suite*)))))
  (:documentation
   "A method used to verify that an object complies with cfr27."))

(defmethod verify progn ((ns neutral-spirits))
  (check-proof (distillations ns) #'>= 190
	    "Produced at or above 190 degrees proof. §5.11(a)")
  (check-proof (bottlings ns) #'>= 80
	       "Bottled at not less than 80 degrees proof. §5.11(a)"))

;;  Vodka
(defclass vodka (neutral-spirits)
  ()
  (:documentation
   "'Vodka' is neutral spirits so distilled, or so treated after
distillation with charcoal or other material, as to be without
distictive character, aroma, taste, or color.  § 5.22 (a1)"))

;; A verify method that confirms "no distinctive character"?  Seems a bit much.

;;  Grain Spirits
(defclass grain-spirits (neutral-spirits)
  ()
  (:default-initargs
   :sources (make-instance 'source :material (make-instance 'grain))
    :storages (make-instance 'storage
			     :container (make-instance 'oak-container)))
  (:documentation
   "'Grain spirits' are neutral spirits distilled from a fermented mash
of grain and stored in oak continers § 5.22 (a2)"))

(defmethod verify progn ((gs grain-spirits))
  (check-types (material (sources gs)) 'grain
	       "Distilled from a fermented mask of grain.  § 5.22 (a2)")
  (check-types (container (storages gs)) 'oak-container
	       "Stored in oak containers. § 5.22 (a2)"))

;;  Class 2: whisky
(defclass whisky (distilled-spirits)
  ()
  (:default-initargs
   :distillations (make-instance 'distillation
				 :proof (reduce-unit '(190 proof)))
    :bottlings (make-instance 'bottling
			      :proof (reduce-unit '(80 proof)))
    :sources (make-instance 'source :material (make-instance 'grain))
    :storages (make-instance 'storage
			     :container (make-instance 'oak-container)))
  (:documentation
   "'Whisky' is an alcoholic distillate from a fermented mash of grain produced
at less than 190° proof in such manner that the distillate possesses the taste,
aroma, and characteristics generally attributed to whisky, stored in oak 
containers (except that corn whisky need not be so stored), and bottled at not 
less than 80° proof, and also includes mixtures of such distillates for which 
no specific standards of identity are prescribed."))

(defmethod verify progn ((whisky whisky))
  (check-proof (distillations whisky) #'<= 190
	       "Produced at less than 190 deg proof. § 5.22 (b)")
  (check-proof (bottlings whisky) #'>= 80
	       "Bottled at not less than 80° proof. § 5.22 (b)")
  (check-types (material (sources whisky)) 'grain
	       "Distillate from a fermented mash of grain. § 5.22 (b)")
  (unless (typep whisky 'corn-whisky)
    (check-types (container (storages whisky)) 'oak-container
		 "Stored in oak containers. § 5.22 (b)")))

(defclass bourbon-whisky (whisky)
  ()
  (:default-initargs
    :sources (list
	      (make-instance 'source
			     :material (make-instance 'corn)
			     :amount 51)
	      (make-instance 'source
			     :material (make-instance 'grain)
			     :amount 49))
    :storages (make-instance 'charred-new-oak-container))
  (:documentation
   "‘Bourbon whisky’, ‘rye whisky’, ‘wheat whisky’, ‘malt whisky’, or ‘rye malt'whisky’ is whisky produced at not exceeding 160° proof from a fermented mash of
not less than 51 percent corn, rye, wheat, malted barley, or malted rye grain, 
respectively, and stored at not more than 125° proof in charred new oak 
containers; and also includes mixtures of such whiskies of the same type."))

(defmethod verify progn ((bourbon-whisky bourbon-whisky))
  (check-percent (sources bourbon-whisky) #'> 51 'corn)
  (check-types (storages bourbon-whisky) 'charred-new-oak-container
	       "Stored in new oak container."))

(defclass rye-whisky (whisky)
  ()
  (:default-initargs :production-proof-maximum (reduce-unit '(160 proof))
    :source-spec (make-instance 'rye :min-percent 51)
    :storage-types '((charred-oak-container :maximum '(125 proof)))
    ))

(defclass corn-whisky (whisky)
  ())
