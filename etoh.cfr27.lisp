;;;; etoh.cfr27.lisp

(in-package #:etoh.cfr27)

(defparameter +ttb-website-url+ "http://www.ttb.gov")
(defparameter *cfr27-version-timestamp*
  (encode-timestamp 0 0 0 0 13 3 2018)
  "A timestamp representing the latest version this software relates to.")

(defvar *site-configuration-lazy-load-tables* t
  "This variable holds a boolean value that is referenced when creating new site-configuration objects.")

(defvar *site-configuration-volume-type* :wine
  "The default type of value to return for volumes, typeically :WINE,
but also can be :PROOF, ie. wine-gallons versus proof-gallons.")

(defvar *site-configuration-volume-temp-correct-conversion* :to-60f
  "The default method to use when temperature correcting volumes.
Values can be :TO-60F and :FROM-60F.")

(defclass site-configuration ()
  ((lazy-load-tables :initarg :lazy-load-tables
		     :accessor lazy-load-tables-p
		     :initform *site-configuration-lazy-load-tables*)
   (message-stream :initarg :message-stream
		   :accessor message-stream
		   :initform *standard-output*)
   (volume-type :initarg :volume-type
		:accessor volume-type
		:initform *site-configuration-volume-type*)
   (temp-correct-conv :initarg :temp-correct-conv
		      :accessor temp-correct-conv
		      :initform *site-configuration-volume-temp-correct-conversion*)))

(defvar *default-site-configuration*
  (make-instance 'site-configuration)
  "This variable holds the default-site-configuration.")

(defun default-volume-type (&optional (config *default-site-configuration*))
  "A helper function to return the site defaults for volume type."
  (volume-type config))

(defun default-temp-correct-conv (&optional (config *default-site-configuration*))
  "A helper function to return the site default of temp correction conversion."
  (temp-correct-conv config))

(defun current-directory ()
  "Gives the location of the source files.  This is useful when
lazy loading large tables."
  (asdf/system::system-source-directory :etoh.cfr27))

(defun message (format-string &rest format-args)
  (let ((stream (message-stream *default-site-configuration*)))
    (when stream
      (apply #'format
	     stream
	     format-string
	     format-args))))

(defparameter *ecfr-27-cfr-update-xml-url*
  "https://www.gpo.gov/fdsys/bulkdata/ECFR/title-27/ECFR-title27.xml")
(defparameter *ecfr-27-cfr* nil
  "An unofficial representation of 27 cfr parsed from xml.")

(defun fetch-ecfr-and-parse (&optional force)
  (if (and *ecfr-27-cfr*
	   (not force))
      *ecfr-27-cfr*
      (setf *ecfr-27-cfr*
	    (parse (http-request *ecfr-27-cfr-update-xml-url*)))))

(defun parse-month-string (month-string)
  (flet ((month= (month) (string= month-string month)))
    (cond ((month= "Jan") 1)
	  ((month= "Feb") 2)
	  ((month= "Mar") 3)
	  ((month= "Apr") 4)
	  ((month= "May") 5)
	  ((month= "Jun") 6)
	  ((month= "Jul") 7)
	  ((month= "Aug") 8)
	  ((month= "Sep") 9)
	  ((month= "Oct") 10)
	  ((month= "Nov") 11)
	  ((month= "Dec") 12))))

(defun parse-publish-date (date-string)
  (cl-ppcre:register-groups-bind (month day year)
      ("([A-z]+)\.? +([0-9]+), +([0-9]+)" date-string)
    (encode-timestamp 0
		      0
		      0
		      0
		      (parse-integer day)
		      (parse-month-string month)
		      (parse-integer year))))

(defun ecfr-publish-date (&optional force)
  (parse-publish-date
   (third (third (third (third (fourth (fetch-ecfr-and-parse force))))))))
