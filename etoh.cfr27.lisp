;;;; etoh.cfr27.lisp

(in-package #:etoh.cfr27)

(defparameter +ttb-website-url+ "http://www.ttb.gov")
(defparameter *cfr27-version-timestamp*
  (encode-timestamp 0 0 0 0 27 5 2017)
  "A timestamp representing the latest version this software relates to.")

(defvar *site-configuration-lazy-load-tables* t
  "This variable holds a boolean value that is referenced when creating new site-configuration objects.")

(defclass site-configuration ()
  ((lazy-load-tables :initarg :lazy-load-tables
		     :accessor lazy-load-tables-p
		     :initform *site-configuration-lazy-load-tables*)
   (message-stream :initarg :message-stream
		   :accessor message-stream
		   :initform *standard-output*)))

(defvar *default-site-configuration*
  (make-instance 'site-configuration)
  "This variable holds the default-site-configuration.")

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
