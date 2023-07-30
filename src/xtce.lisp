(defpackage :xtce
  (:use :cl)
  (:documentation "XTCE")
  (:export :make-space-system
           :make-telemetry-metadata))

(in-package :xtce)

(defmacro check-optional-type (place type &optional type-string)
  `(if ,place
	   (check-type ,place ,type ,type-string)
	   nil))

(defclass space-system ()
  ((header :initarg :header)
   (name :initarg :name)
   (operational-status :initarg :operational-status)
   (long-description :initarg :long-description
                     :type long-description)
   (alias-set :initarg :alias-set)
   (ancialliary-data-set :initarg :ancilliary-data-set)
   (telemetry-metadata :initarg :telemetry-metadata)
   (command-metadata :initarg :command-metadata)
   (xml-base :initarg :xml-base)
   (service-set :initarg :service-set)
   (space-systems-list :initarg :space-systems-list :type space-system-list)
   (parent-system :initarg :parent-system)
   (symbol-table :initarg :symbol-table :initform (make-hash-table))
   (short-description :initarg :short-description)))

(defmethod print-object ((obj space-system) stream)
      (print-unreadable-object (obj stream :type t)
        (with-slots (name short-description) obj
          (format stream "name: ~a, description: ~a" name short-description))))

(defclass space-system-list (xtce-list) ())

(defun make-space-systems-list (&rest items)
  (make-xtce-list 'space-system "SpaceSystem" items))

(defun make-space-system (name
                          &key
							parent-system
							header
                            operational-status
                            long-description
                            alias-set
                            ancilliary-data-set
                            telemetry-metadata
                            command-metadata
                            xml-base
                            service-set
                            space-systems-list
							short-description)
  (check-type name symbol)
  (check-optional-type long-description long-description)
  
  (let ((sys (make-instance 'space-system
				 :name name
				 :parent-system parent-system
				 :header header
				 :operational-status operational-status
				 :long-description long-description
				 :alias-set alias-set
				 :ancilliary-data-set ancilliary-data-set
				 :telemetry-metadata telemetry-metadata
				 :command-metadata command-metadata
				 :xml-base xml-base
				 :service-set service-set
				 :space-systems-list space-systems-list
				 :short-description short-description)))
	(register-system-keys sys)
	(if (not parent-system)
		  (eval `(defvar ,name ,sys "Represents the root space system."))
		sys)))

(defclass long-description () ((long-description :initarg :long-description
                                                 :type string)))

(trace register-system-keys)

(defun register-unique-key (symbol-table symbol value)
  ;(describe symbol-table)
  (assert (not (gethash symbol symbol-table)))
  (setf (gethash symbol symbol-table) value))

(defun register-system-keys (space-system)
  (With-slots (symbol-table space-systems-list telemetry-metadata) space-system 
	(if space-systems-list
		(dolist (subsystem (slot-value space-systems-list 'items))
		  (register-unique-key symbol-table (slot-value subsystem 'name) subsystem)))
	(if telemetry-metadata
		(dolist (parameter-type (slot-value (slot-value telemetry-metadata 'parameter-type-set) 'items))
		  (print parameter-type)))))

(defun make-long-description (s)
  (check-type s string)
  (make-instance 'long-description :long-description s))

(defmethod cxml-marshall ((obj long-description))
  (with-slots (long-description) obj
    (cxml:with-element* ("xtce" "LongDescription"))
    (cxml:text long-description)))


(defgeneric cxml-marshall (obj))

(defmethod cxml-marshall ((obj space-system))
  (with-slots (name
               short-description
               header
               operational-status
               xml-base
               long-description
               alias-set
               telemetry-metadata) obj
    (cxml:with-element* ("xtce" "SpaceSystem")
      (cxml:attribute*  "xsi" "schemaLocation" "http://www.omg.org/spec/XTCE/20180204 XTCE12.xsd")
      (cxml:attribute "name" (format-symbol name))
      (optional-xml-attribute "header" header)
      (optional-xml-attribute "operational-status" operational-status)
      (optional-xml-attribute "xml:base" xml-base)
      (cxml-marshall long-description)
      (cxml-marshall telemetry-metadata))))

(defclass telemetry-metadata ()
  ((parameter-type-set :initarg :parameter-type-set
                       :type parameter-type-set)
   (parameter-set :initarg :parameter-set
                  :type parameter-set)
   (container-set :initarg :container-set
                  :type container-set)
   (message-set :initarg :message-set
                :type message-set)
   (stream-set :initarg :stream-set
               :type stream-set)
   (algorithm-set :initarg :algorithm-set
                  :type algorithm-set)))

(defun make-telemetry-metadata (&key parameter-type-set
                                     parameter-set
                                     container-set
                                     message-set
                                     stream-set
                                     algorithm-set)
  (if parameter-type-set (check-type parameter-type-set parameter-type-set))
  (make-instance 'telemetry-metadata
                 :parameter-type-set parameter-type-set
                 :parameter-set parameter-set
                 :container-set container-set
                 :message-set message-set
                 :stream-set stream-set
                 :algorithm-set algorithm-set))

(defmethod cxml-marshall ((obj telemetry-metadata))
  (with-slots (parameter-type-set parameter-set container-set message-set stream-set algorithm-set) obj
    (cxml-marshall parameter-type-set)
    (cxml-marshall parameter-set)
    (cxml-marshall container-set)
    (cxml-marshall message-set)
    (cxml-marshall stream-set)
    (cxml-marshall algorithm-set)))

(defclass xtce-set ()
  ((base-type :initarg :base-type)
   (items :initarg :items
          :type list)
   (xml-element-name :initarg :xml-element-name
					 :type string)))
  
(defclass xtce-list ()
  ((base-type :initarg :base-type)
   (items :initarg :items
          :type list)
   (xml-element-name :initarg :xml-element-name
					 :type string)))

(defclass xtce-table ()
  ((items :initarg :items)
   (xml-element-name :initarg :xml-element-name
					 :type string)))

(defclass unit-set (xtce-set) ())

(defun make-xtce-list (xtce-type xml-element-name items)
  (let ((items (remove nil items))
		(xtce-type-list (intern (format nil "~A-LIST" xtce-type))))
	(dolist (i items) 
      `(check-type i ,xtce-type))
	(make-instance xtce-type-list :base-type xtce-type :items items :xml-element-name xml-element-name)))

(defun make-xtce-set (xtce-type xml-element-name items)
  (let ((items (remove nil items))
		(xtce-type-set (intern (format nil "~A-SET" xtce-type))))
	(dolist (i items) 
      `(check-type i ,xtce-type))
	(make-instance xtce-type-set :base-type xtce-type :items items :xml-element-name xml-element-name)))

(describe (make-instance 'xtce-set ))

(defclass unit () ((power :initarg :power
                          :type number)
                   (factor :initarg :factor)
                   (description :initarg :description
                                :type string)
                   (form :initarg :form)))

(defun make-unit (&key power factor description form)
  (make-instance 'unit :power power
                       :factor factor
                       :description description
                       :form form))

(defun make-unit-set (&rest items)
  (make-xtce-set 'unit "UnitSet" items))

(defmethod cxml-marshall ((obj unit))
  (with-slots (power factor description form) obj
    (cxml:with-element* ("xtce" "unit") 
      (if power (cxml:attribute "power" power))
      (if factor (cxml:attribute "factor" factor))
      (if description (cxml:attribute "description" description))
      (if form (cxml:text form)))))

(defun dump-space-system-xml (space-system)
  (cxml:with-xml-output (cxml:make-string-sink :indentation 4 :canonical nil)
    (cxml:comment "Bifrost Integral")
    (cxml:with-namespace ("xtce" "http://www.omg.org/spec/XTCE/20180204")
      (cxml:with-namespace ("xsi" "http://www.w3.org/2001/XMLSchema-instance")
        (cxml-marshall space-system)))))

(defun format-bool (a)
  (if a "True" "False"))

(defun format-symbol (a)
  (if a
	  (format nil "~a" a)))

(defmacro optional-xml-attribute (qname value)
  `(if , value
	   (cxml:attribute ,qname ,value) 
	   nil))

(defun xml-dump (element)
  (cxml:with-xml-output (cxml:make-string-sink :indentation 4 :canonical nil)
	(cxml-marshall element)))

(defmethod cxml-marshall ((obj NULL)))

(defmethod cxml-marshall ((obj xtce-list))
  (with-slots (items xml-element-name) obj
	(if xml-element-name
		(cxml:with-element* ("xtce" xml-element-name)
		  (dolist (i items)
			(cxml-marshall i)))
		(dolist (i items)
		  	(cxml-marshall i)))))

(defmethod cxml-marshall ((obj xtce-set))
  (with-slots (items xml-element-name) obj
	(if xml-element-name
		(cxml:with-element* ("xtce" xml-element-name)
		  (dolist (i items)
			(cxml-marshall i)))
		(dolist (i items)
		  	(cxml-marshall i)))))


(defclass parameter-set (xtce-set) ())

(defun make-parameter-set (&rest items)
  (make-xtce-set 'parameter "ParameterSet" items))

(defclass parameter ()
   ((short-description :initarg :short-description
					  :type string)

   (name :initarg :name
		 :type symbol)
   (parameter-type-ref :initarg :parameter-type-ref :type symbol)
   (initial-value :initarg :initial-value)
   (long-description :initarg :long-description
					 :type string)
   (alias-set :initarg :alias-set
			  :type alias-set)
   (ancilliary-data-set :initarg :ancilliary-data-set
						:type ancilliary-data-set)
   (parameter-properties :initarg :parameter-type-properties
						 :type paramater-properties)))


(defmethod print-object ((obj xtce-list) stream)
      (print-unreadable-object (obj stream :type t)
        (with-slots (items) obj
          (format stream "items: ~a" items))))
