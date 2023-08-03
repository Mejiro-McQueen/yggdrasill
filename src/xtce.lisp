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


(defmacro optional-xml-attribute (qname value)
  `(if , value
	   (cxml:attribute ,qname ,value) 
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
		(xtce-type-list (intern (format nil "~A-list" xtce-type))))
	(dolist (i items) 
      `(check-type i ,xtce-type))
	(make-instance xtce-type-list :base-type xtce-type :items items :xml-element-name xml-element-name)))

(defun make-xtce-set (xtce-type xml-element-name items)
  (let ((items (remove nil items))
		(xtce-type-set (intern (format nil "~A-set" xtce-type))))
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

(defmethod print-object ((obj xtce-list) stream)
      (print-unreadable-object (obj stream :type t)
        (with-slots (items) obj
          (format stream "items: ~a" items))))

(defclass container-set (xtce-set) () )

(defmethod make-container-set (&rest items)
  (dolist (i items)
	(check-type i sequence-container))
  (make-instance 'xtce-set :xml-element-name "ContainerSet" :items items :base-type 'sequence-container))

(defclass sequence-container () ((name :initarg :name :type symbol)
								 (short-description :initarg :short-description :type string)
								 (abstract :initarg :abstract :type bool)
								 (idle-pattern :initarg :idle-pattern)
								 (long-description :initarg :long-description :type long-description)
								 (alias-set :initarg :alias-set :type alias-set)
								 (ancilliary-data-set :initarg :ancilliary-data-set :type ancilliary-data-set)
								 (rate-in-stream-type :initarg :rate-in-stream-type)
								 (entry-list :initarg :entry-list :type entry-list)
								 (base-container :initarg :base-container :type base-container)))

(defun make-sequence-container (name
								entry-list
								&key
								  abstract
								  idle-pattern
								  short-description
								  long-description
								  alias-set
								  ancilliary-data-set
								  rate-in-stream-type
								  base-container)

  (make-instance 'sequence-container :name name
									 :entry-list entry-list
									 :short-description short-description
									 :abstract abstract
									 :idle-pattern idle-pattern
									 :long-description long-description
									 :alias-set alias-set
									 :ancilliary-data-set ancilliary-data-set
									 :rate-in-stream-type rate-in-stream-type
									 :base-container base-container
									 :short-description short-description))

(defmethod cxml-marshall ((obj sequence-container))
  (with-slots (name
			   entry-list
			   defmethod
			   abstract
			   idle-pattern
			   short-description
			   long-description
			   alias-set
			   ancilliary-data-set
			   rate-in-stream-type
			   base-container) obj
	(cxml:with-element* ("xtce" "SequenceContainer")
	  (cxml:attribute "name" (format-symbol name))
	  (optional-xml-attribute "shortDescription" short-description)
	  (optional-xml-attribute "abstract" (format-bool abstract))
	  (optional-xml-attribute "idlePattern" idle-pattern)
	  (cxml-marshall long-description)
	  (cxml-marshall alias-set)
	  (cxml-marshall ancilliary-data-set)
	  (cxml-marshall rate-in-stream-type)
	  (cxml-marshall entry-list)
	  (cxml-marshall base-container))))

(defclass default-rate-in-stream () ((basis :initarg :basis)
									 (minimum-value :initarg :minimum-value)
									 (maximum-value :initarg :maximum-value)))

(defun make-default-rate-in-stream (&key basis minimum-value maximum-value)
  (make-instance 'default-rate-in-stream :basis basis :minimum-value minimum-value :maximum-value maximum-value))

(defmethod cxml-marshall ((obj default-rate-in-stream))
  (with-slots (basis maximum-value minimum-value) obj
	(cxml:with-element* ("xtce" "DefaultRateInStream"))
	(optional-xml-attribute "basis" basis)
	(optional-xml-attribute "maximumValue" maximum-value)
	(optional-xml-attribute "minimumValue" minimum-value)))

(defclass entry-list (xtce-list) ())

(defun make-entry-list (&rest items)
  (make-xtce-list 'entry "EntryList" items))

(defclass parameter-ref-entry () ((parameter-ref :initarg :parameter-ref)
												 (short-description :initarg :short-description :type string)
												 (location-in-container-in-bits
												  :initarg :location-in-container-in-bits
												  :type location-in-container-in-bits)
												 (repeat-entry :initarg :repeat-entry :type repeat-entry)
												 (include-condition :initarg :include-condition :type include-condition)
												 (time-association :initarg :time-association :type time-association)
												 (ancilliary-data-set :initarg :ancillary-data-set :type ancilliary-data-set)))

(defun make-parameter-ref-entry (parameter-ref
								 &key
								   short-description
								   location-in-container-in-bits
								   repeat-entry
								   include-condition
								   time-association
								   ancilliary-data-set)
  (make-instance 'parameter-ref-entry :parameter-ref parameter-ref
									  :location-in-container-in-bits location-in-container-in-bits
									  :repeat-entry repeat-entry
									  :include-condition include-condition
									  :time-association time-association
									  :ancillary-data-set ancilliary-data-set
									  :short-description short-description))

(defmethod cxml-marshall ((obj parameter-ref-entry))
  (with-slots (parameter-ref
			   short-description
			   location-in-container-in-bits
			   repeat-entry
			   include-condition
			   time-association
			   ancilliary-data-set) obj
  (cxml:with-element* ("xtce" "ParameterRefEntry")
	(cxml:attribute "parameterRef" (format-symbol parameter-ref))
	(cxml-marshall location-in-container-in-bits)
	(cxml-marshall repeat-entry)
	(cxml-marshall include-condition)
	(cxml-marshall time-association)
	(cxml-marshall ancilliary-data-set))))

(defclass rate-in-stream () ((stream-ref :initarg :stream-ref)
							 (basis :initarg :basis)
							 (minimum-value :initarg :minimum-value)
							 (maximum-value :initarg :maximum-value)))

(defun make-rate-in-stream (stream-ref &key basis minimum-value maximum-value)
  (make-instance 'rate-in-stream :stream-ref stream-ref :basis basis :minimum-value minimum-value :maximum-value maximum-value))

(defmethod cxml-marshall ((obj rate-in-stream))
  (with-slots (stream-ref basis minimum-value maximum-value) obj
	(cxml:attribute "streamRef" stream-ref)
	(optional-xml-attribute "basis" basis)
	(optional-xml-attribute "minimumValue" minimum-value)
	(optional-xml-attribute "maximumValue" maximum-value)))

(defclass parameter-segment-ref-entry () ((parameter-ref :initarg :parameter-ref)
														 (size-in-bits :initarg :size-in-bits)
														 (short-description :initarg :short-description)
														 (order :initarg :order)
														 (location-in-container-in-bits
														  :initarg :location-in-container-in-bits
														  :type location-in-container-in-bits)
														 (repeat-entry :initarg :repeat-entry :type repeat-entry)
														 (include-condition :initarg :include-condition :type include-condition)
														 (time-association :initarg time-association :type time-association)
														 (ancilliary-data-set :initarg :ancillary-data-set :type ancilliary-data-set)))

(defun make-parameter-segment-ref-entry (parameter-ref
										 size-in-bits
										 &key
										   order
										   short-description
										   location-in-container-in-bits
										   repeat-entry
										   include-condition
										   time-association
										   ancilliary-data-set)
  (make-instance 'parameter-segment-ref-entry
				 :parameter-ref parameter-ref
				 :size-in-bits size-in-bits
				 :short-description short-description
				 :order order
				 :location-in-container-in-bits location-in-container-in-bits
				 :repeat-entry repeat-entry
				 :include-condition include-condition
				 :time-association time-association
				 :ancillary-data-set ancilliary-data-set))

(defmethod cxml-marshall ((obj parameter-segment-ref-entry))
  (with-slots (parameter-ref
			   size-in-bits
			   short-description
			   order
			   location-in-container-in-bits
			   repeat-entry
			   include-condition
			   time-association
			   ancilliary-data-set) obj
	(cxml:with-element* ("xtce" "ParameterSegmentRefEntry")
	  (cxml:attribute "parameterRef" (format-symbol parameter-ref))
	  (cxml:attribute "sizeInBits" size-in-bits)
	  (optional-xml-attribute "shortDescription" short-description)
	  (optional-xml-attribute order order)
	  (cxml-marshall location-in-container-in-bits)
	  (cxml-marshall repeat-entry)
	  (cxml-marshall include-condition)
	  (cxml-marshall time-association)
	  (cxml-marshall ancilliary-data-set))))

(defclass container-ref-entry () ((container-ref :initarg :container-ref)
												 (short-description :initarg short-description :type string)
												 (location-in-container-in-bits :initarg location-in-container-in-bits
																				:type location-in-container-in-bits)
												 (repeat-entry :initarg :repeat-entry :type repeat-entry)
												 (include-condition :initarg :include-condition :type include-condition)
												 (time-association :initarg :time-association :type time-association)
												 (ancilliary-data-set :initarg :ancillary-data-set :type ancilliary-data-set)))


(defun make-container-ref-entry (container-ref
								 &key
								   short-description
								   location-in-container-in-bits
								   repeat-entry
								   include-condition
								   time-association
								   ancilliary-data-set)
  (make-instance 'container-ref-entry
				 :container-ref container-ref
				 :short-description short-description
				 :location-in-container-in-bits location-in-container-in-bits
				 :repeat-entry repeat-entry
				 :include-condition include-condition
				 :time-association time-association
				 :ancillary-data-set ancilliary-data-set))

(defmethod cxml-marshall ((obj container-ref-entry))
    (with-slots (container-ref
				 size-in-bits
				 short-description
				 location-in-container-in-bits
				 repeat-entry
				 include-condition
				 time-association
				 ancilliary-data-set) obj
	(cxml:with-element* ("xtce" "ParameterSegmentRefEntry")
	  (cxml:attribute "containerRef" (format-symbol container-ref))
	  (cxml:attribute "shortDescription" short-description)
	  (cxml-marshall location-in-container-in-bits)
	  (cxml-marshall repeat-entry)
	  (cxml-marshall include-condition)
	  (cxml-marshall time-association)
	  (cxml-marshall ancilliary-data-set))))


(defclass container-segment-ref-entry () ((container-ref :initarg :container-ref)
														 (size-in-bits :initarg :size-in-bits :type positive-integer)
														 (short-description :initarg :short-description :type string)
														 (order :initarg :order)
														 (location-in-container-in-bits :initarg :location-in-container-in-bits
																						:type location-in-container-in-bits) 
														 (repeat-entry :initarg :repeat-entry :type repeat-entry)
														 (include-condition :initarg :include-condition :type include-condition)
														 (time-association :initarg :time-association :type time-association)
														 (ancilliary-data-set :initarg :ancillary-data-set :type ancilliary-data-set)))

(defun make-container-segment-ref-entry (container-ref
										 size-in-bits
										 &key
										   order
										   short-description
										   location-in-container-in-bits
										   repeat-entry
										   include-condition
										   time-association
										   ancilliary-data-set)
  (make-instance 'container-segment-ref-entry
				 :container-ref container-ref
				 :size-in-bits size-in-bits
				 :order order
				 :short-description short-description
				 :location-in-container-in-bits location-in-container-in-bits
				 :repeat-entry repeat-entry
				 :include-condition include-condition
				 :time-association time-association
				 :ancillary-data-set ancilliary-data-set))

(defmethod cxml-marshall ((obj container-segment-ref-entry))
  (with-slots (container-ref
			   size-in-bits
			   short-description
			   order
			   location-in-container-in-bits
			   repeat-entry
			   include-condition
			   time-association
			   ancilliary-data-set) obj
	(cxml:with-element* ("xtce" "ContainerSegmentRefEntry")
	  (cxml:attribute "containerRef" (format-symbol container-ref))
	  (cxml:attribute "sizeInBits" size-in-bits)
	  (optional-xml-attribute "shortDescription" short-description)
	  (optional-xml-attribute order order)
	  (cxml-marshall location-in-container-in-bits)
	  (cxml-marshall repeat-entry)
	  (cxml-marshall include-condition)
	  (cxml-marshall time-association)
	  (cxml-marshall ancilliary-data-set))))

(defclass stream-segment-entry () ((stream-ref :initarg :stream-ref)
												  (size-in-bits :initarg :size-in-bits :type positive-integer)
												  (short-description :initarg :short-description :type string)
												  (location-in-container-in-bits :initarg :location-in-container-in-bits
																				 :type location-in-container-in-bits)
												  (repeat-entry :initarg :repeat-entry :type repeat-entry)
												  (include-condition :initarg :include-condition :type include-condition)
												  (time-association :initarg :time-association :type time-association)
												  (ancilliary-data-set :initarg :ancillary-data-set :type ancilliary-data-set)))

(defun make-stream-segment-entry (stream-ref
								  size-in-bits
								  &key
									order
									short-description
									location-in-container-in-bits
									repeat-entry
									include-condition
									time-association
									ancilliary-data-set)
  (make-instance 'stream-segment-entry
				 :stream-ref stream-ref
				 :size-in-bits size-in-bits
				 :order order
				 :short-description short-description
				 :location-in-container-in-bits location-in-container-in-bits
				 :repeat-entry repeat-entry
				 :include-condition include-condition
				 :time-association time-association
				 :ancillary-data-set ancilliary-data-set))

(defmethod cxml-marshall ((obj stream-segment-entry))
  (with-slots (stream-ref
			   size-in-bits
			   short-description
			   order
			   location-in-container-in-bits
			   repeat-entry
			   include-condition
			   time-association
			   ancilliary-data-set) obj
	(cxml:with-element* ("xtce" "StreamSegmentRefEntry")
	  (cxml:attribute "streamRef" (format-symbol stream-ref))
	  (cxml:attribute "sizeInBits" size-in-bits)
	  (optional-xml-attribute "shortDescription" short-description)
	  (optional-xml-attribute order order)
	  (cxml-marshall location-in-container-in-bits)
	  (cxml-marshall repeat-entry)
	  (cxml-marshall include-condition)
	  (cxml-marshall time-association)
	  (cxml-marshall ancilliary-data-set))))

(defclass indirect-parameter-ref-entry () ((short-description :initarg :short-description :type string)
														  (alias-name-space :initarg :alias-name-space)
														  (location-in-container-in-bits :initarg :location-in-container-in-bits
																						 :type location-in-container-in-bits)
														  (repeat-entry :initarg :repeat-entry :type repeat-entry)
														  (include-condition :initarg :include-condition :type include-condition)
														  (time-association :initarg :time-association :type time-association)
														  (ancilliary-data-set :initarg :ancillary-data-set :type ancilliary-data-set)
														  (parameter-instance :initarg :parameter-ref :type parameter-instance)))

(defun make-indirect-parameter-ref-entry (parameter-instance
										  &key
										  alias-name-space
										  short-description
										  location-in-container-in-bits
										  repeat-entry
										  include-condition
										  time-association
										  ancilliary-data-set)
  (make-instance 'indirect-parameter-ref-entry
				 :parameter-instance parameter-instance
				 :alias-name-space alias-name-space
				 :short-description short-description
				 :location-in-container-in-bits location-in-container-in-bits
				 :repeat-entry repeat-entry
				 :include-condition include-condition
				 :time-association time-association
				 :ancillary-data-set ancilliary-data-set))

(defmethod cxml-marshall ((obj indirect-parameter-ref-entry))
  (with-slots (parameter-instance
			   alias-name-space
			   short-description
			   order
			   location-in-container-in-bits
			   repeat-entry
			   include-condition
			   time-association
			   ancilliary-data-set) obj
	(cxml:with-element* ("xtce" "IndirectParameterRefEntry")
	  (cxml-marshall (format-symbol parameter-instance))
	  (optional-xml-attribute "aliasNameSpace" alias-name-space)
	  (optional-xml-attribute "shortDescription" short-description)
	  (optional-xml-attribute order order)
	  (cxml-marshall location-in-container-in-bits)
	  (cxml-marshall repeat-entry)
	  (cxml-marshall include-condition)
	  (cxml-marshall time-association)
	  (cxml-marshall ancilliary-data-set))))

(defclass array-parameter-ref-entry () ((parameter-ref :initarg :parameter-ref)
													   (short-description :initarg :short-description :type string)
													   (location-in-container-in-bits :initarg :location-in-container-in-bits
																					  :type location-in-container-in-bits)
													   (repeat-entry :initarg :repeat-entry :type repeat-entry)
													   (include-condition :initarg :include-condition :type include-condition)
													   (time-association :initarg :time-association :type time-association)
													   (ancilliary-data-set :initarg :ancillary-data-set :type ancilliary-data-set)
													   (dimension-list :initarg :dimension-list :type dimension-list)))

(defun make-array-parameter-ref-entry (parameter-ref
									   dimension-list
									   &key
										 short-description
										 location-in-container-in-bits
										 repeat-entry
										 include-condition
										 time-association
										 ancilliary-data-set)
  (make-instance 'array-parameter-ref-entry
				 :parameter-ref parameter-ref
				 :dimension-list dimension-list
				 :short-description short-description
				 :location-in-container-in-bits location-in-container-in-bits
				 :repeat-entry repeat-entry
				 :include-condition include-condition
				 :time-association time-association
				 :ancillary-data-set ancilliary-data-set))

(defmethod cxml-marshall ((obj array-parameter-ref-entry))
  (with-slots (parameter-ref
			   dimension-list
			   short-description
			   location-in-container-in-bits
			   repeat-entry
			   include-condition
			   time-association
			   ancilliary-data-set) obj
	(cxml:with-element* ("xtce" "ArrayParameterRefEntry")
	  (cxml:attribute "parameterRef" (format-symbol parameter-ref))
	  (cxml-marshall dimension-list)
	  (optional-xml-attribute "shortDescription" short-description)
	  (cxml-marshall location-in-container-in-bits)
	  (cxml-marshall repeat-entry)
	  (cxml-marshall include-condition)
	  (cxml-marshall time-association)
	  (cxml-marshall ancilliary-data-set))))

(defclass dimension () ((starting-index :initarg :starting-index :type starting-index)
						(ending-index :initarg :ending-index :type ending-index)))

(defclass dimension-list (xtce-list) ())

(defun make-dimension-list (&rest items)
  (make-xtce-list 'dimension "DimensionList" items))

(defclass starting-index () ((value :initarg :value)))

(defun make-starting-index (value)
  (make-instance 'starting-index :value value))

(defmethod cxml-marshall ((obj starting-index))
  (with-slots (value) obj
	(cxml:with-element* ("xtce" "StartingIndex")
	(cxml-marshall value))))

(defclass ending-index () ((value :initarg :value)))

(defun make-ending-index (value)
  (make-instance 'ending-index :value value))

(defmethod cxml-marshall ((obj ending-index))
  (with-slots (value) obj
	(cxml:with-element* ("xtce" "EndingIndex")
	(cxml-marshall value))))

(defclass location-in-container-in-bits () ((reference-location :initarg :reference-location)
											(value :initarg :value :type value)))

(defun make-location-in-container-in-bits (value &key reference-location)
  (make-instance 'location-in-container-in-bits :value value :reference-location reference-location))

(defmethod cxml-marshall ((obj location-in-container-in-bits))
  	(with-slots (value reference-location) obj
	  (cxml:with-element* ("xtce" "LocationInContainerBits")
		(cxml:attribute "referenceLocation" reference-location)
		(cxml-marshall value))))

(defclass repeat-entry () ((count_slot :initarg :count_slot :type count_t)
						   (offset :initarg :offset :type offset)))

(defun make-repeat-entry (count &key offset)
  (make-instance 'repeat-entry :count_slot count :offset offset))

(defmethod cxml-marshall ((obj repeat-entry))
  (with-slots (count_slot offset) obj
	(cxml:with-element* ("xtce" "RepeatEntry")
	  (cxml-marshall count_slot)
	  (cxml-marshall offset))))

(defclass count_t () ((value :initarg :value :type integer-value)))

(defun make-count (value)
  (make-instance 'count_t :value value))

(defmethod cxml-marshall ((obj count_t))
  (with-slots (value) obj
	(cxml:with-element* ("xtce" "Count")
	  (cxml-marshall value))))


(defclass offset () ((value :initarg :value :type integer-type)))

(defun make-offset (value)
  (make-instance 'offset :value value))

(defmethod cxml-marshall ((obj offset))
  (with-slots (value) obj
	(cxml:with-element* ("xtce" "Offset")
	  (cxml-marshall value))))

(defclass include-condition () ((match-criteria :initarg :match-criteria :type match-criteria)))

(defun make-include-condition (match-criteria)
  (make-instance 'include-condition :match-criteria match-criteria))

(defmethod cxml-marshall ((obj include-condition))
  (with-slots (match-criteria) obj
	(cxml:with-element* ("xtce" "IncludeCondition")
	  (cxml-marshall match-criteria))))

(defclass base-container () ((container-ref :initarg :container-ref)
							 (restriction-criteria :initarg :restriction-criteria :type restriction-criteria)))

(defun make-base-container (container-ref &optional restriction-criteria)
  (make-instance 'base-container :container-ref container-ref :restriction-criteria restriction-criteria))

(defmethod cxml-marshal ((obj base-container))
  (with-slots (container-ref base-container) obj
	(cxml:with-element* ("xtce" "base-container")
	  (cxml-marshall base-container))))

(defclass restriction-criteria () ((restriction :initarg :restriction)
								   (next-container :initarg :next-container :type next-container)))

(defun make-restriction-criteria (restriction-criteria-type next-container)
  (make-instance 'restriction-criteria :restriction-criteria-type restriction-criteria-type :next-container next-container))

(defmethod cxml-marshal ((obj restriction-criteria))
  (with-slots (restriction-criteria next-container) obj
	(cxml:with-element* ("xtce" "RestrictionCriteria")
	  (cxml-marshall restriction-criteria next-container))))

(defclass next-container () ((container-ref :initarg :container-ref)))

(defun make-next-container (container-ref)
  (make-instance 'next-container :container-ref container-ref))

(defmethod cxml-marshal ((obj next-container))
  (with-slots (container-ref) obj
	(cxml:with-element* ("xtce" "NextContainer")
	  (cxml:attribute "nextContainer" container-ref))))


(defclass dynamic-value ()
  ((instance-ref :initarg :instance-ref)
   (linear-adjustment :initarg :linear-adjustment :type lnear-adjustment)))

(defun make-dynamic-value (instance-ref &key linear-adjustment)
  (make-instance 'dynamic-value :instance-ref instance-ref :linear-adjustment linear-adjustment))

(defmethod cxml-marshall ((obj dynamic-value))
  (with-slots (instance-ref linear-adjustment) obj
	(cxml:with-element* ("xtce" "DynamicValue")
	  (cxml-marshall instance-ref)
	  (cxml-marshall linear-adjustment))))

(defclass argument-instance-ref ()
  ((argument-ref :initarg :argument-ref)
   (use-calibrated-value :initarg :use-calibrated-value
                         :type boole)))

(defun make-argument-instance-ref (argument-ref use-calibrated-value)
  (make-instance 'argument-instance-ref :argument-ref argument-ref :use-calibrated-value use-calibrated-value))

(defmethod cxml-marshall ((obj argument-instance-ref))
  (with-slots (argument-ref use-calibrated-value) obj
	(cxml:with-element* ("xtce" "ArgumentInstanceRef") obj
	  (cxml:attribute "argumentRef" argument-ref)
	  (optional-xml-attribute "useCalibratedValue" use-calibrated-value))))

(defclass parameter-instance-ref ()
  ((parameter-reference :initarg :parameter-ref)
   (instance :initarg :instance)
   (use-calibrated-value :initarg :use-calibrated-value)))

(defun make-parameter-instance-ref (parameter-ref &key instance use-calibrated-value)
  (make-instance 'parameter-instance-ref :parameter-ref parameter-ref :instance instance :use-calibrated-value use-calibrated-value))

(defmethod cxml-marshall ((obj parameter-instance-ref))
  (with-slots (parameter-ref instance use-calibrated-value) obj
	(cxml:with-element* ("xtce" "ParameterInstanceRef") obj
	  (cxml:attribute "parameterRef" parameter-ref)
	  (optional-xml-attribute "instance" instance)
	  (optional-xml-attribute "useCalibratedValue" use-calibrated-value))))

(defclass discrete-lookup-list (xtce-list) ((comparison :initarg :comparison)))

(defun make-discrete-lookup-list (comparison &rest items)
  (let ((lookup-list (make-xtce-list 'discrete-lookup "DiscreteLookupList" items)))
	(setf (slot-value lookup-list 'comparison) comparison)
	lookup-list))

(defclass fixed-value ()
  ((value :initarg :value)))

(defun make-fixed-value (value)
  (make-instance 'fixed-value :value value))

(defmethod cxml-marshall ((obj fixed-value))
  (with-slots (value) obj
	(cxml:with-element* ("xtce" "FixedValue")
	  (cxml:text value))))

(defclass linear-adjustment ()
  ((slope :initarg slope :type float)
   (intercept :initarg intercept :type float)))

(defun make-linear-adjustment (&key slope intercept)
  (make-instance 'linear-adjustment :slope slope :intercept intercept))

(defmethod cxml-marshall ((obj linear-adjustment))
  (with-slots (slope intercept) obj
	(cxml:with-element* ("xtce" "LinearAdjustment")
	  (cxml:attribute "slope" slope)
	  (cxml:attribute "intercept" intercept))))

(defclass termination-char (size-in-bits)
  ((termination-char :initarg :termination-char)))

(defun make-termination-char (termination-char)
  (make-instance 'termination-char :termination-char termination-char))

(defmethod cxml-marshall ((obj termination-char))
  (with-slots (termination-char) obj
	(cxml:with-element* ("xtce" "TerminationChar")
	  (cxml:text termination-char))))

(defclass size-in-bits () ((size :initarg :size)))

(defun make-size-in-bits (size)
  (make-instance 'size-in-bits :size size))

(defmethod cxml-marshall ((obj size-in-bits))
  (with-slots (size) obj
	(cxml:with-element* ("xtce" "SizeInBits") obj
	  (cxml-marshall size))))

(defclass leading-size (size)
  ((size-in-bits-of-size-tag :initarg size-in-bits-of-size-tag
							 :type positive-integer)))

(defun make-leading-size (&optional (size-in-bits-of-size-tag 16))
  (make-instance 'leading-size :size-in-bits-of-size-tag size-in-bits-of-size-tag))

(defmethod cxml-marshall ((obj leading-size))
  (with-slots (size-in-bits-of-size-tag) obj
	  (cxml:with-element* ("xtce" "LeadingSize")
		(cxml:attribute "size-in-bits-of-size-tag" size-in-bits-of-size-tag))))

; Note: CCSDS 660.1-G-2 typo Page 4-142, figure caption says ContainrRefEntry
; Note 4.3.4.8.7 StreamSegmentRefEntry Figure 4-84 describes stream segment and not stream segment ref
;Figure 3-13: DiscreteLookup describes discretelookuplisttype
