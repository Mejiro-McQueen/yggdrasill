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
   (ancialliary-data-set :initarg :ancillary-data-set)
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
        (with-slots (name short-description abstract) obj
          (format stream "name: ~a, description: ~a, abstract:~a " name short-description abstract))))

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
                            ancillary-data-set
                            telemetry-metadata
                            command-metadata
                            xml-base
                            service-set
                            space-systems-list
							short-description)
  (check-type name string)
  (check-optional-type long-description long-description)
  
  (let ((name-as-sym  (intern (string-upcase name)))
		(sys (make-instance 'space-system
				 :name name
				 :parent-system parent-system
				 :header header
				 :operational-status operational-status
				 :long-description long-description
				 :alias-set alias-set
				 :ancillary-data-set ancillary-data-set
				 :telemetry-metadata telemetry-metadata
				 :command-metadata command-metadata
				 :xml-base xml-base
				 :service-set service-set
				 :space-systems-list space-systems-list
				 :short-description short-description)))
	(register-system-keys sys)
	(if (not parent-system)
		(eval `(defparameter ,name-as-sym ,sys "Represents the root space system."))
		sys)))

(defclass long-description () ((long-description :initarg :long-description
                                                 :type string)))

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
      (cxml:attribute "name" name)
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
          :type list
		  :accessor items)
   (xml-element-name :initarg :xml-element-name
					 :type string)))
  
(defclass xtce-list ()
  ((base-type :initarg :base-type)
   (items :initarg :items
          :type list
		  :accessor items)
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

(defun dump-xml (element)
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

(defclass sequence-container () ((name :initarg :name :type string)
								 (short-description :initarg :short-description :type string)
								 (abstract :initarg :abstract :type bool)
								 (idle-pattern :initarg :idle-pattern)
								 (long-description :initarg :long-description :type long-description)
								 (alias-set :initarg :alias-set :type alias-set)
								 (ancillary-data-set :initarg :ancillary-data-set :type ancillary-data-set)
								 (default-rate-in-stream :initarg :default-rate-in-stream :type default-rate-in-stream)
								 (rate-in-stream-set :initarg :rate-in-stream-set :type rate-in-stream-set)
								 (binary-encoding :initarg :binary-encoding :type binary-encoding)
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
								  ancillary-data-set
								  rate-in-stream-set
								  default-rate-in-stream
								  binary-encoding
								  base-container)

  (make-instance 'sequence-container :name name
									 :entry-list entry-list
									 :short-description short-description
									 :abstract abstract
									 :idle-pattern idle-pattern
									 :long-description long-description
									 :alias-set alias-set
									 :ancillary-data-set ancillary-data-set
									 :rate-in-stream-set rate-in-stream-set
									 :default-rate-in-stream default-rate-in-stream
									 :binary-encoding binary-encoding
									 :base-container base-container))

(defmethod cxml-marshall ((obj sequence-container))
  (with-slots (name
			   entry-list
			   defmethod
			   abstract
			   idle-pattern
			   short-description
			   long-description
			   alias-set
			   ancillary-data-set
			   rate-in-stream-set
			   default-rate-in-stream
			   binary-encoding
			   base-container) obj
	(cxml:with-element* ("xtce" "SequenceContainer")
	  (cxml:attribute "name" name)
	  (optional-xml-attribute "shortDescription" short-description)
	  (optional-xml-attribute "abstract" (format-bool abstract))
	  (optional-xml-attribute "idlePattern" idle-pattern)
	  (cxml-marshall long-description)
	  (cxml-marshall alias-set)
	  (cxml-marshall ancillary-data-set)
	  (cxml-marshall rate-in-stream-set)
	  (cxml-marshall default-rate-in-stream)
	  (cxml-marshall binary-encoding)
	  (cxml-marshall entry-list)
	  (cxml-marshall base-container))))

;TODO: I think I would rather just have a conditional reader macro to choose that suggested non-XTCE format 
(defclass resolved-sequence-container (sequence-container)
  ((restriction-criteria-set :initarg :restriction-criteria-set :type restriction-criteria-set)))

(defun make-resolved-sequence-container (name
										 entry-list
										 &key
										   abstract
										   idle-pattern
										   short-description
										   long-description
										   alias-set
										   ancillary-data-set
										   rate-in-stream-set
										   default-rate-in-stream
										   binary-encoding
										   restriction-criteria-set)
  "This is suggested but not defined in XTCE. For use when resolving containers. It's a regular sequence  base-container. The sequence-container's base-container criteria is collected during resolution. Use when dumping a non XTCE"

  (make-instance 'resolved-sequence-container :name name
											  :entry-list entry-list
											  :short-description short-description
											  :abstract abstract
											  :idle-pattern idle-pattern
											  :long-description long-description
											  :alias-set alias-set
											  :ancillary-data-set ancillary-data-set
											  :rate-in-stream-set rate-in-stream-set
											  :default-rate-in-stream default-rate-in-stream
											  :binary-encoding binary-encoding
											  :base-container base-container
											  :restriction-criteria restriction-criteria-set))

(defmethod cxml-marshall ((obj resolved-sequence-container))
  (with-slots (name
			   entry-list
			   defmethod
			   abstract
			   idle-pattern
			   short-description
			   long-description
			   alias-set
			   ancillary-data-set
			   rate-in-stream-set
			   default-rate-in-stream
			   binary-encoding
			   restriction-criteria-set) obj
	(cxml:with-element* ("xtce" "SequenceContainer")
	  (cxml:attribute "name" name)
	  (optional-xml-attribute "shortDescription" short-description)
	  (optional-xml-attribute "abstract" (format-bool abstract))
	  (optional-xml-attribute "idlePattern" idle-pattern)
	  (cxml-marshall long-description)
	  (cxml-marshall alias-set)
	  (cxml-marshall ancillary-data-set)
	  (cxml-marshall rate-in-stream-set)
	  (cxml-marshall default-rate-in-stream)
	  (cxml-marshall binary-encoding)
	  (cxml-marshall entry-list)
	  (cxml-marshall restriction-criteria-set))))

(defmethod print-object ((obj sequence-container) stream)
      (print-unreadable-object (obj stream :type t)
        (with-slots (name short-description abstract base-container) obj
          (format stream "name: ~a, description: ~a, abstract:~a, base-container?:~a" name short-description abstract base-container))))

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
												 (ancillary-data-set :initarg :ancillary-data-set :type ancillary-data-set)))

(defun make-parameter-ref-entry (parameter-ref
								 &key
								   short-description
								   location-in-container-in-bits
								   repeat-entry
								   include-condition
								   time-association
								   ancillary-data-set)
  (make-instance 'parameter-ref-entry :parameter-ref parameter-ref
									  :location-in-container-in-bits location-in-container-in-bits
									  :repeat-entry repeat-entry
									  :include-condition include-condition
									  :time-association time-association
									  :ancillary-data-set ancillary-data-set
									  :short-description short-description))

(defmethod cxml-marshall ((obj parameter-ref-entry))
  (with-slots (parameter-ref
			   short-description
			   location-in-container-in-bits
			   repeat-entry
			   include-condition
			   time-association
			   ancillary-data-set) obj
  (cxml:with-element* ("xtce" "ParameterRefEntry")
	(cxml:attribute "parameterRef" parameter-ref)
	(cxml-marshall location-in-container-in-bits)
	(cxml-marshall repeat-entry)
	(cxml-marshall include-condition)
	(cxml-marshall time-association)
	(cxml-marshall ancillary-data-set))))

;;;

(defclass resolved-parameter-ref-entry () ((parameter-ref :initarg :parameter-ref)
										   (short-description :initarg :short-description :type string)
												 (location-in-container-in-bits
												  :initarg :location-in-container-in-bits
												  :type location-in-container-in-bits)
												 (repeat-entry :initarg :repeat-entry :type repeat-entry)
												 (include-condition :initarg :include-condition :type include-condition)
												 (time-association :initarg :time-association :type time-association)
												 (ancillary-data-set :initarg :ancillary-data-set :type ancillary-data-set)))

(defun make-parameter-ref-entry (parameter-ref
								 &key
								   short-description
								   location-in-container-in-bits
								   repeat-entry
								   include-condition
								   time-association
								   ancillary-data-set)
  (make-instance 'parameter-ref-entry :parameter-ref parameter-ref
									  :location-in-container-in-bits location-in-container-in-bits
									  :repeat-entry repeat-entry
									  :include-condition include-condition
									  :time-association time-association
									  :ancillary-data-set ancillary-data-set
									  :short-description short-description))

(defmethod cxml-marshall ((obj parameter-ref-entry))
  (with-slots (parameter-ref
			   short-description
			   location-in-container-in-bits
			   repeat-entry
			   include-condition
			   time-association
			   ancillary-data-set) obj
  (cxml:with-element* ("xtce" "ParameterRefEntry")
	(cxml:attribute "parameterRef" parameter-ref)
	(cxml-marshall location-in-container-in-bits)
	(cxml-marshall repeat-entry)
	(cxml-marshall include-condition)
	(cxml-marshall time-association)
	(cxml-marshall ancillary-data-set))))


(defclass rate-in-stream () ((stream-ref :initarg :stream-ref
										 :accessor stream-ref)
							 (basis :initarg :basis)
							 (minimum-value :initarg :minimum-value)
							 (maximum-value :initarg :maximum-value)))

(defun make-rate-in-stream (stream-ref &key basis minimum-value maximum-value)
  (make-instance 'rate-in-stream :stream-ref stream-ref :basis basis :minimum-value minimum-value :maximum-value maximum-value))

(defclass rate-in-stream-set (xtce-set) ())

(defun make-rate-in-stream-set (&rest items)
  (make-xtce-set 'rate-in-stream "RateInStreamSet" items))

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
														 (ancillary-data-set :initarg :ancillary-data-set :type ancillary-data-set)))

(defun make-parameter-segment-ref-entry (parameter-ref
										 size-in-bits
										 &key
										   order
										   short-description
										   location-in-container-in-bits
										   repeat-entry
										   include-condition
										   time-association
										   ancillary-data-set)
  (make-instance 'parameter-segment-ref-entry
				 :parameter-ref parameter-ref
				 :size-in-bits size-in-bits
				 :short-description short-description
				 :order order
				 :location-in-container-in-bits location-in-container-in-bits
				 :repeat-entry repeat-entry
				 :include-condition include-condition
				 :time-association time-association
				 :ancillary-data-set ancillary-data-set))

(defmethod cxml-marshall ((obj parameter-segment-ref-entry))
  (with-slots (parameter-ref
			   size-in-bits
			   short-description
			   order
			   location-in-container-in-bits
			   repeat-entry
			   include-condition
			   time-association
			   ancillary-data-set) obj
	(cxml:with-element* ("xtce" "ParameterSegmentRefEntry")
	  (cxml:attribute "parameterRef" parameter-ref)
	  (cxml:attribute "sizeInBits" size-in-bits)
	  (optional-xml-attribute "shortDescription" short-description)
	  (optional-xml-attribute order order)
	  (cxml-marshall location-in-container-in-bits)
	  (cxml-marshall repeat-entry)
	  (cxml-marshall include-condition)
	  (cxml-marshall time-association)
	  (cxml-marshall ancillary-data-set))))

(defclass container-ref-entry () ((container-ref :initarg :container-ref)
												 (short-description :initarg short-description :type string)
												 (location-in-container-in-bits :initarg location-in-container-in-bits
																				:type location-in-container-in-bits)
												 (repeat-entry :initarg :repeat-entry :type repeat-entry)
												 (include-condition :initarg :include-condition :type include-condition)
												 (time-association :initarg :time-association :type time-association)
												 (ancillary-data-set :initarg :ancillary-data-set :type ancillary-data-set)))


(defun make-container-ref-entry (container-ref
								 &key
								   short-description
								   location-in-container-in-bits
								   repeat-entry
								   include-condition
								   time-association
								   ancillary-data-set)
  (make-instance 'container-ref-entry
				 :container-ref container-ref
				 :short-description short-description
				 :location-in-container-in-bits location-in-container-in-bits
				 :repeat-entry repeat-entry
				 :include-condition include-condition
				 :time-association time-association
				 :ancillary-data-set ancillary-data-set))

(defmethod cxml-marshall ((obj container-ref-entry))
    (with-slots (container-ref
				 size-in-bits
				 short-description
				 location-in-container-in-bits
				 repeat-entry
				 include-condition
				 time-association
				 ancillary-data-set) obj
	(cxml:with-element* ("xtce" "ParameterSegmentRefEntry")
	  (cxml:attribute "containerRef"  container-ref)
	  (cxml:attribute "shortDescription" short-description)
	  (cxml-marshall location-in-container-in-bits)
	  (cxml-marshall repeat-entry)
	  (cxml-marshall include-condition)
	  (cxml-marshall time-association)
	  (cxml-marshall ancillary-data-set))))


(defclass container-segment-ref-entry () ((container-ref :initarg :container-ref)
														 (size-in-bits :initarg :size-in-bits :type positive-integer)
														 (short-description :initarg :short-description :type string)
														 (order :initarg :order)
														 (location-in-container-in-bits :initarg :location-in-container-in-bits
																						:type location-in-container-in-bits) 
														 (repeat-entry :initarg :repeat-entry :type repeat-entry)
														 (include-condition :initarg :include-condition :type include-condition)
														 (time-association :initarg :time-association :type time-association)
														 (ancillary-data-set :initarg :ancillary-data-set :type ancillary-data-set)))

(defun make-container-segment-ref-entry (container-ref
										 size-in-bits
										 &key
										   order
										   short-description
										   location-in-container-in-bits
										   repeat-entry
										   include-condition
										   time-association
										   ancillary-data-set)
  (make-instance 'container-segment-ref-entry
				 :container-ref container-ref
				 :size-in-bits size-in-bits
				 :order order
				 :short-description short-description
				 :location-in-container-in-bits location-in-container-in-bits
				 :repeat-entry repeat-entry
				 :include-condition include-condition
				 :time-association time-association
				 :ancillary-data-set ancillary-data-set))

(defmethod cxml-marshall ((obj container-segment-ref-entry))
  (with-slots (container-ref
			   size-in-bits
			   short-description
			   order
			   location-in-container-in-bits
			   repeat-entry
			   include-condition
			   time-association
			   ancillary-data-set) obj
	(cxml:with-element* ("xtce" "ContainerSegmentRefEntry")
	  (cxml:attribute "containerRef" container-ref)
	  (cxml:attribute "sizeInBits" size-in-bits)
	  (optional-xml-attribute "shortDescription" short-description)
	  (optional-xml-attribute order order)
	  (cxml-marshall location-in-container-in-bits)
	  (cxml-marshall repeat-entry)
	  (cxml-marshall include-condition)
	  (cxml-marshall time-association)
	  (cxml-marshall ancillary-data-set))))

(defclass stream-segment-entry () ((stream-ref :initarg :stream-ref)
												  (size-in-bits :initarg :size-in-bits :type positive-integer)
												  (short-description :initarg :short-description :type string)
												  (location-in-container-in-bits :initarg :location-in-container-in-bits
																				 :type location-in-container-in-bits)
												  (repeat-entry :initarg :repeat-entry :type repeat-entry)
												  (include-condition :initarg :include-condition :type include-condition)
												  (time-association :initarg :time-association :type time-association)
												  (ancillary-data-set :initarg :ancillary-data-set :type ancillary-data-set)))

(defun make-stream-segment-entry (stream-ref
								  size-in-bits
								  &key
									order
									short-description
									location-in-container-in-bits
									repeat-entry
									include-condition
									time-association
									ancillary-data-set)
  (make-instance 'stream-segment-entry
				 :stream-ref stream-ref
				 :size-in-bits size-in-bits
				 :order order
				 :short-description short-description
				 :location-in-container-in-bits location-in-container-in-bits
				 :repeat-entry repeat-entry
				 :include-condition include-condition
				 :time-association time-association
				 :ancillary-data-set ancillary-data-set))

(defmethod cxml-marshall ((obj stream-segment-entry))
  (with-slots (stream-ref
			   size-in-bits
			   short-description
			   order
			   location-in-container-in-bits
			   repeat-entry
			   include-condition
			   time-association
			   ancillary-data-set) obj
	(cxml:with-element* ("xtce" "StreamSegmentRefEntry")
	  (cxml:attribute "streamRef" stream-ref)
	  (cxml:attribute "sizeInBits" size-in-bits)
	  (optional-xml-attribute "shortDescription" short-description)
	  (optional-xml-attribute order order)
	  (cxml-marshall location-in-container-in-bits)
	  (cxml-marshall repeat-entry)
	  (cxml-marshall include-condition)
	  (cxml-marshall time-association)
	  (cxml-marshall ancillary-data-set))))

(defclass indirect-parameter-ref-entry () ((short-description :initarg :short-description :type string)
														  (alias-name-space :initarg :alias-name-space)
														  (location-in-container-in-bits :initarg :location-in-container-in-bits
																						 :type location-in-container-in-bits)
														  (repeat-entry :initarg :repeat-entry :type repeat-entry)
														  (include-condition :initarg :include-condition :type include-condition)
														  (time-association :initarg :time-association :type time-association)
														  (ancillary-data-set :initarg :ancillary-data-set :type ancillary-data-set)
														  (parameter-instance :initarg :parameter-ref :type parameter-instance)))

(defun make-indirect-parameter-ref-entry (parameter-instance
										  &key
										  alias-name-space
										  short-description
										  location-in-container-in-bits
										  repeat-entry
										  include-condition
										  time-association
										  ancillary-data-set)
  (make-instance 'indirect-parameter-ref-entry
				 :parameter-instance parameter-instance
				 :alias-name-space alias-name-space
				 :short-description short-description
				 :location-in-container-in-bits location-in-container-in-bits
				 :repeat-entry repeat-entry
				 :include-condition include-condition
				 :time-association time-association
				 :ancillary-data-set ancillary-data-set))

(defmethod cxml-marshall ((obj indirect-parameter-ref-entry))
  (with-slots (parameter-instance
			   alias-name-space
			   short-description
			   order
			   location-in-container-in-bits
			   repeat-entry
			   include-condition
			   time-association
			   ancillary-data-set) obj
	(cxml:with-element* ("xtce" "IndirectParameterRefEntry")
	  (cxml-marshall parameter-instance)
	  (optional-xml-attribute "aliasNameSpace" alias-name-space)
	  (optional-xml-attribute "shortDescription" short-description)
	  (optional-xml-attribute order order)
	  (cxml-marshall location-in-container-in-bits)
	  (cxml-marshall repeat-entry)
	  (cxml-marshall include-condition)
	  (cxml-marshall time-association)
	  (cxml-marshall ancillary-data-set))))

(defclass array-parameter-ref-entry () ((parameter-ref :initarg :parameter-ref)
													   (short-description :initarg :short-description :type string)
													   (location-in-container-in-bits :initarg :location-in-container-in-bits
																					  :type location-in-container-in-bits)
													   (repeat-entry :initarg :repeat-entry :type repeat-entry)
													   (include-condition :initarg :include-condition :type include-condition)
													   (time-association :initarg :time-association :type time-association)
													   (ancillary-data-set :initarg :ancillary-data-set :type ancillary-data-set)
													   (dimension-list :initarg :dimension-list :type dimension-list)))

(defun make-array-parameter-ref-entry (parameter-ref
									   dimension-list
									   &key
										 short-description
										 location-in-container-in-bits
										 repeat-entry
										 include-condition
										 time-association
										 ancillary-data-set)
  (make-instance 'array-parameter-ref-entry
				 :parameter-ref parameter-ref
				 :dimension-list dimension-list
				 :short-description short-description
				 :location-in-container-in-bits location-in-container-in-bits
				 :repeat-entry repeat-entry
				 :include-condition include-condition
				 :time-association time-association
				 :ancillary-data-set ancillary-data-set))

(defmethod cxml-marshall ((obj array-parameter-ref-entry))
  (with-slots (parameter-ref
			   dimension-list
			   short-description
			   location-in-container-in-bits
			   repeat-entry
			   include-condition
			   time-association
			   ancillary-data-set) obj
	(cxml:with-element* ("xtce" "ArrayParameterRefEntry")
	  (cxml:attribute "parameterRef" parameter-ref)
	  (cxml-marshall dimension-list)
	  (optional-xml-attribute "shortDescription" short-description)
	  (cxml-marshall location-in-container-in-bits)
	  (cxml-marshall repeat-entry)
	  (cxml-marshall include-condition)
	  (cxml-marshall time-association)
	  (cxml-marshall ancillary-data-set))))

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

(defun make-base-container (container-ref &key restriction-criteria)
  (make-instance 'base-container :container-ref container-ref :restriction-criteria restriction-criteria))

(defmethod cxml-marshal ((obj base-container))
  (with-slots (container-ref base-container) obj
	(cxml:with-element* ("xtce" "base-container")
	  (cxml-marshall base-container))))

(defclass restriction-criteria () ((restriction-criteria-type :initarg :restriction-criteria-type)
								   (next-container :initarg :next-container :type next-container)))

(defun make-restriction-criteria (restriction-criteria-type &key next-container)
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

(defclass parameter-type () ())

(defclass parameter-type-set (xtce-set) ())
  
(defmethod cxml-marshall ((obj xtce-set))
  (with-slots (items) obj
    (dolist (i items)
      (cxml-marshall i))))

(defun make-parameter-type-set (&rest items)
  (let ((items (remove nil items))) 
    (dolist (i items) 
      (check-type i parameter-type)))
  (make-xtce-set 'parameter-type "ParameterTypeSet" items))

(defclass string-parameter-type (parameter-type) ())

(defclass float-parameter-type (parameter-type)
  ((name :initarg :name
         :type string)
   (short-description :initarg :short-description
                      :type string)
   (base-type :initarg :base-type)
   (initial-value :initarg :initial-value)
   (size-in-bits :initarg :size-in-bits)
   (long-description :initarg :long-description
                     :type long-description)
   (alias-set :initarg :alias-set
              :type alias-set)
   (ancillary-data-set :initarg :ancillary-data-set
                       :type ancillary-data-set)
   (data-encoding :initarg :data-encoding
                  :type data-encoding)
   (to-string :initarg :to-string)
   (valid-range :initarg :valid-range)
   (default-alarm :initarg :default-alarm)
   (unit-set :initarg :unit-set :type unit-set)
   (context-alarm-list :initarg :context-alarm-list :type context-alarm-list)))

(defun make-float-parameter-type (name
								  &key
									short-description
                                    base-type
                                    initial-value
                                    size-in-bits
                                    long-description
                                    alias-set
                                    ancillary-data-set
                                    data-encoding
                                    to-string
                                    valid-range
                                    default-alarm
                                    context-alarm-list
									unit-set)
  (check-type name string)
  ;(require-unique-key name)
                                        ;(if encoding (check-type encoding encoding))
  (check-optional-type short-description string)
  ;(check-optional-type base-type )
  (check-optional-type initial-value float)
  (check-optional-type size-in-bits positive-integer)
  (check-optional-type long-description string)
  (check-optional-type alias-set alias-set)
  (check-optional-type ancillary-data-set ancillary-data-set)
  (check-optional-type data-encoding data-encoding)
  ;(check-optional-type to-string T)
  (check-optional-type valid-range valid-range)
  (check-optional-type default-alarm alarm)
  (check-optional-type context-alarm-list context-alarm-list)
  (make-instance 'float-parameter-type :name name
									   :short-description short-description
									   :base-type base-type
									   :initial-value initial-value
									   :size-in-bits size-in-bits
									   :long-description long-description
									   :alias-set alias-set
									   :ancillary-data-set ancillary-data-set
									   :data-encoding data-encoding
									   :to-string to-string
									   :valid-range valid-range
									   :default-alarm default-alarm
									   :context-alarm-list context-alarm-list
									   :unit-set unit-set))

(defmethod cxml-marshall ((obj float-parameter-type))
  (with-slots (name
			   short-description
			   initial-value
			   size-in-bits
			   long-description
			   alias-set
			   ancillary-data-set
			   data-encoding
			   to-string
			   valid-range
			   default-alarm
			   context-alarm-list
			   unit-set
			   ) obj
	(cxml:with-element* ("xtce" "FloatPrameterType")
      (cxml:attribute "name" name)
      (optional-xml-attribute "shortDescription" short-description)
	  (optional-xml-attribute "initialValue" initial-value)
	  (optional-xml-attribute "sizeInBits" size-in-bits)
	  (cxml-marshall long-description)
	  (cxml-marshall alias-set)
	  (cxml-marshall ancillary-data-set)
	  (cxml-marshall data-encoding)
	  (cxml-marshall to-string)
	  (cxml-marshall valid-range)
	  (cxml-marshall default-alarm)
	  (cxml-marshall context-alarm-list)
	  (cxml-marshall unit-set))))

(defclass integer-parameter-type (parameter-type)
  ((short-description :initarg :short-description
                      :type string)
   (name :initarg :name
         :type string)
   (base-type :initarg :base-type)
   (initial-value :initarg :initial-value
                  :type integer)
   (size-in-bits :initarg :size-in-bits
                 :type positive-integer)
   (signed :initarg :signed
           :type boole)
   (long-description :initarg :long-description
                     :type string)
   (alias-set :initarg :alias-set
              :type alias-set)
   (ancillary-data-set :initarg :ancillary-data-set
                        :type ancillary-data-set)
   (unit-set :initarg :unit-set
             :type unit-set)
   (data-encoding :initarg :data-encoding
                  :type encoding)
   (to-string :initarg :to-string)
   (valid-range :initarg :valid-range)
   (default-alarm :initarg :default-alarm
                  :type alarm)
   (context-alarm-list :initarg :context-alarm-list
                       :type context-alarm-list)))

(defmethod cxml-marshall ((obj integer-parameter-type))
  (with-slots (short-description
               name
               base-type
               initial-value
               size-in-bits
               signed
               long-description alias-set
               ancillary-data-set
               unit-set
               data-encoding
               to-string
               valid-range
               default-alarm
               context-alarm-list) obj
    (cxml:with-element* ("xtce" "IntegerParameterType")
	  (optional-xml-attribute "shortDescription" short-description)
      (optional-xml-attribute "name" name)
      (optional-xml-attribute "baseType" base-type)
      (optional-xml-attribute "initialValue" initial-value)
      (optional-xml-attribute"sizeInBits" size-in-bits)
      (cxml:attribute "signed" (format-bool signed))
      (cxml-marshall long-description)
      (cxml-marshall alias-set)
	  (cxml-marshall unit-set)
      (cxml-marshall data-encoding)
      (cxml-marshall to-string)
      (cxml-marshall valid-range)
      (cxml-marshall default-alarm)
      (cxml-marshall context-alarm-list))))

(defun make-integer-parameter-type (name
                                    &key
                                    short-description                                    
                                    base-type
                                    initial-value
                                    size-in-bits
                                    (signed 'NOTHING)
                                    long-description
                                    alias-set
                                    ancillary-data-set
                                    (unit-set (make-unit-set))
                                    data-encoding
                                    to-string
                                    valid-range
                                    default-alarm
                                    context-alarm-list)
  (check-type name string)
  (check-optional-type short-description string)
  (if base-type nil)
  (if initial-value nil)
  (check-optional-type size-in-bits positive-integer)
  (if (eq signed 'NOTHING)
	  (progn
		(setf signed nil)
		(check-type signed boolean)))
  (check-optional-type long-description long-description)
  (check-optional-type alias-set alias-set)
  (check-optional-type ancillary-data-set ancillary-data-set)
  (check-optional-type unit-set unit-set)
  (check-optional-type data-encoding data-encoding)
  (check-optional-type valid-range valid-range)
  (check-optional-type default-alarm alarm)
  (check-optional-type context-alarm-list context-alarm-list)
  (make-instance 'integer-parameter-type :name name
                                         :short-description short-description
                                         :base-type base-type
                                         :initial-value initial-value
                                         :size-in-bits size-in-bits
                                         :signed signed
                                         :long-description long-description
                                         :to-string to-string
                                         :alias-set alias-set
                                         :ancillary-data-set ancillary-data-set
                                         :unit-set unit-set
                                         :data-encoding data-encoding
                                         :valid-range valid-range
                                         :default-alarm default-alarm
                                         :context-alarm-list context-alarm-list))

(defclass valid-range () ())

(defclass alias-set () ())

(defclass alarm () ())

(defclass numeric-alarm (alarm) ())

(defclass alarm-conditions () ())

(defclass static-alarm-ranges () ())

(defclass change-alarm-ranges () ())

(defclass alarm-mutliranges () ())

(defclass custom-alarm () ())

(defclass alarm-multi-ranges () ())

(defclass context-alarm-list () ())

(deftype positive-integer ()
  "A type for positive integers."
  `(and integer (satisfies plusp)))

(defclass enumerated-parameter-type (parameter-type)
  (
   (short-description :initarg :short-description :type string)
   (name :initarg :name :type string)
   (initial-value :initarg :initial-value)
   (base-type :initarg :base-type)
   (data-encoding :initarg :data-encoding
				  :type data-encoding)
   (enumeration-list :initarg :enumeration-list
					 :type enumeration-list)
   (default-alarm :initarg :default-alarm
				  :type enumeration-alarm-type)
   (context-alarm-list :initarg :context-alarm-list
					   :type context-alarm-list)

   (long-description :initarg :long-description
					 :type long-description)
   (alias-set :initarg :alias-set :type alias-set)
   (ancillary-data-set :initarg :ancillary-data-set :type ancillary-data-set)
   (unit-set :initarg :unit-set :type unit-set)))

(defun make-enumerated-parameter-type (name
									   &key
										 short-description
										 base-type
										 initial-value
										 long-description
										 alias-set
										 ancillary-data-set
										 unit-set
										 data-encoding
										 enumeration-list
										 default-alarm
										 context-alarm-list)
  (check-type name string)
  (check-optional-type short-description string)
  ;(check-optional-type base-type T)
  (check-optional-type long-description string)
  (check-optional-type alias-set alias-set)
  (check-optional-type ancillary-data-set ancillary-data-set)
  (check-optional-type unit-set unit-set)
  (check-optional-type data-encoding data-encoding)
  (check-optional-type enumeration-list enumeration-list)
  (check-optional-type default-alarm enumeration-alarm)
  (check-optional-type context-alarm-list context-alarm-list)
  ;(check-optional-type initial-value T)
  ; Need to check if inital value is in enumeration list
  (make-instance 'enumerated-parameter-type :name name
											:short-description short-description
											:base-type base-type
											:initial-value initial-value
											:long-description long-description
											:alias-set alias-set
											:ancillary-data-set ancillary-data-set
											:unit-set unit-set
											:data-encoding data-encoding
											:enumeration-list enumeration-list
											:default-alarm default-alarm
											:context-alarm-list context-alarm-list))


(defclass enumeration-alarm (alarm)
  ((alarm-level :initarg :alarm-level :type string)
   (enumeration-label :initarg :enumeration-label :type symbol)))

(defun make-enumeration-alarm (alarm-level enumeration-label)
  (let ((allowed-alarm-levels '(normal watch warning distress critical severe)))
  (check-type enumeration-label symbol)
  (check-type alarm-level symbol)
  (assert (member alarm-level allowed-alarm-levels) (alarm-level) "Alarm level ~A is not one of ~A" alarm-level allowed-alarm-levels) 
  (make-instance 'enumeration-alarm :alarm-level alarm-level  :enumeration-label enumeration-label)))

(defclass enumeration-list (xtce-list) ())

(defun make-enumeration-list (&rest items)
  (make-xtce-list 'enumeration "EnumerationList" items))

(defclass enumeration ()
  ((value :initarg :value)
  (max-value :initarg :max-value)
  (label :initarg :label
		 :type symbol)
  (short-description :initarg :short-description
					 :type string)))

(defun make-enumeration (label value  &key max-value short-description)
  (check-type value number)
  (check-type label symbol)
  (if max-value (check-type max-value number))
  (if short-description (check-type short-description string))
  (make-instance 'enumeration :value value :label label :max-value max-value :short-description short-description))

(defmethod cxml-marshall ((obj enumeration))
  (with-slots (value label max-value short-description) obj
	(cxml:with-element* ("xtce" "Enumeration")
	  (cxml:attribute "label" (format nil "~A" label))
	  (cxml:attribute "value" (format nil "~A" value))
	  (if max-value (cxml:attribute "maxValue" max-value))
	  (if short-description (cxml:attribute "shortDescription" short-description)))))

(defmethod cxml-marshall ((obj enumerated-parameter-type))
  (with-slots (name
			   short-description
			   base-type
			   initial-value
			   long-description
			   alias-set
			   ancillary-data-set
			   unit-set
			   data-encoding
			   enumeration-list
			   default-alarm
			   context-alarm-list) obj
	(cxml:with-element* ("xtce" "EnumeratedParameterType")
	  (cxml:attribute "name" name)
	  (cxml-marshall data-encoding)
	  (optional-xml-attribute "shortDescription" short-description)
	  (optional-xml-attribute "baseType" base-type)
	  (optional-xml-attribute "initialValue" initial-value)
	  (cxml-marshall long-description)
	  (cxml-marshall alias-set)
	  (cxml-marshall unit-set)
	  (cxml-marshall enumeration-list)
	  (cxml-marshall default-alarm)
	  (cxml-marshall context-alarm-list))))

(defclass absolute-time-parameter (parameter-type)
  ((short-description :initarg :short-description :type string)
   (name :initarg :name :type string)
   (base-type :initarg :base-type)
   (initial-value :initarg :initial-value)
   (long-description :initarg :long-description :type long-description)
   (alias-set :initarg :alias-set :type alias-set)
   (ancillary-data-set :initarg :ancillary-data-set :type ancillary-data-set)
   (encoding :initarg :encoding :type encoding)
   (reference-time :initarg :reference-time :type reference-time)))

(defun make-absolute-time-parameter (name
									 &key
									 short-description
									 base-type
									 initial-value
									 long-description
									 alias-set
									 ancillary-data-set
									 encoding
									 reference-time)
  (check-type name string)
  (check-optional-type short-description string)
  (check-optional-type base-type string)
  ;(check-optional-type initial-value T)
  (check-optional-type long-description string)
  (check-optional-type alias-set alias-set)
  (check-optional-type ancillary-data-set ancillary-data-set)
  (check-optional-type encoding encoding)
  (check-optional-type reference-time reference-time)
  (make-instance 'absolute-time-parameter
				 :name name
				 :short-description short-description
				 :initial-value initial-value
				 :long-description long-description
				 :alias-set alias-set
				 :ancillary-data-set ancillary-data-set
				 :encoding encoding
				 :reference-time reference-time
				 :base-type base-type))

(defmethod cxml-marshall ((obj absolute-time-parameter))
  (with-slots (short-description
			   name
			   base-type
			   initial-value
			   long-description
			   alias-set
			   ancillary-data-set
			   encoding
			   reference-time) obj
	(cxml:with-element* ("xtce" "AbsoluteTimeParameterType")
	  (cxml:attribute "name" name)
	  (optional-xml-attribute "shortDescription" short-description)
	  (optional-xml-attribute "baseType" base-type)
	  (optional-xml-attribute "initialValue" initial-value)
	  (cxml-marshall long-description)
	  (cxml-marshall alias-set)
	  (cxml-marshall ancillary-data-set)
	  (cxml-marshall encoding)
	  (cxml-marshall reference-time))))

(defclass offset-from () ((parameter-ref :initarg :parameter-ref)))

(defun make-offset-from (parameter-ref)
  (make-instance 'offset-from :parameter-ref parameter-ref))

(defmethod cxml-marshall ((obj offset-from))
  (with-slots (parameter-ref) obj
	(cxml:with-element* ("xtce" "OffsetFrom")
	  (cxml:attribute "parameterRef" parameter-ref))))

(defclass epoch ()
  ((epoch-value :initarg :epoch-value
				:type symbol)))

(defun make-epoch (epoch-value)
  ; TODO: figure out xs and dateTime types
  (let ((allowed-epoch-enumerations '(TAI J2000 UNIX GPS)))
  (check-type epoch-value symbol)
  (assert (member epoch-value allowed-epoch-enumerations) (epoch-value)
		  "epoch string enumeration value ~A is not in ~A" epoch-value allowed-epoch-enumerations))
  (make-instance 'epoch :epoch-value epoch-value))

(defmethod cxml-marshall ((obj epoch))
  (with-slots (epoch-value) obj
	(cxml:with-element* ("xtce" "Epoch")
	  (cxml:text (format-symbol epoch-value)))))

(defclass reference-time () ((reference :initarg :reference )))

(defmethod cxml-marshall ((obj reference-time))
  (with-slots (reference) obj
	(cxml:with-element* ("xtce" "ReferenceTime")
	  (cxml-marshall reference))))

(defun make-reference-time (reference)
  (assert (or (typep reference 'epoch)
			  (typep reference 'offset-from))
		  (reference) "Type of ~A is not one of (EPOCH OFFSET-FROM)" reference )
  (make-instance 'reference-time :reference reference))

(defclass encoding ()
  ((units :initarg :units :type string)
   (scale :initarg :scale :type number)
   (offset :initarg :offest :type number)
   (data-encoding :initarg :data-encoding :type data-encoding)
   (reference-time :initarg :reference-time :type reference-time)))

(defun make-encoding (&key units scale offest data-encoding reference-time)
  (check-optional-type units string)
  (check-optional-type scale number)
  (check-optional-type offest number)
  (check-optional-type data-encoding data-encoding)
  (check-optional-type reference-time reference-time)
  (make-instance 'encoding :units units :scale scale :offest offest :data-encoding data-encoding :reference-time reference-time))

(defmethod cxml-marshall ((obj encoding))
  (with-slots (units scale offset data-encoding reference-time) obj
	(cxml:with-element* ("xtce" "Encoding") 
	  (optional-xml-attribute "units" units)
	  (optional-xml-attribute "scale" (format-number scale))
	  (optional-xml-attribute "offset" offset)
	  (cxml-marshall data-encoding))))


;TODO: Encoding parameters may not accept all data encodings 

(defun format-number (n)
  (format nil "~A" n))


(defclass parameter ()
  ((name :initarg :name :type string)
   (parameter-type-ref :initarg :parameter-type-ref :type string)
   (short-description :initarg :short-description :type string)
   (initial-value :initarg :initial-value)
   (long-description :initarg :long-description :type long-description)
   (alias-set :initarg :alias-set :type alias-set)
   (ancillary-data-set :initarg :ancillary-data-set :type ancillary-data-set)
   (parameter-properties :initarg :parameter-properties :type parameter-properties)))


(defun make-parameter (name
					   parameter-type-ref
					   &key
						 short-description
						 initial-value
						 long-description
						 alias-set
						 ancillary-data-set
						 parameter-properties)
  (check-type name string)
  (check-type parameter-type-ref string)
  (make-instance
   'parameter
   :name name
   :parameter-type-ref parameter-type-ref
   :short-description short-description
   :long-description long-description
   :initial-value initial-value
   :alias-set alias-set
   :ancillary-data-set ancillary-data-set
   :parameter-properties parameter-properties))

(defmethod cxml-marshall ((obj parameter))
  (with-slots (name
			   parameter-type-ref
			   short-description
			   initial-value
			   long-description alias-set
			   ancillary-data-set
			   parameter-properties)
	  obj
	(cxml:with-element* ("xtce" "Parameter") 
	  (cxml:attribute "name"  name)
	  (cxml:attribute "parameterTypeRef" parameter-type-ref)
	  (optional-xml-attribute "shortDescription" short-description)
	  (optional-xml-attribute "initialValue" initial-value)
	  (cxml-marshall long-description)
	  (cxml-marshall alias-set)
	  (cxml-marshall ancillary-data-set)
	  (cxml-marshall parameter-properties))))


(defclass parameter-set (xtce-set) ())

(defun make-parameter-set (&rest items)
  (make-xtce-set 'parameter nil items))

(defclass parameter-properties () ((data-source :initarg :data-source)
								   (read-only :initarg :read-only)
								   (persistence :initarg :persistence)
								   (system-name :initarg :system-name :type system-name)
								   (validity-condition :initarg :validity-condition :type validity-condition)
								   (physical-address-set :initarg :physical-address-set :type physical-address-set)
								   (time-association :initarg :time-association :type time-association)))

(defun make-parameter-properties (&key data-source (read-only :null) persistence system-name validity-condition physical-address-set time-association)
  (make-instance 'parameter-properties
				 :read-only read-only
				 :data-source data-source
				 :persistence persistence
				 :system-name system-name
				 :validity-condition validity-condition
				 :physical-address-set physical-address-set
				 :time-association time-association))

(defmethod cxml-marshall ((obj parameter-properties))
  (with-slots (data-source read-only persistence system-name validity-condition physical-address-set time-association) obj
	(cxml:with-element* ("xtce" "ParameterProperties")
	  (optional-xml-attribute "dataSource" data-source)
	  (if (not (equal read-only :null))
		(optional-xml-attribute "readOnly" (format-bool read-only)))
	  (optional-xml-attribute "persistence" persistence)
	  (cxml-marshall system-name)
	  (cxml-marshall validity-condition)
	  (cxml-marshall physical-address-set)
	  (cxml-marshall time-association))))


(defclass system-name () ())

(defclass validity-condition ()())

(defclass physical-address-set () ())

(defclass time-association () ())


(defclass data-encoding () ())

(defun valid-integer-encoding-p (enc)
  (assert (member enc '(unsigned sign-magnitude twos-complement ones-compliment bcd packed-bcd)) (enc)
          "Int Encoding ~A is not <Unsigned | twos-complement | ones-compliment | bcd | packed-bcd" enc) 
  t)

(defclass integer-data-encoding (data-encoding)
  ((encoding :documentation "Specifies integer numeric value to raw encoding method."
             :initarg :encoding)
   (size-in-bits :documentation "Number of bits to use for the raw encoding."
                 :initarg :size-in-bits :type integer)
   (change-threshold :documentation "Error detection algorithm"
                     :initarg :change-threshold)
   (default-calibrator :documentation "TODO"
                       :initarg :default-calibrator)
   (context-calibrator-list :documentation "TODO"
                            :initarg :context-calibrator-list)))

(defun make-integer-data-encoding (&key (size-in-bits 8)
                                        (encoding 'UNSIGNED)
                                        (change-threshold nil)
                                        (default-calibrator nil)
                                        (context-calibrator-list nil))
  (valid-integer-encoding-p encoding)
  (check-type size-in-bits integer)
  (assert (plusp size-in-bits) (size-in-bits) "size-in-bits ~A must be a positive integer" size-in-bits)
  (make-instance 'integer-data-encoding
                 :size-in-bits size-in-bits
                 :encoding encoding
                 :change-threshold change-threshold
                 :context-calibrator-list context-calibrator-list
                 :default-calibrator default-calibrator))


(defmethod cxml-marshall ((obj integer-data-encoding))
  (with-slots (size-in-bits encoding change-threshold default-calibrator context-calibrator-list) obj
    (cxml:with-element* ("xtce" "IntegerDataEncodingType")
      (optional-xml-attribute "encoding" (format-symbol encoding))
      (optional-xml-attribute "sizeInBits" size-in-bits)
      (optional-xml-attribute "changeThreshold" change-threshold)
      (cxml-marshall default-calibrator)
      (cxml-marshall context-calibrator-list))))

(defclass string-data-encoding (data-encoding)
  ((size-in-bits :initarg :size-in-bits
                 :type size-in-bits)
   (bit-order :initarg :bit-order)
   (error-detect-correct :initarg :error-detect-correct
                         :type error-detect-correct)
   (byte-order-list :initarg :byte-order-list
                    :type byte-order-list)
   (encoding :initarg :encoding)))

;; (defun make-string-data-encoding (size-in-bits &optional (bit-order)
;;                                                          (encoding)
;;                                                          (error-detect-correct)
;;                                                          (byte-order-list)))

(defun valid-string-encoding-p (enc)
  (let ((valid-encodings '(US-ASCII WINDOWS-1252 ISO-UTF-8 UTF-16 UTF-16LE UTF-16BE UTF-32 UTF-32LE UTF-32BE) )) 
    (assert (member enc valid-encodings) (enc) "String encoding ~A is not one of: ~A" enc valid-encodings)) t)

(defclass binary-data-encoding ()
  ((bit-order :initarg :bit-order)
   (byte-order :initarg :byte-order)
   (size-in-bits :initarg :size-in-bits :type size-in-bits)
   (error-detect-correct :initarg :error-detect-correct)
   (from-binary-transform-algorithm :initarg :from-binary-transform-algorithm)
   (to-binary-transform-algorithm :initarg :from-binary-transform-algorithm)))

(defun make-binary-data-encoding (size-in-bits &optional (bit-order)
                                                         (byte-order)
                                                         (error-detect-correct)
                                                         (from-binary-transform-algorithm)
                                                         (to-binary-transform-algorithm))
  (check-type size-in-bits size-in-bits)
  (make-instance 'binary-data-encoding
                 :size-in-bits size-in-bits
                 :bit-order bit-order
                 :byte-order byte-order
                 :error-detect-correct error-detect-correct
                 :from-binary-transform-algorithm from-binary-transform-algorithm
                 :to-binary-transform-algorithm to-binary-transform-algorithm))

(defclass float-data-encoding (data-encoding)
  ((bit-order :documentation "Bit-Order"
              :initarg :bit-order)
   (byte-order :documentation "Byte-order"
               :initarg :byte-order)
   (size-bits :documentation "Size in bits"
              :initarg :size-in-bits)
   (encoding :documentation "<BIG | LITTLE>"
             :initarg :encoding)
   (change-threshold :documentation "change-threshold"
                     :initarg :change-threshold)
   (error-detection :documentation "Error detection algorithm"
                    :initarg :error-detection)
   (default-calibrator :documentation "Default calibrator"
                       :initarg :default-calibrator)
   (context-calibrator-list :initarg :context-calibrator-list)))

(defun valid-float-size-bits (size)
  (if (member size '(32 64 128 'non-32))
      t))

(defun make-float-data-encoding (size-in-bits &optional (bit-order 'MSB)
                                                        (byte-order 'BIG)
                                                        (encoding 'IEE)
                                                        (change-threshold nil)
                                                        (error-detection)
                                                        (default-calibrator)
                                                        (context-calibrator-list))
  (valid-bit-order-p bit-order)
  (or (context-calibrator-list-p context-calibrator-list) (null context-calibrator-list))
  (check-type size-in-bits integer)
  (assert (> size-in-bits 0) (size-in-bits) "size-in-bits ~A must be a positive integer" size-in-bits)
  (make-instance 'float-data-encoding
                 :bit-order bit-order
                 :byte-order byte-order
                 :size-in-bits size-in-bits
                 :encoding encoding
                 :error-detection error-detection
                 :default-calibrator default-calibrator
                 :change-threshold change-threshold
                 :context-calibrator-list context-calibrator-list))

(defun valid-bit-order-p (bit-order)
  
  (assert (member bit-order '(MSB LSB)) (bit-order) "Bit Order ~A is not <LSB | MSB>" bit-order) t)

(defun valid-float-encoding-p (enc)
  (assert (member enc '(IEE MIL)) (enc) "Encoding ~A is not <IEE | MIL>" enc) t)


(defclass numeric-calibrator ()
  ((name :documentation "Optional name"
         :initarg :name)
   (short-description :documentation "Optional description"
                      :initarg :short-description)
   (ancillary-data-set :documentation "Optional additional information"
                       :initarg :ancillary-data-set)))

(defclass context-calibrator ()
  ((context-match)
   (calibrator)))

(defclass context-match () ())

(defclass comparison (context-match)
  ((parameter-ref :initarg :parameter-ref)
   (instance :initarg :instance)
   (use-calibrated-value :initarg :use-calibrated-value)
   (comparison-operator :initarg :comparison-operator)
   (value :initarg :value)))

(defun make-comparison (parameter-ref value &optional (instance 0) (use-calibrated-value t) (comparison-operator 'equal))
  (make-instance 'comparison
                 :parameter-ref parameter-ref
                 :instance instance
                 :use-calibrated-value use-calibrated-value
                 :value value
				 :comparison-operator comparison-operator))

(defmethod cxml-marshall ((obj comparison))
  (with-slots (parameter-ref value instance use-calibrated-value comparison-operator) obj
	(cxml:with-element* ("xtce" "Comparison")
	  (cxml:attribute "parameterRef" parameter-ref)
	  (cxml:attribute "value" value)
	  (optional-xml-attribute "instance" instance)
	  (optional-xml-attribute "useCalibratedValue" (format-bool use-calibrated-value))
	  (optional-xml-attribute "comparisonOperator" (format-symbol comparison-operator)))))

(defclass comparison-list (xtce-list) ())

(defun make-comparison-list (&rest items)
  (make-instance 'xtce-list :xml-element-name "ComparisonList" :base-type 'comparison :items items))

(defclass boolean-expression (context-match)
  ((expression :initarg :expression
               :documentation "a predicate")))

(defclass custom-algorithm (context-match)
  ((expression :initarg :expression
               :documentation "LISP expression")))

(defclass spline-calibrator (numeric-calibrator)
  ((order :initarg order)
   (extrapolate :initarg extrapolate)
   (spline-point-list :initarg spline-point-list :type spline-point-list])
   (ancillary-data-set :initarg ancillary-data-set)))

(defclass spline-point ()
  ((order :initarg :order :type integer)
   (raw :initarg :raw :type number)
   (calibrated :initarg :calibrated :type number)))

(defun context-calibrator-list-p (l)
  (and (listp l)
       (every (lambda (i) (typep i 'NUMERIC-CALIBRATOR)) l)))

(deftype context-calibrator-list ()
  '(satisfies context-calibrator-list-p))

;;(assert (or (equalp extrapolate t) (null extrapolate)) "value for extrapolate ~A must be TRUE or null" extrapolate)

(defun make-spline-point (order raw calibrated)
  (check-type order integer)
  (assert (>= order 0) (order) "value for order ~A must be a positive integer" order)
  (assert (numberp calibrated) (calibrated) "value for calibrated ~A must be a number" calibrated)
  (assert (numberp raw) (raw) "value for raw ~A must be a number" raw)
  (make-instance 'SPLINE-POINT :order order :raw raw :calibrated calibrated))

(defun spline-point-list-p (l)
  (and (listp l)
       (>= 2 (length l))
       (every (lambda (i) (typep i 'spline-point)) l)))

(deftype spline-point-list () "list of spline points"
  '(satisfies spline-point-list-p))

(defclass polynomial-calibrator (numeric-calibrator)
  ((term-list :type term-list
              :initarg :term-list)))

(defclass term()
  ((coefficient :initarg :coefficient
                :type integer )
   (exponent :initarg :exponent
             :type float)))

(defmethod cxml-marshall ((obj term))
  (with-slots (coefficient exponent) obj
    (cxml:with-element* ("xtce" "Term")
      (cxml:attribute "coefficient" (format nil "~A" coefficient))
      (cxml:attribute "exponent" (format nil "~A" exponent)))))

(defmethod cxml::unparse-attribute ((obj term))
  (with-slots (coefficient exponent) obj
    "OK"))

(defun make-term (&key coefficient exponent)
  (check-type coefficient number)
  (check-type exponent number)
  (assert (>= exponent 0) (exponent) "Exponent value ~A must be non-negative, use math-operation-calibrator" exponent)
  (make-instance 'term :coefficient coefficient
                       :exponent exponent))

(defclass term-list (xtce-list) ())
                              
(defun make-term-list (&rest terms)
  (assert (>= (length terms) 1) (terms) "Parameter TERMS: ~A must have at least one polynomial term." terms)
  (dolist (i terms)
    (check-type i term))
  (make-instance 'term-list :items terms :xml-element-name "" :base-type 'term))

(defclass ancillary-data-set () ())

(defun make-polynomial-calibrator (&key name short-description ancillary-data-set term-list)
  (check-type term-list term-list)
  (check-optional-type name string)
  (check-optional-type short-description string)
  (check-optional-type ancillary-data-set ancillary-data-set)
  (make-instance 'polynomial-calibrator :term-list term-list
                                        :name name
                                        :ancillary-data-set ancillary-data-set
                                        :short-description short-description))

(defmethod cxml-marshall ((obj polynomial-calibrator))
  (with-slots (term-list name short-description ancillary-data-set) obj
    (cxml:with-element* ("xtce" "PolynomialCalibrator")
      (if name (cxml:attribute "name" (format nil "~A" name)))
      (if short-description (cxml:attribute "shortDescription" short-description))
      (if ancillary-data-set (cxml-marshall ancillary-data-set))
      (cxml-marshall term-list))))

; Note: CCSDS 660.1-G-2 typo Page 4-142, figure caption says ContainrRefEntry
; Note 4.3.4.8.7 StreamSegmentRefEntry Figure 4-84 describes stream segment and not stream segment ref
;Figure 3-13: DiscreteLookup describes discretelookuplisttype
; PG. 5-14 Typo "revolved"



