(in-package :xtce)

(defmacro check-optional-type (place type &optional type-string)
  `(when ,place
	 (check-type ,place ,type ,type-string)))

(defun optional-xml-attribute (qname value)
  (when (and qname value)
	(cxml:attribute qname value)))

(defclass space-system ()
  ((header :initarg :header)
   (name :initarg :name :type symbol)
   (operational-status :initarg :operational-status)
   (long-description :initarg :long-description
                     :type long-description)
   (alias-set :initarg :alias-set)
   (ancialliary-data-set :initarg :ancillary-data-set)
   (telemetry-metadata :initarg :telemetry-metadata)
   (command-metadata :initarg :command-metadata)
   (xml-base :initarg :xml-base)
   (service-set :initarg :service-set)
   (space-system-list :initarg :space-system-list :type space-system-list)
   (parent-system :initarg :parent-system :writer :parent-system)
   (symbol-table :initarg :symbol-table :type hash-table)
   (short-description :initarg :short-description)
   (root :initarg :root :type boole)))

(deftype space-system-list ()
  "Not an actual XTCE construct, but very convenient for us."
  `(satisfies space-system-list-p))

(defun space-system-list-p (l)
  (and (listp l)
	   (every #'(lambda (i) (typep i 'space-system)) l)))

(defun make-space-system (name
                          &key
							root
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
                            space-system-list
							short-description)
  (check-type name symbol)
  (check-optional-type long-description long-description)
  (let* ((sys (make-instance 'space-system
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
							 :space-system-list space-system-list
							 :short-description short-description
							 :symbol-table (make-filesystem-hash-table)
							 :root root
							 ))
		 (new-symbol (eval `(defparameter ,name ,sys ,short-description))))
	(when root
	  (setf (slot-value sys 'symbol-table) (make-filesystem-hash-table :root t))
	  (finalize-space-system sys nil))
	(if root
	 new-symbol
	 sys)))

(defun finalize-space-system (space-system parent-system)
  (setf (slot-value space-system 'parent-system) parent-system)
  (when parent-system
	(register-filesystem-hash-table (slot-value parent-system 'symbol-table) (slot-value space-system 'symbol-table) (slot-value space-system 'name))
	(add-unique-key 'back parent-system (slot-value space-system 'symbol-table))
	)
  (register-system-keys space-system)
  (when (slot-value space-system 'space-system-list)
	(with-slots (space-system-list) space-system
	  (dolist (child-system space-system-list)
		(finalize-space-system child-system space-system)
		(restart-case (type-check-parameter-set child-system)
		  (continue-with-overwrite () :report (lambda (stream) (format stream "overwrite parameter-ref value and continue."))))))))

(defclass long-description () ((long-description :initarg :long-description
                                                 :type string)))

(defun register-system-keys (space-system)
  (macrolet ((register-keys-in-sequence (sequence slot-name)
			   `(let* ((slot-name-sequence (slot-value ,sequence ,slot-name)))
				 (dolist (item slot-name-sequence)
				   (restart-case (add-unique-key (slot-value item 'name) item symbol-table)
					 (continue-with-overwrite () :report (lambda (stream)
														 (format stream "continue overwriting [key: ~A with value: ~A] for space system ~A"
																 (slot-value item 'name)
																 item
																 space-system))
					   (setf (gethash (slot-value item 'name) symbol-table item) item))
					 (continue-with-new-key (new-key) :report (lambda (stream)
														 (format stream "provide a new key and assign value: ~A for space system ~A"
																 item
																 space-system))
											   :interactive (lambda () (prompt-new-value "Enter a new unique key-name."))
					   (add-unique-key new-key item symbol-table))
					 (continue-with-current () :report (lambda (stream)
														 (format stream "continue with existing entry [key: ~A value: ~A] for space system ~A"
																 (slot-value item 'name)
																 (gethash (slot-value item 'name) symbol-table)
																 space-system))))))))
	
	(With-slots (symbol-table space-system-list telemetry-metadata name parent-system) space-system
	    (when telemetry-metadata
		  (register-keys-in-sequence telemetry-metadata 'parameter-type-set)
		  (register-keys-in-sequence telemetry-metadata 'parameter-set)
		  (register-keys-in-sequence telemetry-metadata 'algorithm-set)
		  (register-keys-in-sequence telemetry-metadata 'stream-set)))))

(define-condition parameter-ref-not-found (error)
  ((parameter :initarg :parameter :accessor parameter)
   (parameter-type-ref :initarg :parameter-type-ref :accessor parameter-type-ref))
  (:report (lambda (condition stream)
     (format stream "Could not find parameter-type-ref: ~a, for parameter: ~a.~&" (parameter-type-ref condition) (parameter condition)))))

(define-condition parameter-not-found (error)
  ((container :initarg :container :accessor container)
   (parameter-ref :initarg :parameter-ref :accessor parameter-ref))
  (:report (lambda (condition stream)
     (format stream "Could not find parameter-ref: ~a, for container: ~a.~&" (parameter-ref condition) (container condition)))))

(defun type-check-parameter-set (space-system)
  (let* ((telemetry-metadata (slot-value space-system 'telemetry-metadata))
		 (symbol-table (slot-value space-system 'symbol-table))
		 (parameter-set (if telemetry-metadata (slot-value telemetry-metadata 'parameter-set)))
		 (container-set (if telemetry-metadata (slot-value telemetry-metadata 'container-set))))
	(dolist (parameter parameter-set)
	  (with-slots (parameter-type-ref) parameter
		(unless (find-key-by-path (format nil "~A" parameter-type-ref) symbol-table)
		  (error `parameter-ref-not-found :parameter parameter :parameter-type-ref parameter-type-ref))))
	(dolist (container container-set )
	  (with-slots (parameter-ref) container
		(unless (find-key-by-path (format nil "~A" parameter-ref) symbol-table)
		  (error `parameter-ref-not-found :container container :parameter-ref parameter-ref))))))

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
               telemetry-metadata
			   space-system-list
			   ) obj
    (cxml:with-element* ("xtce" "SpaceSystem")
      (cxml:attribute*  "xsi" "schemaLocation" "http://www.omg.org/spec/XTCE/20180204 XTCE12.xsd")
      (cxml:attribute "name" (format-symbol name))
      (optional-xml-attribute "header" header)
      (optional-xml-attribute "operational-status" operational-status)
      (optional-xml-attribute "xml:base" xml-base)
      (cxml-marshall long-description)
      (cxml-marshall telemetry-metadata)
	  (cxml-marshall space-system-list)
	  )))

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
	(cxml:with-element* ("xtce" "TelemetryMetadata")
      (cxml-marshall parameter-type-set)
      (cxml-marshall parameter-set)
      (cxml-marshall container-set)
      (cxml-marshall message-set)
      (cxml-marshall stream-set)
      (cxml-marshall algorithm-set))))

(deftype unit-set ()
  `(satisfies unit-set-p))

(defun unit-set-p (l)
  (and (listp l)
	   (every #'(lambda (i) (typep i 'unit)) l)))

(defclass unit () ((power :initarg :power
                          :type number)
                   (factor :initarg :factor)
                   (description :initarg :description
                                :type string)
                   (form :initarg :form)))

(defun make-unit (&key power factor description form)
  (let ((form-symbol (intern (symbol-name form) :xtce)))
	(assert (member form-symbol '(calibrated uncalibrated raw)))
	(make-instance 'unit :power power
						 :factor factor
						 :description description
						 :form form-symbol)))

(defmethod cxml-marshall ((obj unit))
  (with-slots (power factor description form) obj
    (cxml:with-element* ("xtce" "unit") 
      (if power (cxml:attribute "power" power))
      (if factor (cxml:attribute "factor" factor))
      (if description (cxml:attribute "description" description))
      (if form (cxml:text form)))))

(defun dump-xml (element)
  (cxml:with-xml-output (cxml:make-string-sink :indentation 2 :canonical nil )
    (cxml:comment "Bifrost Integral")
    (cxml:with-namespace ("xtce" "http://www.omg.org/spec/XTCE/20180204")
      (cxml:with-namespace ("xsi" "http://www.w3.org/2001/XMLSchema-instance")
        (cxml-marshall element)))))

(defmethod cxml-marshall ((obj NULL)))

(deftype container-set ()
  `(satisfies container-set-p))

(defun container-set-p (l)
  (and (listp l)
	   (every #'(lambda (i) (typep i 'sequence-container)) l)))

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
	  (cxml:attribute "name" (format-symbol name))
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
	  (cxml:attribute "name" (format-symbol name))
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

(defclass entry () ())

(deftype entry-list ()
  `(satisfies entry-list-p))

(defun entry-list-p (l)
  (and (listp l)
	   (every #'(lambda (i) (typep i 'entry)) l)))

(defclass parameter-ref-entry (entry) ((parameter-ref :initarg :parameter-ref)
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
	(cxml:attribute "parameterRef" (format-symbol parameter-ref))
	(cxml-marshall location-in-container-in-bits)
	(cxml-marshall repeat-entry)
	(cxml-marshall include-condition)
	(cxml-marshall time-association)
	(cxml-marshall ancillary-data-set))))

(defmethod cxml-marshall ((obj parameter-ref-entry))
  (with-slots (parameter-ref
			   short-description
			   location-in-container-in-bits
			   repeat-entry
			   include-condition
			   time-association
			   ancillary-data-set) obj
  (cxml:with-element* ("xtce" "ParameterRefEntry")
	(cxml:attribute "parameterRef" (format-symbol parameter-ref))
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

(deftype rate-in-stream-set ()
  `(satisfies rate-in-stream-set-p))

(defun rate-in-stream-set-p (l)
  (and (listp l)
	   (every #'(lambda (i) (typep i 'rate-in-stream)) l)))

(defmethod cxml-marshall ((obj rate-in-stream))
  (with-slots (stream-ref basis minimum-value maximum-value) obj
	(cxml:attribute "streamRef" stream-ref)
	(optional-xml-attribute "basis" basis)
	(optional-xml-attribute "minimumValue" minimum-value)
	(optional-xml-attribute "maximumValue" maximum-value)))

(defclass parameter-segment-ref-entry (entry) ((parameter-ref :initarg :parameter-ref)
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
	  (cxml:attribute "parameterRef" (format-symbol parameter-ref))
	  (cxml:attribute "sizeInBits" size-in-bits)
	  (optional-xml-attribute "shortDescription" short-description)
	  (optional-xml-attribute order order)
	  (cxml-marshall location-in-container-in-bits)
	  (cxml-marshall repeat-entry)
	  (cxml-marshall include-condition)
	  (cxml-marshall time-association)
	  (cxml-marshall ancillary-data-set))))

(defclass container-ref-entry ()
  ((container-ref :initarg :container-ref)
   (short-description :initarg :short-description :type string)
   (location-in-container-in-bits :initarg :location-in-container-in-bits :type location-in-container-in-bits)
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


(defclass container-segment-ref-entry (entry) ((container-ref :initarg :container-ref)
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
	  (cxml:attribute "containerRef" (format-symbol container-ref))
	  (cxml:attribute "sizeInBits" size-in-bits)
	  (optional-xml-attribute "shortDescription" short-description)
	  (optional-xml-attribute order order)
	  (cxml-marshall location-in-container-in-bits)
	  (cxml-marshall repeat-entry)
	  (cxml-marshall include-condition)
	  (cxml-marshall time-association)
	  (cxml-marshall ancillary-data-set))))

(defclass stream-segment-entry (entry) ((stream-ref :initarg :stream-ref)
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

(defclass indirect-parameter-ref-entry (entry) ((short-description :initarg :short-description :type string)
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

(defclass array-parameter-ref-entry (entry) ((parameter-ref :initarg :parameter-ref)
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
	  (cxml:attribute "parameterRef" (format-symbol parameter-ref))
	  (cxml-marshall dimension-list)
	  (optional-xml-attribute "shortDescription" short-description)
	  (cxml-marshall location-in-container-in-bits)
	  (cxml-marshall repeat-entry)
	  (cxml-marshall include-condition)
	  (cxml-marshall time-association)
	  (cxml-marshall ancillary-data-set))))

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
	  (cxml:attribute "nextContainer" (format-symbol container-ref)))))

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
	  (cxml:attribute "parameterRef" (format-symbol parameter-ref))
	  (optional-xml-attribute "instance" instance)
	  (optional-xml-attribute "useCalibratedValue" use-calibrated-value))))

(defclass discrete-lookup () ())

(deftype discrete-lookup-list ()
  `(satisfies discrete-lookup-list-p))

(defun discrete-lookup-list-p (l)
  (and (listp l)
	   (every #'(lambda (i) (typep i 'discrete-lookup)) l)))

(defclass fixed-value ()
  ((value :initarg :value)))

(defun make-fixed-value (value)
  (make-instance 'fixed-value :value value))

(defmethod cxml-marshall ((obj fixed-value))
  (with-slots (value) obj
	(cxml:with-element* ("xtce" "FixedValue")
	  (cxml:text (format nil "~A" value)))))

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

;TODO: Deftype for size
(defclass size-in-bits () ((size :initarg :size)))

(defun make-size-in-bits (size)
  (make-instance 'size-in-bits :size size))

(defmethod cxml-marshall ((obj size-in-bits))
  (with-slots (size) obj
	(cxml:with-element* ("xtce" "SizeInBits") obj
	  (cxml-marshall size))))

(defclass leading-size ()
  ((size-in-bits-of-size-tag :initarg size-in-bits-of-size-tag
							 :type positive-integer)))

(defun make-leading-size (&optional (size-in-bits-of-size-tag 16))
  (make-instance 'leading-size :size-in-bits-of-size-tag size-in-bits-of-size-tag))

(defmethod cxml-marshall ((obj leading-size))
  (with-slots (size-in-bits-of-size-tag) obj
	  (cxml:with-element* ("xtce" "LeadingSize")
		(cxml:attribute "size-in-bits-of-size-tag" size-in-bits-of-size-tag))))

(defclass parameter-type () ())
 
(deftype parameter-type-set-p ()
  `(satisfies container-set-p))

(defun parameter-type-set-p (l)
  (and (listp l)
	   (every #'(lambda (i) (typep i 'parameter-type)) l)))

(defclass size-range-in-characters () ((min-inclusive :initarg :min-inclusive)
									   (max-inclusive :initarg :max-inclusive)))

(defun make-size-range-in-characters (min-inclusive max-inclusive)
  (make-instance 'size-range-in-characters :min-inclusive min-inclusive
										   :max-inclusive max-inclusive))

(defmethod cxml-marshall ((obj size-range-in-characters))
  (with-slots (min-inclusive max-inclusive) obj
	(cxml:with-element* ("xtce" "SizeRangeInCharacters") 
	  (cxml:attribute "minInclusive" min-inclusive)
	  (cxml:attribute "maxInclusive" max-inclusive))))

(defclass string-parameter-type (parameter-type)
  ((short-description :initarg :short-description :type string)
   (name :initarg :name :type symbol)
   (base-type :initarg :base-type :type string)
   (inital-value :initarg :initial-value :type string)
   (restriction-pattern :initarg :restriction-pattern :type string)
   (character-width :initarg :character-width)
   (long-description :initarg :long-description :type long-description)
   (alias-set :initarg :alias-set :type alias-set)
   (ancillary-data-set :initarg :ancillary-data-set :type ancillary-data-set)
   (unit-set :initarg :unit-set :type unit-set)
   (data-encoding :initarg :data-encoding :type encoding)
   (size-range-in-characters :initarg :size-range-in-characters :type size-range-in-characters)
   (default-alarm :initarg :default-alarm :type default-alarm)
   (context-alarm-list :initarg :context-alarm-list :type context-alarm-list)))

(defun make-string-parameter-type (name &key
										  short-description
										  base-type
										  initial-value
										  restriction-pattern
										  character-width
										  long-description
										  alias-set
										  ancillary-data-set
										  unit-set
										  data-encoding
										  size-range-in-characters
										  default-alarm
										  context-alarm-list)
  (make-instance 'string-parameter-type
				 :name name
				 :short-description short-description
				 :base-type base-type
				 :initial-value initial-value
				 :restriction-pattern restriction-pattern
				 :character-width character-width
				 :long-description long-description
				 :alias-set alias-set
				 :ancillary-data-set ancillary-data-set
				 :unit-set unit-set
				 :data-encoding data-encoding
				 :size-range-in-characters size-range-in-characters
				 :default-alarm default-alarm
				 :context-alarm-list context-alarm-list))

(defmethod cxml-marshall ((obj string-parameter-type))
  (with-slots (short-description name base-type inital-value restriction-pattern
			   character-width long-description alias-set ancillary-data-set unit-set
			   encoding size-range-in-characters default-alarm context-alarm-list) obj
	(cxml:with-element* ("xtce" "StringParameterType")
	  (cxml:attribute "shortDescription" short-description)
	  (cxml:attribute "name" (format-symbol name))
	  (cxml:attribute "initalValue" inital-value)
	  (cxml:attribute "restrictionPattern" restriction-pattern)
	  (cxml:attribute "characterWidth" character-width)
	  (cxml-marshall long-description)
	  (cxml-marshall alias-set)
	  (cxml-marshall ancillary-data-set)
	  (cxml-marshall unit-set)
	  (cxml-marshall encoding)
	  (cxml-marshall size-range-in-characters)
	  (cxml-marshall default-alarm)
	  (cxml-marshall context-alarm-list))))

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
  (check-type name symbol)
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
      (cxml:attribute "name" (format-symbol name))
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
      (optional-xml-attribute "name" (format-symbol name))
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
                                    unit-set 
                                    data-encoding
                                    to-string
                                    valid-range
                                    default-alarm
                                    context-alarm-list)
  (check-type name symbol)
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

(defclass binary-context-alarm-list () ())

(defclass binary-parameter-type (parameter-type)
  ((short-description :initarg :short-description :type string)
   (name :initarg :name :type string)
   (initial-value :initarg :initial-value)
   (base-type :initarg :base-type)
   (data-encoding :initarg :data-encoding
				  :type data-encoding)
   (default-alarm :initarg :default-alarm
				  :type enumeration-alarm-type)
   (binary-context-alarm-list :initarg :binary-context-alarm-list
							  :type binary-context-alarm-list)

   (long-description :initarg :long-description
					 :type long-description)
   (alias-set :initarg :alias-set :type alias-set)
   (ancillary-data-set :initarg :ancillary-data-set :type ancillary-data-set)
   (unit-set :initarg :unit-set :type unit-set)))

(defun make-binary-parameter-type (name
									   &key
										 short-description
										 base-type
										 initial-value
										 long-description
										 alias-set
										 ancillary-data-set
										 unit-set
										 data-encoding
										 default-alarm
										 binary-context-alarm-list)
  (check-type name symbol)
  (check-optional-type short-description string)
  ;(check-optional-type base-type T)
  (check-optional-type long-description long-description)
  (check-optional-type alias-set alias-set)
  (check-optional-type ancillary-data-set ancillary-data-set)
  (check-optional-type unit-set unit-set)
  (check-optional-type data-encoding data-encoding)
  (check-optional-type default-alarm enumeration-alarm)
  (check-optional-type binary-context-alarm-list binary-context-alarm-list)
  ;(check-optional-type initial-value T)
  ; Need to check if inital value is in enumeration list
  (make-instance 'binary-parameter-type :name name
											:short-description short-description
											:base-type base-type
											:initial-value initial-value
											:long-description long-description
											:alias-set alias-set
											:ancillary-data-set ancillary-data-set
											:unit-set unit-set
											:data-encoding data-encoding
											:default-alarm default-alarm
											:binary-context-alarm-list binary-context-alarm-list))

(defmethod cxml-marshall ((obj binary-parameter-type))
  (with-slots (name short-description base-type initial-value long-description alias-set ancillary-data-set unit-set data-encoding default-alarm binary-context-alarm-list) obj
	(cxml:with-element* ("xtce" "BinaryParameterType")
	  (cxml:attribute "name" (format-symbol name))
	  (cxml:attribute "shortDescription" short-description)
	  (cxml:attribute "baseType" base-type)
	  (cxml:attribute "initialValue" initial-value)
	  (cxml-marshall long-description)
	  (cxml-marshall alias-set)
	  (cxml-marshall ancillary-data-set)
	  (cxml-marshall unit-set)
	  (cxml-marshall data-encoding)
	  (cxml-marshall default-alarm)
	  (cxml-marshall binary-context-alarm-list))))

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
   (unit-set :initarg :unit-set :type unit-set)
   (alist :initarg :alist :type list)
   ))

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
  (check-type name symbol)
  (check-optional-type short-description string)
  ;(check-optional-type base-type T)
  (check-optional-type long-description long-description)
  (check-optional-type alias-set alias-set)
  (check-optional-type ancillary-data-set ancillary-data-set)
  (check-optional-type unit-set unit-set)
  (check-optional-type data-encoding data-encoding)
  (check-optional-type enumeration-list enumeration-list)
  (check-optional-type default-alarm enumeration-alarm)
  (check-optional-type context-alarm-list context-alarm-list)
  ;(check-optional-type initial-value T)
  ; Need to check if inital value is in enumeration list
  ; TODO: mapcar?
  (let* ((alist (loop for enumeration in enumeration-list
					  for label = (slot-value enumeration 'label)
					  for value = (slot-value enumeration 'value)
					  for entry = (cons label value)
					  collect entry)))
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
											  :context-alarm-list context-alarm-list
											  :alist alist)))

(defclass enumeration-alarm (alarm)
  ((alarm-level :initarg :alarm-level :type string)
   (enumeration-label :initarg :enumeration-label :type symbol)))

(defun make-enumeration-alarm (alarm-level enumeration-label)
  (let ((alarm-level (intern (symbol-name alarm-level) :xtce))
		(allowed-alarm-levels '(normal watch warning distress critical sever)))
  (check-type enumeration-label symbol)
  (check-type alarm-level symbol)
  (assert (member alarm-level allowed-alarm-levels) (alarm-level) "Alarm level ~A is not one of ~A" alarm-level allowed-alarm-levels) 
  (make-instance 'enumeration-alarm :alarm-level alarm-level  :enumeration-label enumeration-label)))

(deftype enumeration-list ()
  `(satisfies enumeration-list-p))

(defun enumeration-list-p (l)
  (and (listp l)
	   (every #'(lambda (i) (typep i 'enumeration)) l)))

(defclass enumeration ()
  ((value :initarg :value)
  (max-value :initarg :max-value)
  (label :initarg :label
		 ;; :type symbol
		 )
  (short-description :initarg :short-description
					 :type string)))

(defun make-enumeration (label value &key max-value short-description)
  ;(check-type value symbol)
  ;(check-type label symbol)
  (check-optional-type max-value number)
  (check-optional-type short-description string)
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
	  (cxml:attribute "name" (format-symbol name))
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
  (check-type name symbol)
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
	  (cxml:attribute "name" (format-symbol name))
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
	  (cxml:attribute "parameterRef" (format-symbol parameter-ref)))))

(defclass epoch ()
  ((epoch-value :initarg :epoch-value
				:type symbol)))

(defun make-epoch (epoch-value)
  ; TODO: figure out xs and dateTime types
  (let ((epoch-value (intern (symbol-name epoch-value) :xtce))
		(allowed-epoch-enumerations '(TAI J2000 UNIX GPS)))
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
  (check-optional-type units symbol)
  (check-optional-type scale number)
  (check-optional-type offest number)
  (check-optional-type data-encoding data-encoding)
  (check-optional-type reference-time reference-time)
  (make-instance 'encoding :units units :scale scale :offest offest :data-encoding data-encoding :reference-time reference-time))

(defmethod cxml-marshall ((obj encoding))
  (with-slots (units scale offset data-encoding reference-time) obj
	(cxml:with-element* ("xtce" "Encoding") 
	  (optional-xml-attribute "units" (format-symbol units))
	  (optional-xml-attribute "scale" (format-number scale))
	  (optional-xml-attribute "offset" offset)
	  (cxml-marshall data-encoding))))


;TODO: Encoding parameters may not accept all data encodings 


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
  (check-type name symbol)
  (check-type parameter-type-ref symbol)
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
	  (cxml:attribute "name"  (format-symbol name))
	  (cxml:attribute "parameterTypeRef" (format-symbol parameter-type-ref))
	  (optional-xml-attribute "shortDescription" short-description)
	  (optional-xml-attribute "initialValue" initial-value)
	  (cxml-marshall long-description)
	  (cxml-marshall alias-set)
	  (cxml-marshall ancillary-data-set)
	  (cxml-marshall parameter-properties))))

(deftype parameter-type-set ()
  `(satisfies parameter-type-set-p))

(deftype parameter-set ()
  `(satisfies parameter-set-p))

(defun parameter-set-p (l)
  (and (listp l)
	   (every #'(lambda (i) (typep i 'parameter)) l)))

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
	  (optional-xml-attribute "dataSource" (format-symbol data-source))
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
  (let ((enc (intern (symbol-name enc) :xtce)))
	(assert (member enc '(unsigned sign-magnitude twos-complement ones-compliment bcd packed-bcd)) (enc)
			"Int Encoding ~A is not <Unsigned | twos-complement | ones-compliment | bcd | packed-bcd" enc) 
	t))

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

(deftype string-size ()
  `(satisfies string-size-p))

(defun string-size-p (a)
  (or (typep a 'size-in-bits)
	  (typep a 'variable-string)))

(defun value-lookup-p (a)
  (or (typep a 'dynamic-value)
	  (typep a 'discrete-lookup-list)))

(deftype value-lookup ()
  `(satisfies value-lookup-p))

(deftype bit-order ()
  '(member |mostSignificantBitFirst| |leastSignificantBitFirst|))

(deftype byte-order ()
  `(member |mostSignificantByteFirst| |leastSignificantByteFirst|))

(deftype string-encoding ()
  `(member US-ASCII WINDOWS-1252 ISO-UTF-8 UTF-16 UTF-16LE UTF-16BE UTF-32 UTF-32LE UTF-32BE))

(defclass leading-size () ((size-in-bits-of-size-tag :initarg size-in-bits-of-size-tag :type positive-integer :documentation "Positive integer representing how many bits the string size integer is."))
  (:documentation "In some string implementations, the size of the string contents (not the memory allocation size) is determined by a leading numeric value. This is sometimes referred to as Pascal strings. If a LeadingSize is specified, then the TerminationChar element does not have a functional meaning."))

(defclass variable-string ()
  ((max-size-in-bits :initarg :max-size-in-bits :type positive-integer
					 :documentation "XTCE: The upper bound of the size of this string data type so that the implementation can reserve/allocate enough memory to capture all reported instances of the string. Bifrost: This value is mostly ignored since we don't need to preallocate space, but you must define it since it is required by the XTCE spec, dynamic containers may require this value.")
   (value-lookup :initarg :value-lookup :type value-lookup :documentation "<DynamicValue | DiscreteLookupList> XTCE: Determine the container size in bits by interrogating an instance of a parameter.")
   (leading-size :initarg :leading-size :type leading-size
				 :documentation "Use to describe Pascal Strings (e.g. pull the next n characters from the stream).")
   (termination-character :initarg :termination-character :type t :documentation "Ignored if leading-size is not nil.")
)
  (:documentation "This is referred to as VARIABLE in XTCE. Its purpose is to help define variable/dynamic length strings. In particular, leading-size can be used to define pascal string encoding (i.e. length of the string is stored here). Otherwise, the termination character can be set to null to define C strings and the like. XTCE specifies that LeadingSize takes precedence over TerminationChar and that we should let you define both simultaneously. XTCE: Variable length strings are those where the space occupied in a container can vary. If the string has variable content but occupies the same amount of space when encoded should use the SizeInBits element. Specification of a variable length string needs to consider that the implementation needs to allocate space to store the string. Specify the maximum possible length of the string data type for memory purposes and also specify the bit size of the string to use in containers with the dynamic elements."))

(defun make-variable (max-size-in-bits value-lookup leading-size termination-character)
  (make-instance 'variable-string
				 :max-size-in-bits max-size-in-bits
				 :value-lookup value-lookup
				 :leading-size leading-size
				 :termination-character termination-character))

(defclass string-data-encoding (data-encoding)
  ((string-size-type :initarg :string-size-type :type string-size)
   (bit-order :initarg :bit-order :type bit-order)
   (error-detect-correct :initarg :error-detect-correct :type error-detect-correct)
   (byte-order :initarg :byte-order :type byte-order)
   (string-encoding :initarg :string-encoding :type string-encoding)))

(defun make-string-data-encoding (string-size-type &optional
													 (bit-order '|mostSignificantBitFirst|)
													 (byte-order '|mostSignificantByteFirst|)
													 (string-encoding 'UTF-8)
													 (error-detect-correct))
  (make-instance 'string-data-encoding
				 :string-size-type string-size-type
				 :bit-order bit-order
				 :error-detect-correct error-detect-correct
				 :byte-order byte-order
				 :string-encoding string-encoding))

(defclass binary-data-encoding (data-encoding)
  ((bit-order :initarg :bit-order :type bit-order)
   (byte-order :initarg :byte-order :type byte-order)
   (size-in-bits :initarg :size-in-bits :type size-in-bits)
   (error-detect-correct :initarg :error-detect-correct :type error-detect-correct)
   (from-binary-transform-algorithm :initarg :from-binary-transform-algorithm)
   (to-binary-transform-algorithm :initarg :to-binary-transform-algorithm)))

(defun make-binary-data-encoding (size-in-bits &key
												 (bit-order '|mostSignificantBitFirst|)
												 (byte-order '|mostSignificantByteFirst|)
												 (error-detect-correct)
                                                 (from-binary-transform-algorithm)
                                                 (to-binary-transform-algorithm))
  (make-instance 'binary-data-encoding
                 :size-in-bits size-in-bits
                 :bit-order bit-order
                 :byte-order byte-order
                 :error-detect-correct error-detect-correct
                 :from-binary-transform-algorithm from-binary-transform-algorithm
                 :to-binary-transform-algorithm to-binary-transform-algorithm))

(defmethod cxml-marshall ((obj binary-data-encoding))
  (with-slots (size-in-bits bit-order byte-order error-detect-correct from-binary-transform-algorithm to-binary-transform-algorithm) obj
	(cxml:with-element* ("xtce" "BinaryDataEncoding")
	  (optional-xml-attribute "bitOrder" (format-symbol bit-order))
	  (optional-xml-attribute "byteOrder" (format-symbol byte-order))
	  (cxml-marshall error-detect-correct)
	  (cxml-marshall size-in-bits)
	  (cxml-marshall from-binary-transform-algorithm)
	  (cxml-marshall to-binary-transform-algorithm))))

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
  (let ((size (intern (symbol-name size) :xtce)))
	(if (member size '(32 64 128 'non-32))
		t)))

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
  (let ((bit-order (intern (symbol-name bit-order) :xtce)))
	(assert (member bit-order '(MSB LSB)) (bit-order) "Bit Order ~A is not <LSB | MSB>" bit-order) t))

(defun valid-float-encoding-p (enc)
  (let ((enc (intern (symbol-name enc) :xtce)))
	(assert (member enc '(IEE MIL)) (enc) "Encoding ~A is not <IEE | MIL>" enc) t))

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
	  (cxml:attribute "parameterRef" (format-symbol parameter-ref))
	  (cxml:attribute "value" value)
	  (optional-xml-attribute "instance" instance)
	  (optional-xml-attribute "useCalibratedValue" (format-bool use-calibrated-value))
	  (optional-xml-attribute "comparisonOperator" (format-symbol comparison-operator)))))

(deftype comparison-list ()
  `(satisfies comparison-list-p))

(defun comparison-list-p (l)
  (and (listp l)
	   (every #'(lambda (i) (typep i 'comparison)) l)))

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

(deftype term-list ()
  `(satisfies term-list-p))

(defun term-list-p (l)
  (and
   (not (null l))
   (listp l)
   (every #'(lambda (i) (typep i 'term)) l)))

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


(defclass index () ((index-type :initarg :index-type))) ; deftype satisfies x or y or z

(defclass starting-index (index) ())

(defun make-starting-index (index-type)
  (make-instance 'starting-index :index-type index-type))

(defmethod cxml-marshall ((obj starting-index))
  (with-slots (index-type) obj
	(cxml:with-element* ("xtce" "StartingIndex")
	  (cxml-marshall index-type))))

(defclass ending-index (index) ())

(defun make-ending-index (index-type)
  (make-instance 'ending-index :index-type index-type))

(defmethod cxml-marshall ((obj ending-index))
  (with-slots (index-type) obj
	(cxml:with-element* ("xtce" "EndingIndex")
	  (cxml-marshall index-type))))


(defclass dimension () ((starting-index :initarg :starting-index :type starting-index)
						(ending-index :initarg :ending-index :type ending-index)))

(defun make-dimension (starting-index ending-index)
  (make-instance 'dimension :starting-index starting-index :ending-index ending-index))

(defmethod cxml-marshall ((obj dimension))
  (with-slots (starting-index ending-index) obj
	(cxml:with-element* ("xtce" "Dimension")
	  (cxml-marshall starting-index)
	  (cxml-marshall ending-index))))

(deftype dimension-list ()
  `(satisfies dimension-list-p))

(defun dimension-list-p (l)
  (and
   (listp l)
   (every #'(lambda (i) (typep i 'dimension)) l)))

(defclass array-parameter-type (parameter-type)
  ((short-description :initarg :short-description :type string)
   (name :initarg :name :type symbol)
   (array-type-ref :initarg :array-type-ref :type symbol)
   (dimension-list :initarg :dimension-list :type dimension-list)
   (long-description :initarg :long-description :type long-description)
   (alias-set :initarg :alias-set :type alias-set)
   (ancillary-data-set :initarg :ancillary-data-set :type ancillary-data-set)
   (default-alarm :initarg :default-alarm :type default-alarm)
   (context-alarm-list :initarg :context-alarm-list :type context-alarm-list)))

(defun make-array-parameter-type (name array-type-ref
								  &key
									short-description
									long-description
									alias-set
									ancillary-data-set
									dimension-list)
  (make-instance 'array-parameter-type
				 :name name
				 :array-type-ref array-type-ref
				 :short-description short-description
				 :long-description long-description
				 :alias-set alias-set
				 :ancillary-data-set ancillary-data-set
				 :dimension-list dimension-list))


(defmethod cxml-marshall ((obj array-parameter-type))
  (with-slots (name array-type-ref short-description long-description alias-set ancillary-data-set dimension-list) obj
	  (cxml:with-element* ("xtce" "ArrayParameterType")
		(cxml:attribute "name" (format-symbol name))
		(cxml:attribute "shortDescription" short-description)
		(cxml:attribute "arrayTypeRef" (format-symbol array-type-ref))
		(cxml-marshall dimension-list))))

(defmethod cxml-marshall ((obj string-parameter-type))
  (with-slots (short-description name base-type inital-value restriction-pattern
			   character-width long-description alias-set ancillary-data-set unit-set
			   data-encoding size-range-in-characters default-alarm context-alarm-list) obj
	(cxml:with-element* ("xtce" "StringParameterType")
	  (cxml:attribute "shortDescription" short-description)
	  (cxml:attribute "name" (format-symbol name))
	  (cxml:attribute "initalValue" inital-value)
	  (cxml:attribute "restrictionPattern" restriction-pattern)
	  (cxml:attribute "characterWidth" character-width)
	  (cxml-marshall long-description)
	  (cxml-marshall alias-set)
	  (cxml-marshall ancillary-data-set)
	  (cxml-marshall unit-set)
	  (cxml-marshall data-encoding)
	  (cxml-marshall size-range-in-characters)
	  (cxml-marshall default-alarm)
	  (cxml-marshall context-alarm-list))))

(defmethod print-object ((obj space-system) stream)
      (print-unreadable-object (obj stream :type t)
        (with-slots (name short-description) obj
          (format stream "name: ~a, description: ~a " name short-description))))

(defmethod print-object ((obj parameter) stream)
      (print-unreadable-object (obj stream :type t)
        (with-slots (name short-description parameter-type-ref) obj
          (format stream "name: ~a, description: ~a, type: ~a " name short-description parameter-type-ref))))

(defmethod print-object ((obj parameter-type) stream)
      (print-unreadable-object (obj stream :type t)
        (with-slots (name short-description) obj
          (format stream "name: ~a, description: ~a " name short-description))))


(defclass data-stream () ())

(deftype stream-set ()
  `(satisfies stream-set-p))

(defun stream-set-p (l)
  (and (listp l)
	   (every #'(lambda (i) (typep i 'parameter-type)) l)))

(defclass variable-frame-stream (data-stream) ())

(defclass custom-stream (data-stream) ())

(defclass fixed-frame-stream (data-stream)
  ((name :initarg :name :type symbol :reader name)
   (short-description :initarg :short-description :type short-description :reader :short-description)
   (long-description :initarg :long-description :type string :reader :long-description)
   (alias-set :initarg :alias-set :type alias-set :reader :alias-set)
   (ancillary-data-set :initarg :ancillary-data-set :type ancillary-data-set :reader :ancillary-data-set)
   (bit-rate-in-bps :initarg :bit-rate-in-bips :reader :bit-rate-in-bps)
   (pcm-type :initarg :pcm-type :reader :pcm-type)
   (inverted :initarg :inverted :type boole :reader :inverted)
   (sync-aperture-in-bits :initarg :sync-aperture-in-bits :type sync-aperture-in-bits :reader :sync-aperture-in-bits)
   (frame-length-in-bits :initarg :frame-length-in-bits :type positive-integer :reader :frame-length-in-bits)
   (next-ref :initarg :next-ref :type symbol :reader :next-ref)
   (sync-strategy :initarg :sync-strategy :type sync-strategy :reader :sync-strategy)))

(defun make-fixed-frame-stream (name frame-length-in-bits next-ref sync-strategy
								&key
								  short-description
								  bit-rate-in-bps
								  pcm-type
								  inverted
								  sync-aperture-in-bits
								  long-description
								  alias-set
								  ancillary-data-set)
  "For streams that contain a series of frames with a fixed frame length where the frames are found by looking for a marker in the data. This marker is sometimes called the frame sync pattern and sometimes the Asynchronous Sync Marker (ASM). This marker need not be contiguous although it usually is."
  (make-instance 'fixed-frame-stream
			:name name
			:frame-length-in-bits frame-length-in-bits
			:next-ref next-ref
			:sync-strategy sync-strategy
			:short-description short-description
			:bit-rate-in-bips bit-rate-in-bps
			:pcm-type pcm-type
			:inverted inverted
			:sync-aperture-in-bits sync-aperture-in-bits
			:long-description long-description
			:alias-set alias-set
			:ancillary-data-set ancillary-data-set))

(defclass sync-strategy () ((auto-invert :initarg :auto-invert :type auto-invert)
							(sync-pattern :initarg :sync-pattern :type sync-pattern)
							(verify-to-lock-good-frames :initarg :verify-to-lock-good-frames :type postiive-integer)
							(check-to-lock-good-frames :initarg :check-to-lock-good-frames :type postiive-integer)
							(max-bit-errors-in-sync-pattern :initarg :max-bit-errors-in-sync-pattern :type postiive-integer)))

(defun make-sync-strategy (&key (verify-to-lock-good-frames 4) (check-to-lock-good-frames 1) (max-bit-errors-in-sync-pattern 0))
  "CCSDS: A Sync Strategy specifies the strategy on how to find frames within a stream of PCM data. The sync strategy is based upon a state machine that begins in the 'Search' state until the first sync marker is found. Then it goes into the 'Verify' state until a specified number of successive good sync markers are found. Then, the state machine goes into the 'Lock' state, in the 'Lock' state frames are considered good. Should a sync marker be missed in the 'Lock' state, the state machine will transition into the 'Check' state, if the next sync marker is where it's expected within a specified number of frames, then the state machine will transition back to the 'Lock' state, it not it will transition back to 'Search'"
  (make-instance 'sync-strategy
				 :verify-to-lock-good-frames verify-to-lock-good-frames
				 :check-to-lock-good-frames check-to-lock-good-frames
				 :max-bit-errors-in-sync-pattern max-bit-errors-in-sync-pattern))

(defclass sync-pattern ()
  ((pattern
	:initarg :pattern
	:documentation "Hexadecimal pattern to match against a potential synchronization marker. e.g. CCSDS ASM for non-turbocoded frames is #x1acffc1d"
	:reader :pattern)
   (pattern-length-in-bits
	:initarg :pattern-length-in-bits
	:type positive-integer
	:documentation "Truncate the pattern from the left so that the pattern is exactly this many bits."
	:reader :pattern-length-in-bits)
   (bit-location-from-start
	:initarg :bit-location-from-start
	:type positive-integer
	:documentation "After the synchronization marker is truncated, truncate this amount more so that the left most bit of the frame corresponds to the start of the container."
	:reader :bit-location-from-start)
   (mask
	:initarg :mask
	:documentation "Apply this mask (e.g. #x0x29a) to the potential synchronization marker before checking against the pattern."
	:reader :mask)
   (mask-length-in-bits
	:initarg :mask-length-in-bits
	:type postive-integer
	:documentation "Truncate the mask from the left so that the pattern is exactly this many bits."
	:reader mask-length-in-bits)))

(defun make-sync-pattern (&key (pattern #x1acffc1d) (pattern-length-bits (hex-length-in-bits pattern)) mask mask-length-bits (bit-location-from-start 0))
  "CCSDS: The pattern of bits used to look for frame synchronization. See SyncPatternType.
   Bifrost: Define a synchronization pattern and masks. Used as metadata to search the synchronization markers of fixed frames."
  (make-instance 'sync-pattern
				 :pattern pattern
				 :pattern-length-in-bits pattern-length-bits
				 :bit-location-from-start bit-location-from-start
				 :mask mask
				 :mask-length-in-bits mask-length-bits))


(defgeneric instantiate-parameter (parameter-type data)
  (:documentation "Instantiate a parameter given a byte"))

(defmethod instantiate-parameter ((parameter enumerated-parameter-type) data)
  (assoc data (slot-value parameter 'alist)))

(defmethod cxml-marshall ((obj list))
  "Exclusively used for xtce-lists and xtce-sets (a homogenous list of XTCE constructs).
   These types used to be classes, but its complexity didn't offer any obvious benefits.
  "
  (let ((lname (typecase obj
				 (comparison-list
				  "ComparisonList")
				 (container-set
				  "ContainerSet")
				 (dimension-list
				  "DimensionList")
				 (discrete-lookup-list
				  "DiscreteLookupList")
				 (entry-list
				  "EntryList")
				 (enumeration-list
				  "EnumerationList")
				 (parameter-set
				  "ParameterSet")
				 (parameter-type-set
				  "ParameterTypeSet")
				 (rate-in-stream-set
				  "RateInStreamSet")
				 (space-system-list
				  nil) ;Not an actual XTCE construct
				 (spline-point-list
				  "SplinePointList")
				 (stream-set
				  "StreamSet")
				 (term-list
				  "TermList")
				 (unit-set
				  "UnitSet"))))
	(if lname
		(progn
		  (cxml:with-element* ("xtce" lname)
			(dolist (i obj)
			  (cxml-marshall i))))
		(dolist (i obj)
		  (cxml-marshall i)))))
