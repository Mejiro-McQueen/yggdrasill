(in-package :xtce)

(defmacro check-optional-type (place type &optional type-string)
  `(when ,place
	 (check-type ,place ,type ,type-string)))

(defun optional-xml-attribute (qname value)
  (when (and qname value)
	(cxml:attribute qname value)))

(defclass space-system ()
  ((header :initarg :header :reader :header)
   (name :initarg :name :type symbol :reader :name)
   (operational-status :initarg :operational-status :reader :operational-status)
   (long-description :initarg :long-description :type long-description :reader :long-description)
   (alias-set :initarg :alias-set :type alias-set :reader :alias-set)
   (ancillary-data-set :initarg :ancillary-data-set :type ancillary-data-set :reader :ancillary-data-set)
   (telemetry-metadata :initarg :telemetry-metadata :type telemetry-metadata :reader :telemetry-metadata)
   (command-metadata :initarg :command-metadata :type command-metadata :reader :command-metadata)
   (xml-base :initarg :xml-base :reader xml-base)
   (service-set :initarg :service-set :type service-set :reader :service-set)
   (space-system-list :initarg :space-system-list :type space-system-list :reader :space-system-list)
   (parent-system :initarg :parent-system :accessor :parent-system :type space-system)
   (symbol-table :initarg :symbol-table :type hash-table :reader :symbol-table)
   (short-description :initarg :short-description :reader :short-description)
   (root :initarg :root :type boole :reader :root)))

(deftype space-system-list ()
  "Not an actual XTCE construct, but very convenient for us."
  `(satisfies space-system-list-p))

(defun space-system-list-p (l)
  (and (listp l)
	   (every #'(lambda (i) (typep i 'space-system)) l)))

;;Notes regarding symbol table
;; The symbol table is used to lookup named/keyed xtce objects.
;; They name or reference of the object is interned as a symbol. 
;; When we lookup or place a keyed object into the table, we must intern the symbol
;; This results in the symbols not having a package attached if they originated from a foreign package (i.e. xtce-engine or stc)

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
		 ;(new-symbol (eval `(defparameter ,name ,sys ,short-description)))
		 )
	(when root
	  (setf (slot-value sys 'symbol-table) (make-filesystem-hash-table :root t))
	  (finalize-space-system sys nil))
	sys))

(defun finalize-space-system (space-system parent-system)
  (setf (slot-value space-system 'parent-system) parent-system)
  (register-system-keys space-system)
  (when parent-system
	(link-filesystem-hash-tables (slot-value parent-system 'symbol-table) (slot-value space-system 'symbol-table) (symbol-name (slot-value space-system 'name))))
  (restart-case (type-check-parameter-set space-system)
	  (continue-with-overwrite () :report (lambda (stream) (format stream "overwrite parameter-ref value and continue."))))
  (when (slot-value space-system 'space-system-list)
	(with-slots (space-system-list) space-system
	  (dolist (child-system space-system-list)
		(finalize-space-system child-system space-system)
		))))

(defclass long-description () ((long-description :initarg :long-description
                                                 :type string)))

(defun register-keys-in-sequence (sequence symbol-table system-name)
  (dolist (item sequence)
	(restart-case (add-unique-key (symbol-name (slot-value item 'name)) item symbol-table)
	  (continue-with-overwrite () :report (lambda (stream)
											(format stream "continue overwriting [key: ~A with value: ~A] for space system ~A"
													(slot-value item 'name)
													item
													system-name))
		(setf (gethash (slot-value item 'name) symbol-table item) item))
	  (continue-with-new-key (new-key) :report (lambda (stream)
												 (format stream "provide a new key and assign value: ~A for space system ~A"
														 item
														 system-name))
									   :interactive (lambda () (prompt-new-value "Enter a new unique key-name."))
		(add-unique-key new-key item symbol-table))
	  (continue-with-current () :report (lambda (stream)
										  (format stream "continue with existing entry [key: ~A value: ~A] for space system ~A"
												  (symbol-name (slot-value item 'name))
												  (gethash (slot-value item 'name) symbol-table)
												  system-name)))))
  symbol-table)

(defun get-items (sequence slot-name)
  (slot-value sequence slot-name))

(defun register-system-keys (space-system)
	(With-slots (symbol-table space-system-list telemetry-metadata name parent-system) space-system
	  (when telemetry-metadata
		(register-keys-in-sequence (get-items telemetry-metadata 'parameter-type-set) symbol-table name)
		(register-keys-in-sequence (get-items telemetry-metadata 'parameter-set) symbol-table name)
		(register-keys-in-sequence (get-items telemetry-metadata 'algorithm-set) symbol-table name)
		(register-keys-in-sequence (get-items telemetry-metadata 'container-set) symbol-table name)
		(register-keys-in-sequence (get-items telemetry-metadata 'stream-set) symbol-table name))))

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

(define-condition entry-not-found (error)
  ((container :initarg :container :accessor container)
   (entry :initarg :entry :accessor entry))
  (:report (lambda (condition stream)
     (format stream "Could not find container-ref entry: ~a, for container: ~a.~&" (entry condition) (container condition)))))

(define-condition stream-next-ref-not-found (error)
  ((stream :initarg :_stream :accessor _stream)
   (next-ref :initarg :next-ref :accessor next-ref))
  (:report (lambda (condition stream)
     (format stream "Could not find next-ref entry: ~a, for stream: ~a.~&" (next-ref condition) (_stream condition)))))

(defun check-stream-set-refs (space-system)
  (let* ((telemetry-metadata (slot-value space-system 'telemetry-metadata))
		 (symbol-table (slot-value space-system 'symbol-table))
		 (stream-set (if telemetry-metadata (slot-value telemetry-metadata 'stream-set))))
	(dolist (stream stream-set)
	  (with-slots (next-ref) stream
		(typecase next-ref
		  (stream-ref)
		  (container-ref 
		   (unless (find-key-by-path (format nil "~A" (slot-value next-ref 'container-ref)) symbol-table)
			 (error `stream-next-ref-not-found :_stream stream :next-ref next-ref))))))))

;TODO: Use dereference from xtce-engine (i.e. move it here)
(defun check-container-set-refs (space-system)
  (let* ((telemetry-metadata (slot-value space-system 'telemetry-metadata))
		 (symbol-table (slot-value space-system 'symbol-table))
		 (container-set (if telemetry-metadata (slot-value telemetry-metadata 'container-set))))
	(dolist (container container-set)
	  (with-slots (entry-list) container
		(dolist (entry-item entry-list)
		  (check-type entry-item entry "is not a valid container entry-list item:  <parameter-ref-entry | paraemter-segment-ref-entry | container-ref-entry | container-segment-entry | stream-segment-entry | indirect-parameter-ref-entry | array-parameter-ref-entry>")
		  (let* ((entry-ref (ref entry-item))
				 (entry-ref-string (format nil "~A" entry-ref))
				 (dereferenced-entry (find-key-by-path entry-ref-string symbol-table)))
			(check-dereference-mismatch entry-item dereferenced-entry)
			(unless dereferenced-entry
			  (error `entry-not-found :container container :entry entry-ref))))))))

;Type of entry should align with dereferenced type

(defun check-parameter-refs (space-system)
  (let* ((telemetry-metadata (slot-value space-system 'telemetry-metadata))
		 (symbol-table (slot-value space-system 'symbol-table))
		 (parameter-set (if telemetry-metadata (slot-value telemetry-metadata 'parameter-set))))
	;(print (alexandria:hash-table-keys symbol-table))
	(dolist (parameter parameter-set)
	  (with-slots (parameter-type-ref) parameter
		;; (print (format nil "~%~%~%"))
		;; (print parameter-type-ref)
		;; (print (symbol-name parameter-type-ref))
		;; (print (find-key-by-path (symbol-name parameter-type-ref) symbol-table))
		(dereference-with-cycle-detection parameter symbol-table)
		(unless (find-key-by-path (format nil "~A" parameter-type-ref) symbol-table)
		  (error `parameter-ref-not-found :parameter parameter :parameter-type-ref parameter-type-ref))
		))))

(defun type-check-parameter-set (space-system)
  (check-parameter-refs space-system)
  (check-container-set-refs space-system)
  (check-stream-set-refs space-system)
  )


(defun make-long-description (s)
  (check-type s string)
  (make-instance 'long-description :long-description s))

(defmethod marshall ((obj long-description))
  (with-slots (long-description) obj
    (cxml:with-element* ("xtce" "LongDescription"))
    (cxml:text long-description)))

(defgeneric marshall (obj)
  (:documentation "TIP: I'm pretty sure you need to dump all the attributes first, because the element macro won't unwind"
  ))

(defmethod marshall ((obj space-system))
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
      (cxml:attribute "name" name)
      (optional-xml-attribute "header" header)
      (optional-xml-attribute "operational-status" operational-status)
      (optional-xml-attribute "xml:base" xml-base)
      (marshall long-description)
      (marshall telemetry-metadata)
	  (marshall space-system-list)
	  )))

(defclass telemetry-metadata ()
  ((parameter-type-set :initarg :parameter-type-set
                       :type parameter-type-set
					   :reader :parameter-type-set)
   (parameter-set :initarg :parameter-set
                  :type :parameter-set
				  :reader parameter-set)
   (container-set :initarg :container-set
                  :type container-set
				  :reader container-set)
   (message-set :initarg :message-set
                :type message-set
				:reader :message-set)
   (stream-set :initarg :stream-set
               :type stream-set
			   :reader :stream-set)
   (algorithm-set :initarg :algorithm-set
                  :type algorithm-set
				  :reader :algorithm-set)))

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

(defmethod marshall ((obj telemetry-metadata))
  (with-slots (parameter-type-set parameter-set container-set message-set stream-set algorithm-set) obj
	(cxml:with-element* ("xtce" "TelemetryMetadata")
      (marshall parameter-type-set)
      (marshall parameter-set)
      (marshall container-set)
      (marshall message-set)
      (marshall stream-set)
      (marshall algorithm-set))))

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

(defmethod marshall ((obj unit))
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
        (marshall element)))))

(defmethod marshall ((obj NULL)))

(deftype container-set ()
  `(satisfies container-set-p))

(defun container-set-p (l)
  (and (listp l)
	   (every #'(lambda (i) (typep i 'sequence-container)) l)))

(defclass sequence-container () ((name :initarg :name :type string)
								 (short-description :initarg :short-description :type string)
								 (abstract :initarg :abstract :type bool)
								 (idle-pattern :initarg :idle-pattern :reader idle-pattern)
								 (long-description :initarg :long-description :type long-description)
								 (alias-set :initarg :alias-set :type alias-set)
								 (ancillary-data-set :initarg :ancillary-data-set :type ancillary-data-set)
								 (default-rate-in-stream :initarg :default-rate-in-stream :type default-rate-in-stream)
								 (rate-in-stream-set :initarg :rate-in-stream-set :type rate-in-stream-set)
								 (binary-encoding :initarg :binary-encoding :type binary-encoding)
								 (entry-list :initarg :entry-list :type entry-list :reader entry-list)
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

  (check-type entry-list entry-list)
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

(defmethod marshall ((obj sequence-container))
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
	  (optional-xml-attribute "abstract" abstract)
	  (optional-xml-attribute "idlePattern" (print-hex idle-pattern))
	  (marshall long-description)
	  (marshall alias-set)
	  (marshall ancillary-data-set)
	  (marshall rate-in-stream-set)
	  (marshall default-rate-in-stream)
	  (marshall binary-encoding)
	  (marshall entry-list)
	  (marshall base-container))))


;TODO: I think I would rather just have a conditional reader macro to choose that suggested non-XTCE format 
(defclass resolved-sequence-container (sequence-container)
  ((restriction-criteria-set :initarg :restriction-criteria-set :type restriction-criteria-set)))

(defmethod marshall ((obj resolved-sequence-container))
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
	  (optional-xml-attribute "abstract" abstract)
	  (optional-xml-attribute "idlePattern" (print-hex idle-pattern))
	  (marshall long-description)
	  (marshall alias-set)
	  (marshall ancillary-data-set)
	  (marshall rate-in-stream-set)
	  (marshall default-rate-in-stream)
	  (marshall binary-encoding)
	  (marshall entry-list)
	  (marshall restriction-criteria-set))))

(defmethod print-object ((obj sequence-container) stream)
  (print-unreadable-object (obj stream :type t)
    (with-slots (name short-description abstract base-container) obj
      (format stream "name: ~a, description: ~a, abstract:~a, base-container?:~a" name short-description abstract base-container))))

(defclass default-rate-in-stream () ((basis :initarg :basis)
									 (minimum-value :initarg :minimum-value)
									 (maximum-value :initarg :maximum-value)))

(defun make-default-rate-in-stream (&key basis minimum-value maximum-value)
  (make-instance 'default-rate-in-stream :basis basis :minimum-value minimum-value :maximum-value maximum-value))

(defmethod marshall ((obj default-rate-in-stream))
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

(defclass parameter-ref-entry (entry) ((parameter-ref :initarg :parameter-ref :type symbol :reader ref)
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

(defmethod marshall ((obj parameter-ref-entry))
  (with-slots (parameter-ref
			   short-description
			   location-in-container-in-bits
			   repeat-entry
			   include-condition
			   time-association
			   ancillary-data-set) obj
  (cxml:with-element* ("xtce" "ParameterRefEntry")
	(cxml:attribute "parameterRef" parameter-ref)
	(marshall location-in-container-in-bits)
	(marshall repeat-entry)
	(marshall include-condition)
	(marshall time-association)
	(marshall ancillary-data-set))))

(defclass rate-in-stream () ((stream-ref :initarg :stream-ref
										 :reader stream-ref)
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

(defmethod marshall ((obj rate-in-stream))
  (with-slots (stream-ref basis minimum-value maximum-value) obj
	(cxml:attribute "streamRef" stream-ref)
	(optional-xml-attribute "basis" basis)
	(optional-xml-attribute "minimumValue" minimum-value)
	(optional-xml-attribute "maximumValue" maximum-value)))

(defclass parameter-segment-ref-entry (entry) ((parameter-ref :initarg :parameter-ref :type symbol :reader :entry)
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

(defmethod marshall ((obj parameter-segment-ref-entry))
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
	  (marshall location-in-container-in-bits)
	  (marshall repeat-entry)
	  (marshall include-condition)
	  (marshall time-association)
	  (marshall ancillary-data-set))))

(defclass container-ref-entry (entry)
  ((container-ref :initarg :container-ref :type symbol :reader ref)
   (short-description :initarg :short-description :type string)
   (location-in-container-in-bits :initarg :location-in-container-in-bits :type location-in-container-in-bits)
   (repeat-entry :initarg :repeat-entry :type repeat-entry)
   (include-condition :initarg :include-condition :type include-condition)
   (time-association :initarg :time-association :type time-association)
   (ancillary-data-set :initarg :ancillary-data-set :type ancillary-data-set)))

(defclass container-ref () ((container-ref :initarg :container-ref :type symbol :reader container-ref)))

(defun make-container-ref (container-ref)
  (check-type container-ref symbol)
  (make-instance 'container-ref :container-ref container-ref))

(defmethod marshall ((obj container-ref))
  (with-slots (container-ref) obj
	(cxml:with-element* ("xtce" "ContainerRef") 
	  (cxml:attribute "containerRef" container-ref))))

(defclass service-ref () ())

(defclass stream-ref () ())

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

(defmethod print-object ((obj entry) stream)
  (print-unreadable-object (obj stream :type t)
    (format stream "reference: ~a" (ref obj))))

(defmethod marshall ((obj container-ref-entry))
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
	  (marshall location-in-container-in-bits)
	  (marshall repeat-entry)
	  (marshall include-condition)
	  (marshall time-association)
	  (marshall ancillary-data-set))))


(defclass container-segment-ref-entry (entry) ((container-ref :initarg :container-ref :type symbol :reader ref)
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

(defmethod marshall ((obj container-segment-ref-entry))
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
	  (marshall location-in-container-in-bits)
	  (marshall repeat-entry)
	  (marshall include-condition)
	  (marshall time-association)
	  (marshall ancillary-data-set))))

(defclass stream-segment-entry (entry) ((stream-ref :initarg :stream-ref :type symbol :reader ref)
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

(defmethod marshall ((obj stream-segment-entry))
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
	  (marshall location-in-container-in-bits)
	  (marshall repeat-entry)
	  (marshall include-condition)
	  (marshall time-association)
	  (marshall ancillary-data-set))))

(defclass indirect-parameter-ref-entry (entry) ((short-description :initarg :short-description :type string)
												(alias-name-space :initarg :alias-name-space)
												(location-in-container-in-bits :initarg :location-in-container-in-bits
																			   :type location-in-container-in-bits)
												(repeat-entry :initarg :repeat-entry :type repeat-entry)
												(include-condition :initarg :include-condition :type include-condition)
												(time-association :initarg :time-association :type time-association)
												(ancillary-data-set :initarg :ancillary-data-set :type ancillary-data-set)
												(parameter-instance :initarg :parameter-ref :type parameter-instance :reader ref)))

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

(defmethod marshall ((obj indirect-parameter-ref-entry))
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
	  (marshall parameter-instance)
	  (optional-xml-attribute "aliasNameSpace" alias-name-space)
	  (optional-xml-attribute "shortDescription" short-description)
	  (optional-xml-attribute order order)
	  (marshall location-in-container-in-bits)
	  (marshall repeat-entry)
	  (marshall include-condition)
	  (marshall time-association)
	  (marshall ancillary-data-set))))

(defclass array-parameter-ref-entry (entry) ((parameter-ref :initarg :parameter-ref :type symbol :reader ref)
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

(defmethod marshall ((obj array-parameter-ref-entry))
  (with-slots (parameter-ref
			   dimension-list
			   short-description
			   location-in-container-in-bits
			   repeat-entry
			   include-condition
			   time-association
			   ancillary-data-set) obj
	(cxml:with-element* ("xtce" "ArrayParameterRefEntry")
	  (cxml:attribute "parameterRef"  parameter-ref)
	  (marshall dimension-list)
	  (optional-xml-attribute "shortDescription" short-description)
	  (marshall location-in-container-in-bits)
	  (marshall repeat-entry)
	  (marshall include-condition)
	  (marshall time-association)
	  (marshall ancillary-data-set))))

(defclass location-in-container-in-bits () ((reference-location :initarg :reference-location)
											(value :initarg :value :type value)))

(defun make-location-in-container-in-bits (value &key reference-location)
  (make-instance 'location-in-container-in-bits :value value :reference-location reference-location))

(defmethod marshall ((obj location-in-container-in-bits))
  	(with-slots (value reference-location) obj
	  (cxml:with-element* ("xtce" "LocationInContainerBits")
		(cxml:attribute "referenceLocation" reference-location)
		(marshall value))))

(defclass repeat-entry () ((count_slot :initarg :count_slot :type count_t)
						   (offset :initarg :offset :type offset)))

(defun make-repeat-entry (count &key offset)
  (make-instance 'repeat-entry :count_slot count :offset offset))

(defmethod marshall ((obj repeat-entry))
  (with-slots (count_slot offset) obj
	(cxml:with-element* ("xtce" "RepeatEntry")
	  (marshall count_slot)
	  (marshall offset))))

(defclass count_t () ((value :initarg :value :type integer-value)))

(defun make-count (value)
  (make-instance 'count_t :value value))

(defmethod marshall ((obj count_t))
  (with-slots (value) obj
	(cxml:with-element* ("xtce" "Count")
	  (marshall value))))


(defclass offset () ((value :initarg :value :type integer-type)))

(defun make-offset (value)
  (make-instance 'offset :value value))

(defmethod marshall ((obj offset))
  (with-slots (value) obj
	(cxml:with-element* ("xtce" "Offset")
	  (marshall value))))

(defclass include-condition () ((match-criteria :initarg :match-criteria :type match-criteria)))

(defun make-include-condition (match-criteria)
  (make-instance 'include-condition :match-criteria match-criteria))

(defmethod marshall ((obj include-condition))
  (with-slots (match-criteria) obj
	(cxml:with-element* ("xtce" "IncludeCondition")
	  (marshall match-criteria))))

(defclass base-container () ((container-ref :initarg :container-ref)
							 (restriction-criteria :initarg :restriction-criteria :type restriction-criteria)))

(defun make-base-container (container-ref &key restriction-criteria)
  (make-instance 'base-container :container-ref container-ref :restriction-criteria restriction-criteria))

(defmethod cxml-marshal ((obj base-container))
  (with-slots (container-ref base-container) obj
	(cxml:with-element* ("xtce" "base-container")
	  (marshall base-container))))

(defclass restriction-criteria () ((restriction-criteria-type :initarg :restriction-criteria-type)
								   (next-container :initarg :next-container :type next-container)))

(defun make-restriction-criteria (restriction-criteria-type &key next-container)
  (make-instance 'restriction-criteria :restriction-criteria-type restriction-criteria-type :next-container next-container))

(defmethod cxml-marshal ((obj restriction-criteria))
  (with-slots (restriction-criteria next-container) obj
	(cxml:with-element* ("xtce" "RestrictionCriteria")
	  (marshall restriction-criteria next-container))))

(defclass next-container () ((container-ref :initarg :container-ref)))

(defun make-next-container (container-ref)
  (make-instance 'next-container :container-ref container-ref))

(defmethod cxml-marshal ((obj next-container))
  (with-slots (container-ref) obj
	(cxml:with-element* ("xtce" "NextContainer")
	  (cxml:attribute "nextContainer" container-ref))))

(defclass dynamic-value ()
  ((parameter-instance-ref :initarg :parameter-instance-ref
						   :type parameter-instance-ref)
   (linear-adjustment :initarg :linear-adjustment
					  :type linear-adjustment)))

(defun make-dynamic-value (parameter-instance-ref &key linear-adjustment)
  (make-instance 'dynamic-value :parameter-instance-ref parameter-instance-ref :linear-adjustment linear-adjustment))

(defun resolve-dynamic-get-size (dynamic-value alist db-connection)
  (with-slots (parameter-instance-ref linear-adjustment) dynamic-value
	(with-slots (parameter-reference instance use-calibrated-value) parameter-instance-ref
	  ;(print "WOOOOOOO")
	  ;(print alist)
	  ;(print parameter-reference)
	  ;(print (format nil "~%"))
	  (log:debug parameter-reference alist )
	  (cond
		((= instance 0)
		 (assert (listp alist))
		 (assert (assoc parameter-reference alist))
		 (let ((value (cdr (assoc parameter-reference alist))))
		   (if linear-adjustment
			   (with-slots (slope intercept ) linear-adjustment
				 (log:debug (+ (* slope value) intercept) slope intercept value)
				 (+ (* slope value) intercept))
			   value)))
		
		((< instance 0)
		 (assert db-connection)
		 (assert nil () "Not implemented!!")))))
  )

(defun resolve-get-size (obj &key alist db-connection)
  (with-slots (size) obj
  (typecase size
	(dynamic-value
	 (resolve-dynamic-get-size size alist db-connection))
	(t (get-size size)))))

(defmethod marshall ((obj dynamic-value))
  (with-slots (instance-ref linear-adjustment) obj
	(cxml:with-element* ("xtce" "DynamicValue")
	  (marshall instance-ref)
	  (marshall linear-adjustment))))

(defclass argument-instance-ref ()
  ((argument-ref :initarg :argument-ref)
   (use-calibrated-value :initarg :use-calibrated-value
                         :type boole)))

(defun make-argument-instance-ref (argument-ref use-calibrated-value)
  (make-instance 'argument-instance-ref :argument-ref argument-ref :use-calibrated-value use-calibrated-value))

(defmethod marshall ((obj argument-instance-ref))
  (with-slots (argument-ref use-calibrated-value) obj
	(cxml:with-element* ("xtce" "ArgumentInstanceRef") obj
	  (cxml:attribute "argumentRef" argument-ref)
	  (optional-xml-attribute "useCalibratedValue" use-calibrated-value))))

(defclass parameter-instance-ref ()
  ((parameter-reference :initarg :parameter-ref)
   (instance :initarg :instance)
   (use-calibrated-value :initarg :use-calibrated-value)))

(defun make-parameter-instance-ref (parameter-ref &key (instance 0) (use-calibrated-value 'True))
  (make-instance 'parameter-instance-ref :parameter-ref parameter-ref :instance instance :use-calibrated-value use-calibrated-value))

(defmethod marshall ((obj parameter-instance-ref))
  (with-slots (parameter-reference instance use-calibrated-value) obj
	(cxml:with-element* ("xtce" "ParameterInstanceRef") obj
	  (cxml:attribute "parameterRef" parameter-reference)
	  (optional-xml-attribute "instance" instance)
	  (optional-xml-attribute "useCalibratedValue" use-calibrated-value))))

(defclass discrete-lookup () ())

(deftype discrete-lookup-list ()
  `(satisfies discrete-lookup-list-p))

;;TODO: Turn this into a class
;; (defmethod get-size ((obj discrete-lookup-list))
;;   (assert nil () "Not implemented!"))

(defun discrete-lookup-list-p (l)
  (and (listp l)
	   (every #'(lambda (i) (typep i 'discrete-lookup)) l)))

(defclass fixed-value ()
  ((value :initarg :value
		  :reader size)))

(defun make-fixed-value (value)
  (make-instance 'fixed-value :value value))

(defmethod get-size ((obj fixed-value))
  (size obj))

(defmethod marshall ((obj fixed-value))
  (with-slots (value) obj
	(cxml:with-element* ("xtce" "FixedValue")
	  (cxml:text (format nil "~A" value)))))

(defclass linear-adjustment ()
  ((slope :initarg :slope :type float)
   (intercept :initarg :intercept :type float)))

(defun make-linear-adjustment (&key slope intercept)
  (make-instance 'linear-adjustment :slope slope :intercept intercept))

(defmethod marshall ((obj linear-adjustment))
  (with-slots (slope intercept) obj
	(cxml:with-element* ("xtce" "LinearAdjustment")
	  (cxml:attribute "slope" slope)
	  (cxml:attribute "intercept" intercept))))

(defclass termination-char (size-in-bits)
  ((termination-char :initarg :termination-char)))

(defun make-termination-char (termination-char)
  (make-instance 'termination-char :termination-char termination-char))

(defmethod marshall ((obj termination-char))
  (with-slots (termination-char) obj
	(cxml:with-element* ("xtce" "TerminationChar")
	  (cxml:text termination-char))))

;Fixed type for string sizes. This is extra wonky, it just holds a fixed-value type
(defclass fixed () ((fixed-value :initarg :fixed-value :type fixed-value)))

(defun make-fixed (fixed-value)
  (check-type fixed-value fixed-value)
  (make-instance 'fixed :fixed-value fixed-value))

(defgeneric get-size (obj))

(defclass size-in-bits () ((size :initarg :size
								 :reader size)))

(defun make-size-in-bits (size)
  ;termination-char, fixed, and leading-size are used for string encodings, yes it's weird blame the spec.
  (check-type size (or fixed-value dynamic-value discrete-lookup-list termination-char leading-size fixed))
  (make-instance 'size-in-bits :size size))

(defmethod marshall ((obj size-in-bits))
  (with-slots (size) obj
	(cxml:with-element* ("xtce" "SizeInBits") obj
	  (marshall size))))

(defclass leading-size ()
  ((size-in-bits-of-size-tag :initarg size-in-bits-of-size-tag
							 :type positive-integer
							 :reader size)))

(defmethod get-size ((obj leading-size))
	(size obj))

(defun make-leading-size (&optional (size-in-bits-of-size-tag 16))
  (make-instance 'leading-size :size-in-bits-of-size-tag size-in-bits-of-size-tag))

(defmethod marshall ((obj leading-size))
  (with-slots (size-in-bits-of-size-tag) obj
	  (cxml:with-element* ("xtce" "LeadingSize")
		(cxml:attribute "size-in-bits-of-size-tag" size-in-bits-of-size-tag))))

(defclass parameter-type ()
  ((hort-description :initarg :short-description :type string)
   (name :initarg :name :type symbol)
   (long-description :initarg :long-description :type long-description)
   (alias-set :initarg :alias-set :type alias-set)
   (ancillary-data-set :initarg :ancillary-data-set :type ancillary-data-set)))
 
(deftype parameter-type-set-p ()
  `(satisfies parameter-type-set-p))

(defun parameter-type-set-p (l)
  (and (listp l)
	   (every #'(lambda (i) (typep i 'parameter-type)) l)))

(defclass size-range-in-characters () ((min-inclusive :initarg :min-inclusive)
									   (max-inclusive :initarg :max-inclusive)))

(defun make-size-range-in-characters (min-inclusive max-inclusive)
  (make-instance 'size-range-in-characters :min-inclusive min-inclusive
										   :max-inclusive max-inclusive))

(defmethod marshall ((obj size-range-in-characters))
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
   (unit-set :initarg :unit-set :type unit-set)
   (data-encoding :initarg :data-encoding
				  :type encoding
				  :accessor data-encoding)
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

(defmethod marshall ((obj string-parameter-type))
  (with-slots (short-description name base-type inital-value restriction-pattern
			   character-width long-description alias-set ancillary-data-set unit-set
			   encoding size-range-in-characters default-alarm context-alarm-list) obj
	(cxml:with-element* ("xtce" "StringParameterType")
	  (cxml:attribute "shortDescription" short-description)
	  (cxml:attribute "name" name)
	  (cxml:attribute "initalValue" inital-value)
	  (cxml:attribute "restrictionPattern" restriction-pattern)
	  (cxml:attribute "characterWidth" character-width)
	  (marshall long-description)
	  (marshall alias-set)
	  (marshall ancillary-data-set)
	  (marshall unit-set)
	  (marshall encoding)
	  (marshall size-range-in-characters)
	  (marshall default-alarm)
	  (marshall context-alarm-list))))

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

(defmethod marshall ((obj float-parameter-type))
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
	  (marshall long-description)
	  (marshall alias-set)
	  (marshall ancillary-data-set)
	  (marshall data-encoding)
	  (marshall to-string)
	  (marshall valid-range)
	  (marshall default-alarm)
	  (marshall context-alarm-list)
	  (marshall unit-set))))

(defclass boolean-parameter-type (parameter-type)
  ((short-description :initarg :short-description
                      :type string)
   (name :initarg :name
         :type string)
   (base-type :initarg :base-type)
   (initial-value :initarg :initial-value
                  :type integer)
   (one-string-value :initarg :one-string-value
					 :type string)
   (zero-string-value :initarg :zero-string-value
					  :type string)
   (long-description :initarg :long-description
                     :type string)
   (alias-set :initarg :alias-set
              :type alias-set)
   (ancillary-data-set :initarg :ancillary-data-set
                        :type ancillary-data-set)
   (unit-set :initarg :unit-set
             :type unit-set)
   (data-encoding :initarg :data-encoding
                  :type encoding
				  :reader data-encoding)
   (default-alarm :initarg :default-alarm
                  :type alarm)
   (context-alarm-list :initarg :context-alarm-list
                       :type context-alarm-list)))

(defun make-boolean-parameter-type (name
									&key
									  data-encoding
									  short-description
									  base-type
									  initial-value
									  (one-string-value "True")
									  (zero-string-value "False")
									  long-description
									  alias-set
									  ancillary-data-set
									  unit-set
									  default-alarm
									  context-alarm-list)
  (make-instance 'boolean-parameter-type :name name
										 :short-description short-description
										 :base-type base-type
										 :initial-value initial-value
										 :one-string-value one-string-value
										 :zero-string-value zero-string-value
										 :long-description long-description
										 :alias-set alias-set
										 :ancillary-data-set ancillary-data-set
										 :unit-set unit-set
										 :data-encoding data-encoding
										 :default-alarm default-alarm
										 :context-alarm-list context-alarm-list))

(defmethod marshall ((obj boolean-parameter-type))
  (with-slots (short-description
			   name
			   base-type
			   initial-value
			   one-string-value
			   zero-string-value
			   long-description
			   alias-set
			   ancillary-data-set
			   unit-set
			   data-encoding
			   default-alarm
			   context-alarm-list) obj
	(cxml:with-element* ("xtce" "BooleanParameterType")
	  (cxml:attribute "name" name)
	  (optional-xml-attribute "shortDescription" short-description)
	  (optional-xml-attribute "baseType" base-type)
	  (optional-xml-attribute "initialValue" initial-value)
	  (cxml:attribute "oneStringValue" one-string-value)
	  (cxml:attribute "zeroStringValue" zero-string-value)
	  (marshall unit-set)
	  (marshall long-description)
	  (marshall alias-set)
	  (marshall ancillary-data-set)
	  (marshall data-encoding)
	  (marshall default-alarm)
	  (marshall context-alarm-list))))

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
                  :type encoding
				  :reader data-encoding)
   (to-string :initarg :to-string)
   (valid-range :initarg :valid-range)
   (default-alarm :initarg :default-alarm
                  :type alarm)
   (context-alarm-list :initarg :context-alarm-list
                       :type context-alarm-list)))

(defmethod marshall ((obj integer-parameter-type))
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
      (cxml:attribute "name" name)
      (optional-xml-attribute "baseType" base-type)
      (optional-xml-attribute "initialValue" initial-value)
      (optional-xml-attribute"sizeInBits" size-in-bits)
      (cxml:attribute "signed" signed)
      (marshall long-description)
      (marshall alias-set)
	  (marshall unit-set)
      (marshall data-encoding)
      (marshall to-string)
      (marshall valid-range)
      (marshall default-alarm)
      (marshall context-alarm-list))))

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
   (name :initarg :name :type symbol)
   (initial-value :initarg :initial-value)
   (base-type :initarg :base-type)
   (data-encoding :initarg :data-encoding
				  :type data-encoding
				  :reader data-encoding)
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

(defmethod marshall ((obj binary-parameter-type))
  (with-slots (name short-description base-type initial-value long-description alias-set ancillary-data-set unit-set data-encoding default-alarm binary-context-alarm-list) obj
	(cxml:with-element* ("xtce" "BinaryParameterType")
	  (cxml:attribute "name" name)
	  (cxml:attribute "shortDescription" short-description)
	  (cxml:attribute "baseType" base-type)
	  (cxml:attribute "initialValue" initial-value)
	  (marshall long-description)
	  (marshall alias-set)
	  (marshall ancillary-data-set)
	  (marshall unit-set)
	  (marshall data-encoding)
	  (marshall default-alarm)
	  (marshall binary-context-alarm-list))))

(defclass enumerated-parameter-type (parameter-type)
  (
   (short-description :initarg :short-description :type string)
   (name :initarg :name :type string)
   (initial-value :initarg :initial-value)
   (base-type :initarg :base-type)
   (data-encoding :initarg :data-encoding
				  :type data-encoding
				  :reader data-encoding)
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

(defmethod marshall ((obj enumeration))
  (with-slots (value label max-value short-description) obj
	(cxml:with-element* ("xtce" "Enumeration")
	  (cxml:attribute "label" (format nil "~A" label))
	  (cxml:attribute "value" (format nil "~A" value))
	  (if max-value (cxml:attribute "maxValue" max-value))
	  (if short-description (cxml:attribute "shortDescription" short-description)))))

(defmethod marshall ((obj enumerated-parameter-type))
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
	  (optional-xml-attribute "shortDescription" short-description)
	  (optional-xml-attribute "baseType" base-type)
	  (optional-xml-attribute "initialValue" initial-value)
	  (marshall data-encoding)
	  (marshall long-description)
	  (marshall alias-set)
	  (marshall unit-set)
	  (marshall enumeration-list)
	  (marshall default-alarm)
	  (marshall context-alarm-list))))

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

(defmethod marshall ((obj absolute-time-parameter))
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
	  (marshall long-description)
	  (marshall alias-set)
	  (marshall ancillary-data-set)
	  (marshall encoding)
	  (marshall reference-time))))

(defclass offset-from () ((parameter-ref :initarg :parameter-ref)))

(defun make-offset-from (parameter-ref)
  (make-instance 'offset-from :parameter-ref parameter-ref))

(defmethod marshall ((obj offset-from))
  (with-slots (parameter-ref) obj
	(cxml:with-element* ("xtce" "OffsetFrom")
	  (cxml:attribute "parameterRef" parameter-ref))))

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

(defmethod marshall ((obj epoch))
  (with-slots (epoch-value) obj
	(cxml:with-element* ("xtce" "Epoch")
	  (cxml:text (format nil "~A" epoch-value)))))

(defclass reference-time () ((reference :initarg :reference )))

(defmethod marshall ((obj reference-time))
  (with-slots (reference) obj
	(cxml:with-element* ("xtce" "ReferenceTime")
	  (marshall reference))))

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

(defmethod marshall ((obj encoding))
  (with-slots (units scale offset data-encoding reference-time) obj
	(cxml:with-element* ("xtce" "Encoding") 
	  (optional-xml-attribute "units" units)
	  (optional-xml-attribute "scale" scale)
	  (optional-xml-attribute "offset" offset)
	  (marshall data-encoding))))


										;TODO: Encoding parameters may not accept all data encodings 
(defclass parameter ()
  ((name :initarg :name :type symbol)
   (parameter-type-ref :initarg :parameter-type-ref :type symbol :reader ref)
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
  (assert (not (equal name parameter-type-ref)) () "Error instantiating parameter ~A. The name of this parameter and its parameter-type-reference are the same. This would cause an immediate circular reference." name)
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

(defmethod marshall ((obj parameter))
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
	  (marshall long-description)
	  (marshall alias-set)
	  (marshall ancillary-data-set)
	  (marshall parameter-properties))))

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

(defmethod marshall ((obj parameter-properties))
  (with-slots (data-source read-only persistence system-name validity-condition physical-address-set time-association) obj
	(cxml:with-element* ("xtce" "ParameterProperties")
	  (optional-xml-attribute "dataSource" data-source)
	  (unless (equal read-only :null)
		(optional-xml-attribute "readOnly" read-only))
	  (optional-xml-attribute "persistence" persistence)
	  (marshall system-name)
	  (marshall validity-condition)
	  (marshall physical-address-set)
	  (marshall time-association))))


(defclass system-name () ())

(defclass validity-condition ()())

(defclass physical-address-set () ())

(defclass time-association () ())


(defclass data-encoding () ())

(defun valid-integer-encoding-p (enc)
  (let ((enc (intern (symbol-name enc) :xtce)))
	(assert (member enc '(unsigned sign-magnitude twos-complement ones-compliment bcd packed-bcd)) (enc)
			"Int Encoding ~A is not <'unsigned | 'twos-complement | 'ones-compliment | 'bcd | 'packed-bcd" enc) 
	t))

(deftype integer-encoding ()
  '(satisfies valid-integer-encoding-p))

(defclass integer-data-encoding (data-encoding)
  ((integer-encoding :documentation "Specifies integer numeric value to raw encoding method."
					 :initarg :integer-encoding)
   (size-in-bits :documentation "Number of bits to use for the raw encoding."
                 :initarg :size-in-bits :type integer)
   (change-threshold :documentation "Error detection algorithm"
                     :initarg :change-threshold)
   (default-calibrator :documentation "TODO"
                       :initarg :default-calibrator)
   (context-calibrator-list :documentation "TODO"
                            :initarg :context-calibrator-list)))

(defun make-integer-data-encoding (&key (size-in-bits 8)
                                     (integer-encoding 'UNSIGNED)
                                     (change-threshold nil)
                                     (default-calibrator nil)
                                     (context-calibrator-list nil))
  (valid-integer-encoding-p integer-encoding)
  (check-type size-in-bits integer)
  (assert (plusp size-in-bits) (size-in-bits) "size-in-bits ~A must be a positive integer" size-in-bits)
  (make-instance 'integer-data-encoding
                 :size-in-bits size-in-bits
                 :integer-encoding integer-encoding
                 :change-threshold change-threshold
                 :context-calibrator-list context-calibrator-list
                 :default-calibrator default-calibrator))

(defmethod marshall ((obj integer-data-encoding))
  (with-slots (size-in-bits integer-encoding change-threshold default-calibrator context-calibrator-list) obj
    (cxml:with-element* ("xtce" "IntegerDataEncodingType")
      (optional-xml-attribute "encoding" integer-encoding)
      (optional-xml-attribute "sizeInBits" size-in-bits)
      (optional-xml-attribute "changeThreshold" change-threshold)
      (marshall default-calibrator)
      (marshall context-calibrator-list))))

(deftype string-size ()
  `(satisfies string-size-p))

(defun string-size-p (a)
  (or (and (typep a 'size-in-bits)
		   (typep (slot-value a 'size) '(or fixed termination-char leading-size)))
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

(defun make-string-data-encoding (size-type &optional
											  (bit-order '|mostSignificantBitFirst|)
											  (byte-order '|mostSignificantByteFirst|)
											  (string-encoding 'UTF-8)
											  (error-detect-correct))
  (check-type size-type string-size)
  (make-instance 'string-data-encoding
				 :string-size-type size-type
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

(defmethod marshall ((obj binary-data-encoding))
  (with-slots (size-in-bits bit-order byte-order error-detect-correct from-binary-transform-algorithm to-binary-transform-algorithm) obj
	(cxml:with-element* ("xtce" "BinaryDataEncoding")
	  (optional-xml-attribute "bitOrder" bit-order)
	  (optional-xml-attribute "byteOrder" byte-order)
	  (marshall error-detect-correct)
	  (marshall size-in-bits)
	  (marshall from-binary-transform-algorithm)
	  (marshall to-binary-transform-algorithm))))

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

(defclass default-calibrator ()
  ((ancillary-data-set :initarg :ancillary-data-set :type ancillary-data-set)
   (callibrator :initarg :numeric-calibrator :type numeric-calibrator)))

(defun make-default-calibrator (numeric-calibrator &key ancillary-data-set)
  (make-instance 'default-calibrator
				 :numeric-calibrator numeric-calibrator
				 :ancillary-data-set ancillary-data-set))


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

(defmethod marshall ((obj comparison))
  (with-slots (parameter-ref value instance use-calibrated-value comparison-operator) obj
	(cxml:with-element* ("xtce" "Comparison")
	  (cxml:attribute "parameterRef" parameter-ref)
	  (cxml:attribute "value" value)
	  (optional-xml-attribute "instance" instance)
	  (optional-xml-attribute "useCalibratedValue" use-calibrated-value)
	  (optional-xml-attribute "comparisonOperator" comparison-operator))))

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

(defmethod marshall ((obj term))
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

(defun make-polynomial-calibrator (&key name short-description ancillary-data-set term-list)
  (check-type term-list term-list)
  (check-optional-type name string)
  (check-optional-type short-description string)
  (check-optional-type ancillary-data-set ancillary-data-set)
  (make-instance 'polynomial-calibrator :term-list term-list
                                        :name name
                                        :ancillary-data-set ancillary-data-set
                                        :short-description short-description))

(defmethod marshall ((obj polynomial-calibrator))
  (with-slots (term-list name short-description ancillary-data-set) obj
    (cxml:with-element* ("xtce" "PolynomialCalibrator")
      (if name (cxml:attribute "name" (format nil "~A" name)))
      (if short-description (cxml:attribute "shortDescription" short-description))
      (if ancillary-data-set (marshall ancillary-data-set))
      (marshall term-list))))


(defclass index () ((index-type :initarg :index-type))) ; deftype satisfies x or y or z

(defclass starting-index (index) ())

(defun make-starting-index (index-type)
  (make-instance 'starting-index :index-type index-type))

(defmethod marshall ((obj starting-index))
  (with-slots (index-type) obj
	(cxml:with-element* ("xtce" "StartingIndex")
	  (marshall index-type))))

(defclass ending-index (index) ())

(defun make-ending-index (index-type)
  (make-instance 'ending-index :index-type index-type))

(defmethod marshall ((obj ending-index))
  (with-slots (index-type) obj
	(cxml:with-element* ("xtce" "EndingIndex")
	  (marshall index-type))))


(defclass dimension () ((starting-index :initarg :starting-index :type starting-index)
						(ending-index :initarg :ending-index :type ending-index)))

(defun make-dimension (starting-index ending-index)
  (make-instance 'dimension :starting-index starting-index :ending-index ending-index))

(defmethod marshall ((obj dimension))
  (with-slots (starting-index ending-index) obj
	(cxml:with-element* ("xtce" "Dimension")
	  (marshall starting-index)
	  (marshall ending-index))))

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


(defmethod marshall ((obj array-parameter-type))
  (with-slots (name array-type-ref short-description long-description alias-set ancillary-data-set dimension-list) obj
	  (cxml:with-element* ("xtce" "ArrayParameterType")
		(cxml:attribute "name" name)
		(cxml:attribute "shortDescription" short-description)
		(cxml:attribute "arrayTypeRef" array-type-ref)
		(marshall dimension-list))))

(defmethod marshall ((obj string-parameter-type))
  (with-slots (short-description name base-type inital-value restriction-pattern
			   character-width long-description alias-set ancillary-data-set unit-set
			   data-encoding size-range-in-characters default-alarm context-alarm-list) obj
	(cxml:with-element* ("xtce" "StringParameterType")
	  (cxml:attribute "shortDescription" short-description)
	  (cxml:attribute "name" name)
	  (cxml:attribute "initalValue" inital-value)
	  (cxml:attribute "restrictionPattern" restriction-pattern)
	  (cxml:attribute "characterWidth" character-width)
	  (marshall long-description)
	  (marshall alias-set)
	  (marshall ancillary-data-set)
	  (marshall unit-set)
	  (marshall data-encoding)
	  (marshall size-range-in-characters)
	  (marshall default-alarm)
	  (marshall context-alarm-list))))

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
  (and
   (not (null l))
   (listp l)
   (every #'(lambda (i) (typep i '(or fixed-frame-stream variable-frame-stream custom-stream))) l )))

(defclass variable-frame-stream (data-stream) ())

(defclass custom-stream (data-stream) ())

(defclass fixed-frame-stream (data-stream)
  ((name :initarg :name :type symbol :reader name)
   (short-description :initarg :short-description :type short-description :reader short-description)
   (long-description :initarg :long-description :type string :reader long-description)
   (alias-set :initarg :alias-set :type alias-set :reader alias-set)
   (ancillary-data-set :initarg :ancillary-data-set :type ancillary-data-set :reader ancillary-data-set)
   (bit-rate-in-bps :initarg :bit-rate-in-bips :reader bit-rate-in-bps)
   (pcm-type :initarg :pcm-type :reader pcm-type)
   (inverted :initarg :inverted :type boole :reader inverted)
   (sync-aperture-in-bits :initarg :sync-aperture-in-bits :type sync-aperture-in-bits :reader sync-aperture-in-bits)
   (frame-length-in-bits :initarg :frame-length-in-bits :type positive-integer :reader frame-length-in-bits)
   (next-ref :initarg :next-ref :type symbol :reader next-ref)
   (sync-strategy :initarg :sync-strategy :type sync-strategy :reader sync-strategy)
   (stream-ref :initarg :stream-ref :type stream-ref :reader stream-ref)))

(defun make-fixed-frame-stream (name frame-length-in-bits next-ref sync-strategy
								&key
								  short-description
								  bit-rate-in-bps
								  (pcm-type 'NRZL)
								  inverted
								  (sync-aperture-in-bits 0)
								  long-description
								  alias-set
								  ancillary-data-set
								  stream-ref)
  "For streams that contain a series of frames with a fixed frame length where the frames are found by looking for a marker in the data. This marker is sometimes called the frame sync pattern and sometimes the Asynchronous Sync Marker (ASM). This marker need not be contiguous although it usually is."
  (check-type next-ref (or container-ref service-ref))
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
			:ancillary-data-set ancillary-data-set
			:stream-ref stream-ref))

(defmethod print-object ((obj fixed-frame-stream) stream)
  (print-unreadable-object (obj stream :type t)
    (with-slots (name short-description) obj
      (format stream "name: ~a, description: ~a" name short-description))))

(defmethod print-object ((obj container-ref) stream)
  (print-unreadable-object (obj stream :type t)
    (with-slots (container-ref) obj
      (format stream "ref: ~a" container-ref))))

(deftype next-ref ()
  '(satisfies next-ref-p))

(defun next-ref-p (ref)
  (and (typep ref 'container-ref) (typep ref 'service-ref)))

(defmethod marshall((obj fixed-frame-stream))
  (with-slots (name short-description long-description alias-set ancillary-data-set bit-rate-in-bps pcm-type inverted sync-aperture-in-bits frame-length-in-bits next-ref sync-strategy stream-ref) obj
	(cxml:with-element* ("xtce" "FixedFrameStream")
	  (cxml:attribute "name" name)
	  (cxml:attribute "frameLengthInBits" frame-length-in-bits)
	  (optional-xml-attribute "shortDescription" short-description)
	  (optional-xml-attribute "bitRateinBPS" bit-rate-in-bps)
	  (optional-xml-attribute "pcmType" pcm-type)
	  (optional-xml-attribute "inverted" inverted)
	  (optional-xml-attribute "syncApertureInBits" sync-aperture-in-bits)
	  (marshall long-description)
	  (marshall alias-set)
	  (marshall next-ref)
	  (marshall stream-ref)
	  (marshall sync-strategy))))

(defclass sync-strategy () ((auto-invert :initarg :auto-invert :type auto-invert)
							(sync-pattern :initarg :sync-pattern :type sync-pattern :reader sync-pattern)
							(verify-to-lock-good-frames :initarg :verify-to-lock-good-frames :type postiive-integer :reader verify-to-lock-good-frames)
							(check-to-lock-good-frames :initarg :check-to-lock-good-frames :type postiive-integer :reader check-to-lock-good-frames)
							(max-bit-errors-in-sync-pattern :initarg :max-bit-errors-in-sync-pattern :type postiive-integer :reader max-bit-errors-in-sync-pattern)))

(defun make-sync-strategy (sync-pattern &key (verify-to-lock-good-frames 4) (check-to-lock-good-frames 1) (max-bit-errors-in-sync-pattern 0) auto-invert)
  "CCSDS: A Sync Strategy specifies the strategy on how to find frames within a stream of PCM data. The sync strategy is based upon a state machine that begins in the 'Search' state until the first sync marker is found. Then it goes into the 'Verify' state until a specified number of successive good sync markers are found. Then, the state machine goes into the 'Lock' state, in the 'Lock' state frames are considered good. Should a sync marker be missed in the 'Lock' state, the state machine will transition into the 'Check' state, if the next sync marker is where it's expected within a specified number of frames, then the state machine will transition back to the 'Lock' state, it not it will transition back to 'Search'"
  (check-type sync-pattern sync-pattern)
  (make-instance 'sync-strategy
				 :auto-invert auto-invert
				 :sync-pattern sync-pattern
				 :verify-to-lock-good-frames verify-to-lock-good-frames
				 :check-to-lock-good-frames check-to-lock-good-frames
				 :max-bit-errors-in-sync-pattern max-bit-errors-in-sync-pattern))

(defmethod marshall ((obj sync-strategy))
  (with-slots (auto-invert sync-pattern verify-to-lock-good-frames check-to-lock-good-frames max-bit-errors-in-sync-pattern) obj
	(cxml:with-element* ("xtce" "SyncStrategy")
	  (optional-xml-attribute "autoInvert" auto-invert)
	  (optional-xml-attribute "verifyToLockGoodFrames" verify-to-lock-good-frames)
	  (optional-xml-attribute "checkTypLockGoodFrames" check-to-lock-good-frames)
	  (optional-xml-attribute "maxBitErrorsInSyncPatter" max-bit-errors-in-sync-pattern)
	  (marshall sync-pattern))))

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

(defun make-sync-pattern (&key (pattern #x1acffc1d) (pattern-length-in-bits (hex-length-in-bits pattern)) mask mask-length-in-bits (bit-location-from-start 0))
  "CCSDS: The pattern of bits used to look for frame synchronization. See SyncPatternType.
   Bifrost: Define a synchronization pattern and masks. Used as metadata to search the synchronization markers of fixed frames."
  (make-instance 'sync-pattern
				 :pattern pattern
				 :pattern-length-in-bits pattern-length-in-bits
				 :bit-location-from-start bit-location-from-start
				 :mask mask
				 :mask-length-in-bits mask-length-in-bits))

(defmethod marshall ((obj sync-pattern))
  (with-slots (pattern pattern-length-in-bits bit-location-from-start mask mask-length-in-bits) obj
	  (cxml:with-element* ("xtce" "SyncPattern")
		(cxml:attribute "pattern" pattern)
		(cxml:attribute "patternLengthInBits" pattern-length-in-bits)
		(optional-xml-attribute "mask" mask)
		(optional-xml-attribute "maskLengthInBits" mask-length-in-bits)
		(optional-xml-attribute "bitLocationFromStartOfContainer" bit-location-from-start))))

(defgeneric instantiate-parameter (parameter-type data)
  (:documentation "Instantiate a parameter given a byte"))

(defmethod instantiate-parameter ((parameter enumerated-parameter-type) data)
  (assoc data (slot-value parameter 'alist)))

(deftype ancillary-data-set ()
  `(satisfies ancillary-data-set-p))

(defun ancillary-data-set-p (l)
  (and (listp l)
	   (every #'(lambda (i) (typep i 'ancillary-data)) l )))

(defclass ancillary-data () ((name :initarg :name :reader :name :type symbol)
							 (value :initarg :value :reader :value)
							 (mime-type :initarg :mime-type :reader :mime-type :type string)
							 (href :initarg :href :reader :href :type string)))

(defun make-ancillary-data (name value &key mime-type href)
  (make-instance 'ancillary-data :name name :value value :mime-type mime-type :href href))

(defmethod marshall ((obj ancillary-data))
  (with-slots (name value mime-type href) obj
	(cxml:with-element* ("xtce" "AncillaryData")
	  (cxml:attribute "name" name)
	  (cxml:attribute "value" value)
	  (optional-xml-attribute "mimeType" mime-type)
	  (optional-xml-attribute "href" href))))

(defmethod marshall ((obj list))
  "Exclusively used for xtce-lists and xtce-sets (a homogenous list of XTCE constructs).
   These types used to be classes, but its complexity didn't offer any obvious benefits.
  "
  (let ((lname (typecase obj
				 (ancillary-data-set
				  "AncillaryDataSet")
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
				  "UnitSet")
				 (t
				  :nil)
				 )))
	(assert (not (equal lname :nil)) () "Fatal Programming Error: Could not find a deftype for the list ~A." obj)
	;The type needs to be defined and listed in the typecase above.
	(if lname
		(progn
		  (cxml:with-element* ("xtce" lname)
			(dolist (i obj)
			  (marshall i))))
		(dolist (i obj)
		  (marshall i)))))

(defmethod marshall (obj)
  (if (typep obj 'boolean)
	  (when obj "True" "False")
	  (assert nil () "Programming Error: Could not find specializer for ~A. Every XTCE element needs a marshall specializer. " obj)))

(defmethod marshall ((obj symbol))
  (when obj
	  (symbol-name obj)))

(defmethod marshall ((obj number))
  (format nil "~A" obj))

(defmethod unparse-attribute ((obj symbol))
  (format nil "~A" obj))

(define-condition dereference-mismatch (error)
  ((reference-holder :initarg :reference-holder :accessor reference-holder)
   (dereferenced-obj :initarg :dereferenced-obj :accessor dereferenced-obj))
  (:report (lambda (condition stream)
     (format stream "~A was dereferenced from ~A and is an incompatible reference. Check the reference holder.~&" (dereferenced-obj condition) (reference-holder condition)))))

(defgeneric check-dereference-mismatch (reference-holder dereferenced-object))
(defmethod check-dereference-mismatch ((reference-holder parameter-ref-entry) dereferenced-object)
  (unless (typep dereferenced-object 'parameter)
	(error 'dereference-mismatch :reference-holder reference-holder :dereferenced-obj dereferenced-object)))

(defmethod check-dereference-mismatch ((reference-holder container-ref-entry) dereferenced-object)
  (unless (typep dereferenced-object 'sequence-container)
	(error 'dereference-mismatch :reference-holder reference-holder :dereferenced-obj dereferenced-object)))


(defgeneric dereference (object-with-reference symbol-table))

(defmethod dereference ((obj container-ref) symbol-table)
  (let* ((reference (container-ref obj))
		 (res (filesystem-hash-table:find-key-by-path (symbol-name reference) symbol-table)))
	res))

(defmethod dereference ((obj xtce::container-ref-entry) symbol-table)
  (let* ((reference (xtce::ref obj))
		(res (filesystem-hash-table:find-key-by-path (symbol-name reference) symbol-table)))
	res))

(defmethod dereference ((obj xtce::parameter) symbol-table)
  (let* ((reference (xtce::ref obj))
		(res (filesystem-hash-table:find-key-by-path (symbol-name reference) symbol-table)))
	res
  ))

(defmethod dereference ((obj xtce::parameter-ref-entry) symbol-table)
  (let* ((reference (xtce::ref obj))
		(res (filesystem-hash-table:find-key-by-path (symbol-name reference) symbol-table)))
	res))

(define-condition circular-reference-found (error)
  ((visited :initarg :visited :accessor visited)
   (reference-holder :initarg :reference-holder :accessor reference-holder))
  (:report (lambda (condition stream)
     (format stream "Detected circular reference in ~A. Dereference chain: ~A~&" (reference-holder condition) (visited condition)))))

(defun dereference-with-cycle-detection (item symbol-table &optional (visited '()))
  (when (typep item '(or entry sequence-container parameter))
	(let ((dereference (dereference item symbol-table)))
	  (if (member item visited)
		  (error 'circular-reference-found :visited visited :reference-holder item)
		  (dereference-with-cycle-detection dereference symbol-table (push item visited))))))
