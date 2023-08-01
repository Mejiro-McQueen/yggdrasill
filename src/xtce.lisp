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
  (make-xtce-set 'sequence-container "ContainerSet" items))

(defclass sequence-container () ((name :initarg :name :type symbol)
								 (short-description :initarg short-description :type string)
								 (abstract :initarg :abstract)
								 (idle-patter :initarg :idle-pattern)
								 (long-description :initarg :long-description :type long-description)
								 (alias-set :initarg :alias-set :type alias-set)
								 (ancilliary-data-set :initarg :ancilliary-data-set :type ancilliary-data-set)
								 (rate-in-stream-type :initarg :rate-in-stream-type)
								 (entry-list :initarg :entry-list :type entry-list)
								 (base-container :initarg :base-container :type base-container)))

(defclass default-rate-in-stream () ((basis :initarg :basis)
									 (minimum-value :initarg :minimum-value)
									 (maximum-value :initarg :maximum-value)
									 (stream-ref :initarg :stream-ref)))

(defclass entry-list () ((parameter-ref-entry :initarg :parameter-ref-entry :type parameter-ref-entry)
						 (parameter-segment-ref-entry :initarg :parameter-segment-ref-entry :type parameter-segment-ref-entry)
						 (container-ref-entry :initarg :container-ref-entry :type container-ref-entry)
						 (container-segment-ref-entry :initarg :container-segment-ref-entry :type container-segment-ref-entry)
						 (stream-segment-entry :initarg :stream-segment-entry :type stream-segment-entry)
						 (indirect-parameter-ref-entry :initarg :indirect-parameter-ref-entry :type indirect-parameter-ref-entry)
						 (array-parameter-ref-entry :initarg :array-parameter-ref-entry :type array-parameter-ref-entry)))

(defclass parameter-ref-entry () ((parameter-ref :initarg :parameter-ref)
								  (short-description :initarg :short-description :type string)
								  (location-in-container-in-bits
								   :initarg :location-in-container-in-bits
								   :type location-in-container-in-bits)
								  (repeat-entry :initarg :repeat-entry :type repeat-entry)
								  (include-condition :initarg :include-condition :type include-condition)
								  (time-assosciation :initarg :time-association :type time-association)
								  (ancilliary-data-set :initarg :ancillary-data-set :type ancilliary-data-set)))

(defclass rate-in-stream () ((stream-ref :initarg :stream-ref)
							 (basis :initarg :basis)
							 (minimum-value :initarg :minimum-value)
							 (maximum-value :initarg :maximum-value)))

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

(defclass container-ref-entry () ((container-ref :initarg :container-ref)
								  (short-description :initarg short-description :type string)
								  (location-in-container-in-bits :initarg location-in-container-in-bits :type location-in-container-in-bits)
								  (repeat-entry :initarg :repeat-entry :type repeat-entry)
								  (include-condition :initarg :include-condition :type include-condition)
								  (time-association :initarg :time-association :type time-association)
								  (ancilliary-data-set :initarg :ancillary-data-set :type ancilliary-data-set)))


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

(defclass stream-sgment-ref-entry () ((stream-ref :initarg :stream-ref)
									  (size-in-bits :initarg :size-in-bits :type positive-integer)
									  (short-description :initarg :short-description :type string)
									  (location-in-container-in-bits :initarg :location-in-container-in-bits
																	 :type location-in-container-in-bits)
									  (repeat-entry :initarg :repeat-entry :type repeat-entry)
									  (include-condition :initarg :include-condition :type include-condition)
									  (time-association :initarg :time-association :type time-association)
									  (ancilliary-data-set :initarg :ancillary-data-set :type ancilliary-data-set)))

(defclass indirect-parameter-ref-entry () ((short-description :initarg :short-description :type string)
										   (alias-name-space :initarg :alias-name-space)
										   (location-in-container-in-bits :initarg :location-in-container-in-bits
																		  :type location-in-container-in-bits)
										   (repeat-entry :initarg :repeat-entry :type repeat-entry)
										   (include-condition :initarg :include-condition :type include-condition)
										   (time-association :initarg :time-association :type time-association)
										   (ancilliary-data-set :initarg :ancillary-data-set :type ancilliary-data-set)
										   (parameter-instance :initarg :parameter-ref :type parameter-instance)))

(defclass array-parameter-ref-entry () ((parameter-ref :initarg :parameter-ref)
										(short-description :initarg :short-description :type string)
										(location-in-container-in-bits :initarg :location-in-container-in-bits
																		  :type location-in-container-in-bits)
										(repeat-entry :initarg :repeat-entry :type repeat-entry)
										(include-condition :initarg :include-condition :type include-condition)
										(time-association :initarg :time-association :type time-association)
										(ancilliary-data-set :initarg :ancillary-data-set :type ancilliary-data-set)
										(dimension-list :initarg :dimension-list :type dimension-list)))


(defclass dimension () ((starting-index :initarg :starting-index :type starting-index)
						(ending-index :initarg :ending-index :type ending-index)))

(defclass starting-index () ((value :initarg :value)))

(defclass ending-index () ((value :initarg :value)))

(defclass location-in-container-in-bits () ((reference-location :initarg :reference-location)
											(value :initarg :value :type value)))

(defclass repeat-entry () ((count :initarg :count :type count_t)
						   (offset :initarg :offset :type offset)))
  
(defclass count_t () ((value :initarg :value :type value)))

(defclass offset () ((value :initarg :value :type value)))

(defclass include-condition () ((match :initarg :match :type match)))

(defclass base-container () ((container-ref :initarg :container-ref)
							 (restriction-criteria :initarg :restriction-criteria :type restriction-criteria)))

(defclass restriction-criteria () ((restriction :initarg :restriction)
								   (next-container :initarg :next-container :type next-container)))

(defclass next-container () ((container-ref :initarg :container-ref)))

; Note: CCSDS 660.1-G-2 typo Page 4-142, figure caption says ContainrRefEntry
