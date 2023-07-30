(in-package :xtce)

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
         :type symbol)
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
      (cxml-marshall  long-description)
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

(defclass enumerated-parameter-type (parameter-type)
  (
   (short-description :initarg :short-description :type string)
   (name :initarg :name :type symbol)
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
  (check-type name symbol)
  (check-optional-type short-description string)
  (check-optional-type base-type T)
  (check-optional-type long-description string)
  (check-optional-type alias-set alias-set)
  (check-optional-type ancillary-data-set ancillary-data-set)
  (check-optional-type unit-set unit-set)
  (check-optional-type data-encoding data-encoding)
  (check-optional-type enumeration-list enumeration-list)
  (check-optional-type default-alarm enumeration-alarm)
  (check-optional-type context-alarm-list context-alarm-list)
  (check-optional-type initial-value T)
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
  ((alarm-level :initarg :alarm-level :type symbol)
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
   (name :initarg :name :type symbol)
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
  ((name :initarg :name :type symbol)
   (parameter-type-ref :initarg :parameter-type-ref :type symbol)
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
						 ancillary-data-set)
  (make-instance
   'parameter
   :name name
   :parameter-type-ref parameter-type-ref
   :short-description short-description
   :long-description long-description
   :initial-value initial-value
   :alias-set alias-set
   :ancillary-data-set ancillary-data-set))

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
	  (cxml:attribute "name" (format-symbol name))
	  (cxml:attribute "parameterTypeRef" (format-symbol parameter-type-ref))
	  (optional-xml-attribute "shortDescription" short-description)
	  (optional-xml-attribute "initialValue" initial-value)
	  (cxml-marshall long-description)
	  (cxml-marshall alias-set)
	  (cxml-marshall ancillary-data-set)
	  (cxml-marshall parameter-type-ref))))

(defun make-parameter-set (&rest items)
  (make-xtce-set 'parameter "Parameter" items))

(defclass parameter-properties () ((data-source :initarg :data-source)
								   (read-only :initarg :read-only)
								   (persistence :initarg :persistence)
								   (system-name :initarg :system-name :type system-name)
								   (validity-condition :initarg :validity-condition :type validity-condition)
								   (physical-address-set :initarg :physical-address-set :type physical-address-set)
								   (time-assosciation :initarg :time-association :type time-association)))

(defmethod cxml-marshall ((obj parameter-properties))
  (with-slots (data-source read-only persistence system-name validity-condition physical-address-set time-association) obj
	(cxml:with-element* ("xtce" "ParameterProperties")
	  (optional-xml-attribute "dataSource" data-source)
	  (optional-xml-attribute read-only (format-bool read-only))
	  (optional-xml-attribute persistence)
	  (cxml-marshall system-name)
	  (cxml-marshall validity-condition)
	  (cxml-marshall physical-address-set)
	  (cxml-marshall time-association))))
  

(defclass system-name () ())

(defclass validity-condition ()())

(defclass physical-address-set () ())

(defclass time-association () ())
