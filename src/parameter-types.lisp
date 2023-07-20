(in-package :xtce)

(defclass parameter-type ()
  ((name :initarg :name
		 :type symbol)
   (short-description :initarg :short-description
					  :type string)
   (long-description :initarg :long-description
					 :type string)
   (alias-set :initarg :alias-set
			  :type alias-set)
   (ancillary-data-set :initarg :ancillary-data-set
					   :type ancillary-data-set)
   (unit-set :initarg :unit-set
             :type unit-set)))
   
(defclass xtce-set ()
  ((items :initarg :items
          :type list)))


(defclass parameter-type-set (xtce-set) ())
  

(defmethod cxml-marshall ((obj xtce-set))
  (with-slots (items) obj
    (dolist (i items)
      (cxml-marshall i))))

(defun make-parameter-type-set (&rest items)
  (let ((items (remove nil items))) 
    (dolist (i items) 
      (check-type i parameter-type)))
  (make-instance 'parameter-type-set :items items))

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
   (encoding-type :initarg :encoding-type
                  :type encoding)
   (to-string :initarg :to-string)
   (valid-range :initarg :valid-range)
   (default-alarm :initarg :default-alarm)
   (context-alarm-list :initarg :context-alarm-list :type context-alarm-list)))

(defun make-float-parameter-type (name &key short-description
                                            base-type
                                            initial-value
                                            size-in-bits
                                            long-description
                                            alias-set
                                            ancillary-data-set
                                            encoding-type
                                            to-string
                                            valid-range
                                            default-alarm
                                            context-alarm-list)
  (check-type name symbol)
  (require-unique-key name)
                                        ;(if encoding (check-type encoding encoding))
  (if short-description (check-type short-description string))
  (if base-type nil)
  (if initial-value nil)
  (if size-in-bits nil)
  (if long-description nil)
  (if alias-set nil)
  (if ancillary-data-set nil)
  (if encoding-type nil)
  (if to-string nil)
  (if valid-range nil)
  (if default-alarm nil)
  (if context-alarm-list nil)
  (make-instance 'float-parameter :name name
                                  :short-description short-description
                                  :base-type base-type
                                  :initial-value initial-value
                                  :size-in-bits size-in-bits
                                  :long-description long-description
                                  :alias-set alias-set
                                  :ancillary-data-set ancillary-data-set
                                  :encoding-type encoding-type
                                  :to-string to-string
                                  :valid-range valid-range
                                  :default-alarm default-alarm
                                  :context-alarm-list context-alarm-list))

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
   (encoding-type :initarg :encoding-type
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
               encoding-type
               to-string
               valid-range
               default-alarm
               context-alarm-list) obj
    (cxml:with-element* ("xtce" "IntegerParameterType")
      (if short-description (cxml:attribute "shortDescription" short-description))
      (if name (cxml:attribute "name" (format nil "~A" name)))
      (if base-type (cxml:attribute "baseType" base-type))
      (if initial-value (cxml:attribute "initialValue" initial-value))
      (if size-in-bits (cxml:attribute "sizeInBits" size-in-bits))
      (cxml:attribute "signed" (format-bool signed))
      (if long-description (cxml-marshall  long-description))
      (if alias-set (cxml-marshall alias-set))
      (if unit-set (cxml-marshall unit-set))
      (if encoding-type (cxml-marshall encoding-type))
      (if to-string (cxml-marshall to-string))
      (if valid-range (cxml-marshall valid-range))
      (if default-alarm (cxml-marshall default-alarm))
      (if context-alarm-list (cxml-marshall context-alarm-list)))))

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
                                    encoding-type
                                    to-string
                                    valid-range
                                    default-alarm
                                    context-alarm-list)
  (check-type name symbol)
  (if short-description (check-type short-description string))
  
  (if base-type nil)
  (if initial-value nil)
  (if size-in-bits (check-type size-in-bits positive-integer))
  (if (eq signed 'NOTHING)
	  (progn
		(setf signed nil)
		(check-type signed boolean)))
  (if long-description (check-type long-description long-description))
  (if alias-set (check-type alias-set alias-set))
  (if ancillary-data-set (check-type ancillary-data-set ancillary-data-set))
  (if unit-set (check-type unit-set unit-set))
  (if encoding-type (check-type encoding-type encoding))
  (if valid-range (check-type valid-range valid-range))
  (if default-alarm (check-type default-alarm alarm))
  (if context-alarm-list (check-type context-alarm-list context-alarm-list))
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
                                         :encoding-type encoding-type
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

(defmethod cxml-marshall ((obj float-parameter-type))
  (with-slots (name short-description) obj
    (cxml:attribute "name" name)
    (if short-description (cxml:attribute "shortDescription" short-description))))

(defclass enumerated-parameter-type (parameter-type)
  ((inital-value :initarg :initial-value)
   (encoding-type :initarg :encoding-type
				  :type encoding)
   (enumeration-list :initarg :enumeration-list
					 :type enumeration-list)
   (default-alarm :initarg :default-alarm
				  :type enumeration-alarm-type)
   (context-alarm-list :initarg :context-alarm-list
					   :type context-alarm-list)))

(defun make-enumerated-parameter-type (name
									   &key
										 short-description
										 base-type
										 initial-value
										 long-description
										 alias-set
										 ancillary-data-set
										 unit-set
										 encoding-type
										 enumeration-list
										 default-alarm
										 context-alarm-list)
  (check-type name string)
  (check-optional-type short-description string)
  (check-optional-type base-type t)
  (check-optional-type long-description string)
  (check-optional-type alias-set alias-set)
  (check-optional-type ancillary-data-set ancillary-data-set)
  (check-optional-type unit-set unit-set)
  (check-optional-type encoding-type encoding)
  (check-optional-type enumeration-list enumeration-list)
  (check-optional-type default-alarm enumeration-alarm)
  (check-optional-type context-alarm-list context-alarm-list)
  (check-optional-type initial-value t)
  ; Need to check if inital value is in enumeration list
  (make-instance 'enumerated-parameter-type )
  )

(defclass enumeration-alarm (alarm)
  (ancillary-data-set :initarg ancillary-data-set)
  (alarm-conditions :initarg :alarm-conditions)
  (alarm-co))

(defclass enumeration-list (xtce-set) ())

(defclass enumeration ()
  (value :initarg :value)
  (max-value :initarg :max-value)
  (label :initarg :label
		 :type symbol)
  (short-description :initarg :short-description
					 :type string))

(defun make-enumeration (value label &key max-value short-description)
  (check-type value number)
  (check-type label symbol)
  (if max-value (check-type max-value number))
  (if short-description (check-type short-description string))
  (make-instance 'enumeration :value value :label label :max-value max-value :short-description short-description))

(defmethod cxml-unmarshall ((obj enumeration))
  (with-slots (value label max-value short-description) obj
	(cxml:with-element* ("xtce" "Enumeration")
	  (cxml:attribute "label" (format nil "~A" label))
	  (cxml:attribute "value" (format nil "~A" value))
	  (if max-value (cxml:attribute "maxValue" max-value))
	  (if short-description (cxml:attribute "shortDescription" short-description)))))

(defun make-enumeration-list (&rest items)
  (make-xtce-set 'enumeration items))

(describe (make-instance 'enumerated-parameter-type))

