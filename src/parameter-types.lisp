(in-package :xtce)

(defclass parameter-type () ((unit-set :initarg :unit-set
                                       :type unit-set)))

(deftype parameter-type-set ()
  '(and list (satisfies parameter-type-set-p)))

(defun parameter-type-set-p (s)
  (every (lambda (i) (typep i 'parameter-type)) s))

(defun make-parameter-type-set (&rest items)
  (let ((items (remove nil items))) 
    (dolist (i items) 
      (check-type i pramater-type))))

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
                                  :encoding encoding
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
   (ancialliary-data-set :initarg :ancillary-data-set
                         :type ancillary-data-set)
   (unit-set :initarg :unit-set
             :type unit-set)
   (to-string :initarg :to-string)
   (valid-range :initarg :valid-range)
   (default-alarm :initarg :default-alarm
                  :type alarm)
   (context-alarm-list :initarg :context-alarm-list
                       :type context-alarm-list)))

(class alarm () ())

(class numeric-alarm () ())

(deftype positive-integer ()
  "A type for positive integers."
  `(and integer (satisfies plusp)))

(defmethod cxml-marshall ((obj float-parameter-type))
  (with-slots (name short-description) obj
    (cxml:attribute "name" name)
    (if short-description (cxml:attribute "shortDescription" short-description))))
