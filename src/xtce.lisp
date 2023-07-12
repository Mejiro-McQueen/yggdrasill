(defpackage :xtce
  (:use :cl)
  (:use :cxml)
  (:documentation "XTCE"))

(in-package :xtce)

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
   (space-system :initarg :space-system)))

(defclass long-description () ((long-description :initarg :long-description
                                                 :type string)))

(defun make-long-description (s)
  (check-type s string)
  (make-instance 'long-description :long-description s))

(defmethod cxml-marshall ((obj long-description))
  (with-slots (long-description) obj
    (cxml:with-element* ("xtce" "LongDescription"))
    (cxml:text long-description)))

(defun make-space-system (name
                          &key header
                               operational-status
                               long-description
                               alias-set
                               ancilliary-data-set
                               telemetry-metadata
                               command-metadata
                               xml-base
                               service-set
                               space-system)
  (check-type long-description long-description)
  (make-instance 'space-system :name name
                               :header header
                               :operational-status operational-status
                               :long-description long-description
                               :alias-set alias-set
                               :ancilliary-data-set ancilliary-data-set
                               :telemetry-metadata telemetry-metadata
                               :command-metadata command-metadata
                               :xml-base xml-base
                               :service-set service-set
                               :space-system space-system))

(defgeneric cxml-marshall (obj))

(defmacro with-non-empty (var &body body)
  "Eval whenever var is non empty. Use to skip processing optional elements and attributes."
  `(when (not (null ,var))
     ,@body))

(defparameter *TEST* (make-space-system "Tuatha De Danaan"
                                        :long-description (make-long-description "A not so long description")))

(describe *TEST*)


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
      (with-non-empty header (cxml:attribute "header" header))
      (with-non-empty operational-status (cxml:attribute "operational-status" operational-status))
      (with-non-empty xml-base (cxml:attribute "xml:base" xml-base))
      (with-non-empty long-description (cxml-marshall long-description))
      (with-non-empty telemetry-metadata (cxml-marshall telemetry-metadata)))))

(cxml:with-xml-output (cxml:make-string-sink :indentation 4 :canonical nil)
  (cxml:comment "Bifrost Integral")
  (cxml:with-namespace ("xtce" "http://www.omg.org/spec/XTCE/20180204")
    (cxml:with-namespace ("xsi" "http://www.w3.org/2001/XMLSchema-instance")
        (cxml-marshall *TEST*))))


(defclass telemetry-metadata ()
  ((parameter-type-set)
   (parameter-set)
   (container-set)
   (message-set)
   (stream-set)
   (algorithm-set)))

(defun make-telemetry (&key parameter-type-set
                            parameter-set
                            container-set
                            message-set
                            stream-set
                            algorithm-set)
  (with-non-empty parameter-type-set (check-type parameter-type-set parameter-type-set))
  )

;;;;; Parameter Types

(deftype parameter-type-set () ())

(defclass parameter () ())

(defclass string-parameter (parameter) ())

(defclass float-parameter (parameter)
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
   (encoding :initarg :encoding
             :type encoding)
   (to-string :initarg :to-string)
   (valid-range :initarg :valid-range)
   (default-alarm :initarg :default-alarm)
   (context-alarm-list :initarg :context-alarm-list :type context-alarm-list)))

(defun make-float-parameter (&key name
                                  short-description
                                  base-type
                                  initial-value
                                  size-in-bits
                                  long-description
                                  alias-set
                                  ancillary-data-set
                                  encoding
                                  to-string
                                  valid-range
                                  default-alarm
                                  context-alarm-list)
  (check-type name string)
  ;(with-non-empty encoding (check-type encoding encoding))
  (with-non-empty short-description (check-type short-description string))
  (with-non-empty base-type nil)
  (with-non-empty initial-value nil)
  (with-non-empty size-in-bits nil)
  (with-non-empty long-description nil)
  (with-non-empty alias-set nil)
  (with-non-empty ancillary-data-set nil)
  (with-non-empty encoding nil)
  (with-non-empty to-string nil)
  (with-non-empty valid-range nil)
  (with-non-empty default-alarm nil)
  (with-non-empty context-alarm-list nil)
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

(defmethod cxml-marshall ((obj float-parameter))
  (with-slots (name short-description) obj
    (cxml:attribute "name" name)
    (with-non-empty short-description (cxml:attribute "shortDescription" short-description))))



