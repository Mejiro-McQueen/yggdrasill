(defpackage :xtce
  (:use :cl)
  (:documentation "XTCE")
  (:export :make-space-system
           :make-telemetry-metadata))

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
  (if long-description (check-type long-description long-description))
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
      (if header (cxml:attribute "header" header))
      (if operational-status (cxml:attribute "operational-status" operational-status))
      (if xml-base (cxml:attribute "xml:base" xml-base))
      (if long-description (cxml-marshall long-description))
      (if telemetry-metadata (cxml-marshall telemetry-metadata)))))


(defclass telemetry-metadata ()
  ((parameter-type-set)
   (parameter-set)
   (container-set)
   (message-set)
   (stream-set)
   (algorithm-set)))

(defun make-telemetry-metadata (&key parameter-type-set
                                     parameter-set
                                     container-set
                                     message-set
                                     stream-set
                                     algorithm-set)
  (if parameter-type-set (check-type parameter-type-set parameter-type-set)))

(defclass unit-set () ((items :initarg :items
                               :type list)))

(defun make-unit-set (&rest items)
  (let ((items (remove nil items))) 
    (dolist (i items) 
      (check-type i unit))
    (make-instance 'unit-set :items items)))

(defmethod cxml-marshall ((object unit-set))
  (with-slots (items) object
    (cxml:with-element* ("xtce" "UnitSet")
      (dolist (i items)
        (if i (cxml-marshall i))))))

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

(defun require-unique-key (key)
  (assert (null (member key *UNIQUE-KEYS*)) (key) "The key ~A must be a symbol unique to this XTCE system. If working interactively, reset using (setf *UNIQUE-KEYS* ())" key)
  (setf *UNIQUE-KEYS* (cons key *UNIQUE-KEYS*)))
