(ql:quickload "bifrost-integral")
(ql:quickload "uiop")
(declaim (optimize (speed 0) (space 0) (debug 3)))

(in-package :xtce)

(make-space-system
 "SpaceVechicle"
 :telemetry-metadata
 (make-telemetry-metadata
  :parameter-type-set
  (make-parameter-type-set
   (make-integer-parameter-type
    "IDType"
    :data-encoding
    (make-integer-data-encoding)
	:signed nil)
   (make-integer-parameter-type
	"SecHType"
	:signed nil
	:data-encoding
	(make-integer-data-encoding :size-in-bits 1))
   (make-integer-parameter-type
	"TypeType"
	:signed nil
	:data-encoding
	(make-integer-data-encoding :size-in-bits 1))
   (make-integer-parameter-type
	"LengthType"
	:signed nil
	:data-encoding
	(make-integer-data-encoding :size-in-bits 16))
   (make-enumerated-parameter-type
	"PSWHLTIMFLGType"
	:data-encoding
	(make-integer-data-encoding :size-in-bits 16)
	:enumeration-list
	(make-enumeration-list
	 (make-enumeration 'TIMER_OFF 0)
	 (make-enumeration 'TIMER_ON 1)
	 (make-enumeration 'TIMER_COMPLETED 2)))
   
   (make-float-parameter-type
	"PBATMTEMPType"
	:size-in-bits 64
	:unit-set
	(make-unit-set
	 (make-unit
	  :description "Bq"
	  :form "units:Becquerel"))
	:data-encoding
	(make-integer-data-encoding
	 :size-in-bits 16
	 :encoding 'twos-complement
	 :default-calibrator
	 (make-polynomial-calibrator
	  :term-list
	  (make-term-list 
	   (make-term :coefficient -7459.23273708 :exponent 0)
			 (make-term :coefficient 8.23643519148 :exponent 1)
			 (make-term :coefficient -3.02185061876e3 :exponent 2)
			 (make-term :coefficient 2.33422429056e-7 :exponent 3)
			 (make-term :coefficient 5.67189556173e11 :exponent 4)))))
   
   (make-absolute-time-parameter
	"MissionTimeType"
	:reference-time
	(make-reference-time
	 (make-offset-from "Seconds")))
   
   (make-absolute-time-parameter
	"SecondsType"
	:encoding
	(make-encoding
	 :units "seconds"
	 :data-encoding
	 (make-integer-data-encoding :size-in-bits 32))
	:reference-time
	(make-reference-time
	 (make-offset-from "Milliseconds")))
   
   (make-absolute-time-parameter
	"Milliseconds"
	:encoding
	(make-encoding
	 :units "seconds"
	 :scale 0.001
	 :data-encoding
	 (make-integer-data-encoding :size-in-bits 16))
	:reference-time
	(make-reference-time
	 (make-epoch 'TAI)))
   )
  :parameter-set
  (make-parameter-set
   (make-parameter "SecH" "SecHType")
   (make-parameter "Type" "TypeType")
   (make-parameter "ID" "IDType")
   (make-parameter "Length" "LengthType")
   (make-parameter "Seconds" "SecondsType")
   (make-parameter "Milliseconds" "MillisecondsType")
   (make-parameter "PBATMTEMP" "PBATMTEMPType")
   (make-parameter "PSWHLTIMFLG" "PSWHLTIMFLGType")
   (make-parameter "MissionTime" "MissionTimeType"
				   :parameter-properties
				   (make-parameter-properties :data-source "derived")))
  :container-set
  (make-container-set
   (make-sequence-container
	"Header"
	(make-entry-list
	 (make-parameter-ref-entry "ID")
	 (make-parameter-ref-entry "SecH")
	 (make-parameter-ref-entry "Type")
	 (make-parameter-ref-entry "Length")
	 (make-parameter-ref-entry "SecondaryHeader"
							   :include-condition
							   (make-include-condition (make-comparison "SecH" 1)))
	 )
	)
   )
  ))


(dump-space-system-xml (symbol-value 'SPACEVECHICLE))

(defparameter *P1* '/NICE/LMAO/ROFLCOPTER)
(defparameter *P1* './NICE/LMAO/ROFLCOPTER)
(defparameter *P1* '../NICE/LMAO/ROFLCOPTER)
(defparameter *P1* 'ROFLCOPTER)

(defparameter *flags* nil)
(defparameter *paths* nil)
(defparameter *namestring* nil)
(defparameter *just-path* nil)


;(describe (make-comparison 'ref 1) )

;; (defun search-xtce-key (requested-key)
;;   (check-type requested-key symbol)
;;   (multiple-value-bind (flag path-list)
;; 	  (uiop::split-unix-namestring-directory-components (string requested-key) :ensure-directory t)
;; 	(print flag)
;; 	(print path-list)
;; 	(cond
;; 	  ( (equal flag :absolute)
;; 		(redu))
;;   ))

;(search-xtce-key '/NICE/LMAO/ROFLCOPTER)

;When evaluating a qualified path:
; climb parents until nil is reached
; climb down as needed
; grab symbol from symbol table
; optionally-teleport to *ROOT*

; Note: Containers need to be resolved. It is possible that conditions eval in such a way that different sub containers can be dynamically sswapped in and out (include conditions).

;Need to build DAG and resolve at leaves


(defparameter *TEST* (make-container-set 
 (make-sequence-container
  "MyFormatHeader"
  (make-entry-list
   (make-parameter-ref-entry "Version")
   (make-parameter-ref-entry "Type")
   (make-parameter-ref-entry "ID")
   (make-parameter-ref-entry "Length"))
   :abstract t)
 
 (make-sequence-container
  "MyTimeStampHeader"
  (make-entry-list
   (make-parameter-ref-entry "TimeStamp"))
  :abstract t
  :base-container
  (make-base-container
   "MyFormatHeader"
   :restriction-criteria
   (make-restriction-criteria
	(make-comparison-list
	 (make-comparison "Version" 1)
	 (make-comparison "Type" 1)))))

 (make-sequence-container
  "MyConcretePacket"
  (make-entry-list
   (make-parameter-ref-entry "P1")
   (make-parameter-ref-entry "P2")
   (make-parameter-ref-entry "P3"))
  :base-container
  (make-base-container
   "MyTimeStampHeader"
   :restriction-criteria
   (make-restriction-criteria
	(make-comparison "ID" 100))))))


(defparameter *HASH* (make-hash-table))

(dolist (container (slot-value *TEST* 'items))
  (let ((container-name (slot-value container 'name)))
  (setf (gethash container-name *HASH*) container)))

(defun find-resolvable-containers (containers-map)
  (let ((resolveable-containers '()))
	(maphash (lambda (key value)
			   (declare (ignore key))
			   (when (resolveable-container-p value)
				 (push value resolveable-containers)))
			 containers-map)
  resolveable-containers))

(defun resolve (parent child container-map)
  (cond ((not parent) child)
		((resolveable-container-p parent)
		 (let* ((parent-base-container (slot-value parent 'base-container))
			   (parent-container-ref (slot-value parent-base-container 'container-ref))
			   (grand-parent (gethash parent-container-ref container-map)))
		   (resolve grand-parent parent container-map)))
		(t (deep-inherit parent child))))

(resolve-containers *HASH*)

(defun resolve-containers (container-map)
  (let ((resolvable-containers (find-resolvable-containers container-map))
		(resolved '()))
	(dolist (child resolvable-containers)
	  (let* ((base-container (slot-value child 'base-container))
			 (parent-reference (slot-value base-container 'container-ref))
			 (parent (gethash parent-reference container-map)))
		(push (resolve parent child container-map) resolved)))
	resolved))

(defun resolveable-container-p (container)
  (when container
  (let ((abstract (slot-value container 'abstract))
		(base-container (slot-value container 'base-container)))
		(and base-container abstract))))

(defmethod deep-inherit ((parent null) (child sequence-container))
  (declare (ignore parent))
  child)

(defmethod deep-inherit ((parent sequence-container) (child sequence-container))
  (let* ((name (slot-value child 'name))
		 (abstract (slot-value child 'abstract))
		 (idle-pattern (slot-value child 'idle-pattern))
		 (short-description (slot-value child 'short-description))
		 (long-description (slot-value child 'long-description))
		 (alias-set (slot-value child 'long-description))
		 (ancillary-data-set (append (slot-value child 'ancillary-data-set) (slot-value parent 'ancillary-data-set)))
		 (default-rate-in-stream (if (slot-value child 'default-rate-in-stream) (slot-value parent 'default-rate-in-stream)))
		 (child-rate-in-stream-set (slot-value child 'rate-in-stream-set))
		 (parent-rate-in-stream-set (slot-value parent 'rate-in-stream-set))
		 (rate-in-stream-set (make-rate-in-stream-set
							  (union (if child-rate-in-stream-set
										 (items child-rate-in-stream-set)
										 nil)
									 (if parent-rate-in-stream-set
										 (items parent-rate-in-stream-set)
										 nil) :key 'stream-ref)))
		 
		 (binary-encoding (if (slot-value child 'binary-encoding) (slot-value parent 'binary-encoding)))
		 (parent-entry-list (slot-value parent 'entry-list))
		 (child-entry-list (slot-value child 'entry-list))
		 (entry-list (make-entry-list
					  (append (if child-entry-list
								  (items child-entry-list)
								  nil)
							  (if parent-entry-list
								  (items parent-entry-list)))))
		 (base-container nil)
		 (new (make-sequence-container name
									   entry-list
									   :abstract abstract
									   :idle-pattern idle-pattern
									   :short-description short-description
									   :long-description long-description
									   :alias-set alias-set
									   :ancillary-data-set ancillary-data-set
									   :rate-in-stream-set rate-in-stream-set
									   :base-container base-container
									   :default-rate-in-stream default-rate-in-stream
									   :binary-encoding binary-encoding)))
	new))

;(mapcar 'describe (resolve-containers *HASH*))



;; (step (resolve-containers *HASH*))

;; (trace resolve)
;; (trace resolve-containers)


;; (defparameter t1
;;   (make-sequence-container
;;    "MyFormatHeader"
;;    (make-entry-list
;; 	(make-parameter-ref-entry "Version")
;; 	(make-parameter-ref-entry "Type")
;; 	(make-parameter-ref-entry "ID")
;; 	(make-parameter-ref-entry "Length"))
;;    :abstract t))

;; (defparameter t2
;;   (make-sequence-container
;;    "MyTimeStampHeader"
;;    (make-entry-list
;; 	(make-parameter-ref-entry "TimeStamp"))
;;    :abstract t
;;    :base-container
;;    (make-base-container
;; 	"MyFormatHeader"
;; 	:restriction-criteria
;; 	(make-restriction-criteria
;; 	 (make-comparison-list
;; 	  (make-comparison "Version" 1)
;; 	  (make-comparison "Type" 1))))))
