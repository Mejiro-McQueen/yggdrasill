(in-package :xtce)

(defclass resolved-parameter-ref-entry () ((parameter-ref :initarg :parameter-ref)
										   (short-description :initarg :short-description :type string)
												 (location-in-container-in-bits
												  :initarg :location-in-container-in-bits
												  :type location-in-container-in-bits)
												 (repeat-entry :initarg :repeat-entry :type repeat-entry)
												 (include-condition :initarg :include-condition :type include-condition)
												 (time-association :initarg :time-association :type time-association)
												 (ancillary-data-set :initarg :ancillary-data-set :type ancillary-data-set)))

(defun find-resolvable-containers (containers-map)
  (let ((resolveable-containers '()))
	(maphash (lambda (key container)
			   (declare (ignore key))
			   (when (resolveable-container-p container)
				 (push container resolveable-containers)))
			 containers-map)
  resolveable-containers))

(defun abstract-container-p (container)
  (when container
	(slot-value container 'abstract)))

(defun concrete-container-p (container)
  (when container
  (not (abstract-container-p container))))
		  
(defun resolveable-container-p (container)
  (when container
	(and (concrete-container-p container)
		 (heir-container-p container))))

(defun heir-container-p (container)
  (when container
	(if (slot-value container 'base-container)
		t)))

(defun unresolvable-container-p (container)
  (not (resolveable-container-p container)))

;; (dump-xml (first (resolve-containers *HASH*)))

(defun resolve (func container container-map)
  (let* ((base-container (if container (slot-value container 'base-container)))
		 (parent-container-ref (if base-container (slot-value base-container 'container-ref)))
		 (parent-container (gethash parent-container-ref container-map)))
	(if (heir-container-p parent-container)
		(funcall func container (resolve func parent-container container-map))
		(funcall func container parent-container))))

(defun resolve-containers (f container-map)
  "Map a function on all concrete containers, descending recurs down their inheritance chain.

   Args:
   function: (container container-parent container-jump-table)
   container-map: jump table for containers."
  (let ((resolvable-containers (find-resolvable-containers container-map)))
		(mapcar #'(lambda (container) (resolve f container container-map)) resolvable-containers)))

(defmethod deep-inherit ((child sequence-container) (parent sequence-container))
  ;NOTE: It looks like nextContainer just means that container's restriction's must pass for this container to take effect.
  ;See: 4.3.4.9.4 NextContainer Element
  ;5.5 DYNAMIC CONTAINER MATCHING seems to take effect in order to dynamically resolve that container if it is abstract
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
		 (rate-in-stream-set (apply #' make-rate-in-stream-set
							  (union (if child-rate-in-stream-set
										 (items child-rate-in-stream-set)
										 nil)
									 (if parent-rate-in-stream-set
										 (items parent-rate-in-stream-set)
										 nil) :key 'stream-ref)))
		 
		 (binary-encoding (if (slot-value child 'binary-encoding) (slot-value parent 'binary-encoding)))
		 (parent-entry-list (slot-value parent 'entry-list))
		 (child-entry-list (slot-value child 'entry-list))
		 (entry-list (apply #'make-entry-list
							(append
							 (if parent-entry-list
								 (items parent-entry-list)
								 nil)
							 (if child-entry-list
								 (items child-entry-list)
								 nil)
							 )))
		 ;I'm keeping the base container so that we can neatly check the restriction criteria.
		 ;I don't know how I feel about making a non XTCE construct to hold it at the top level.
		 ;Descending the inheritance tree shouldn't bee too expensive.
		 (base-container nil) ;(slot-value child 'base-container))
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

;; (resolve-containers #'deep-inherit *HASH*)


;; (defparameter *TEST* (make-container-set 
;;  (make-sequence-container
;;   '|MyFormatHeader|
;;   (make-entry-list
;;    (make-parameter-ref-entry '|Version|)
;;    (make-parameter-ref-entry '|Type|)
;;    (make-parameter-ref-entry '|ID|)
;;    (make-parameter-ref-entry '|Length|))
;;    :abstract t)
 
;;  (make-sequence-container
;;   '|MyTimeStampHeader|
;;   (make-entry-list
;;    (make-parameter-ref-entry '|TimeStamp|))
;;   :abstract t
;;   :base-container
;;   (make-base-container
;;    '|MyFormatHeader|
;;    :restriction-criteria
;;    (make-restriction-criteria
;; 	(make-comparison-list
;; 	 (make-comparison '|Version| 1)
;; 	 (make-comparison '|Type| 1)))))

;;  (make-sequence-container
;;   '|MyConcretePacket|
;;   (make-entry-list
;;    (make-parameter-ref-entry '|P1|)
;;    (make-parameter-ref-entry '|P2|)
;;    (make-parameter-ref-entry '|P3|))
;;   :base-container
;;   (make-base-container
;;    '|MyTimeStampHeader|
;;    :restriction-criteria
;;    (make-restriction-criteria
;; 	(make-comparison '|ID| 100))))))


;; (defparameter *HASH* (make-hash-table))

;; (dolist (container (slot-value *TEST* 'items))
;;   (let ((container-name (slot-value container 'name)))
;;   (setf (gethash container-name *HASH*) container)))


;; (dump-xml (first (resolve-containers #'deep-inherit *HASH*)))



; Note: CCSDS 660.1-G-2 typo Page 4-142, figure caption says ContainrRefEntry
; Note 4.3.4.8.7 StreamSegmentRefEntry Figure 4-84 describes stream segment and not stream segment ref
;Figure 3-13: DiscreteLookup describes discretelookuplisttype
; PG. 5-14 Typo "revolved"
