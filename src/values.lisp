(in-package :xtce)

(defclass size () ())

(defclass integer-value ()
  ((boxed-value :initarg :boxed-value)))

(defun make-integer-value (boxed-value)
  (check-type boxed-value (or fixed-value dynamic-value discrete-value))
  (make-instance 'integer-value :boxed-value boxed-value))

(defclass fixed-value ()
  ((value :initarg :value)))

(defun make-fixed-value (value)
  (make-instance 'fixed-value :value value))

(defclass dynamic-value ()
  ((parameter-instance-reference :initarg :parameter-instance-reference)))

(defclass parameter-instance-reference ()
  ((parameter-reference :initarg parameter-reference)
   (instance :initarg instance)
   (use-calibrated-value :initarg use-calibrated-value)))

(defclass linear-adjustment ()
  ((slope :initarg slope :type float)
   (intercept :initarg intercept :type float)))

(defclass argument-dynamic-value (dynamic-value)
  ((argument-instance-reference :type argument-instance-reference)))

(defclass argument-instance-ref ()
  ((argument-reference :initarg :argument-reference)
   (use-calibrated-value :initarg :use-calibrated-value
                         :type boole)))

(deftype discrete-lookup-list ()) ;; this one is complex

(defclass size-in-bits () ())

(defclass termination-char (size-in-bits)
  ((termination-char :initarg :termination-char)))

(defun make-termination-char (&optional (termination-char nil))
  (make-instance 'termination-char :termination-char termination-char)) 

(defclass leading-size (size)
  ((leading-size :initarg leading-size
                 :type integer)))

(defun make-leading-size (&optional (leading-size 16))
  leading-size
  )

(defclass fixed-size-in-bits (size-in-bits) ())



(defclass error-data-detect () ())

(defclass byte-order-list () ())

