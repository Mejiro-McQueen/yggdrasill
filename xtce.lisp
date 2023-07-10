(require "asdf")
(ql:quickload "cxml")
(asdf:load-system :cxml)

(defclass space-system ()
  ((header :initarg :header)
   (name :initarg :name)))

(defun make-space-system (&optional (header))
  (make-instance 'space-system :header header))


(defclass telemetry-metadata ()
  ((parameter-type-set)
   (parameter-set)
   (container-set)
   (message-set)
   (stream-set)
   (algorithm-set)))

(defun parameter-type-set-p (s)
  (and (listp s)
       (every #'parameter-type-p s)))

;;;;;;;;;;;;;;;;;;;;;

(defun valid-integer-encoding-p (enc)
  (assert (member enc '(unsigned sign-magnitude twos-compliment ones-compliment bcd packed-bcd)) (enc)
          "Int Encoding ~A is not <Unsigned | twos-compliment | ones-compliment | bcd | packed-bcd" enc) 
  t)

(defclass integer-data-encoding ()
  ((encoding :documentation "Specifies integer numeric value to raw encoding method." :initarg :encoding)
   (size-in-bits :documentation "Number of bits to use for the raw encoding." :initarg :size-bits :type integer)
   (change-threshold :documentation "Error detection algorithm" :initarg :change-threshold)
   (default-calibrator :documentation "TODO" :initarg :default-calibrator)
   (context-calibrator-list :documentation "TODO" :initarg :context-calibrator-list)))

(defun make-int-data-encoding (&optional (size-bits 8)
                                         (encoding 'UNSIGNED)
                                         (change-threshold nil)
                                         (default-calibrator nil)
                                         (context-calibrator-list))
  (valid-integer-encoding-p encoding)
  (check-type size-bits integer)
  (assert (plusp size-bits) (size-bits) "size-bits ~A must be a positive integer" size-bits)
  (make-instance 'integer-data-encoding
                 :size-bits size-bits
                 :encoding encoding
                 :change-threshold change-threshold
                 :context-calibrator-list context-calibrator-list))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass numeric-calibrator ()
  ((name :documentation "Optional name" :initarg :name)
   (short-description :documentation "Optional description" :initarg :short-description)
   (ancilliary-data-set :documentation "Optional additional information") :initarg :ancilliary-data-set))

(defclass spline-calibrator (numeric-calibrator)
  ((order :initarg order)
   (extrapolate :initarg extrapolate)
   (spline-point-list :initarg spline-point-list :type spline-point-list])
   (ancilliary-data-set :initarg ancillary-data-set)))

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
  ((polynmial-term-list :type polynomial-term-list :initarg polynomial-term-list)))

(defclass polynomial-term-type ()
  ((coefficient :initarg coefficient :type integer )
   (exponent :initarg exponent :type coefficient)))

(defun make-polynomial-term-type (coefficient exponent)
  (check-type coefficient number)
  (check-type exponent number)
  (assert (>= 0 exponent) (exponent) "Exponent value ~A must be non-negative, use math-operation-calibrator" exponent)
  (make-instance 'polynomial-term-type coefficient exponent))

(deftype polynomial-term-list () "list of polynomial terms points"
  '(satisfies spline-point-list-p))

(defun polynomial-term-list-p (l)
  (and (listp l)
       (every (lambda (i) (typep i 'polynomial-term-type)) l)))

;;;;;;;;;;;;;;;;;;; Calibrators
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

(defun make-comparison (parameter-ref value &optional (instance 0) (use-calibrated-value t) (comparison-operator '=))
  (make-instance 'comparison
                 :parameter-ref parameter-ref
                 :instance instance
                 :use-calibrated-value use-calibrated-value
                 :value value))

(deftype comparison-list ()
  '(satisfies comparison-list-p))

(defun comparison-list-p (l)
  (and (listp l)
       (every (lambda (i) (typep i 'comparison)) l)))

(defclass boolean-expression (context-match)
  ((expression :initarg :expression
               :documentation "a predicate")))

(defclass custom-algorithm (context-match)
  ((expression :initarg :expression
               :documentation "LISP expression")))

;;;;;;;;;;VALUES;;;;;;;;;;;;;;;;

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
   (intercept: :initarg intercept :type float)))

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

(defun make-leading-size (&optional (leading-size 16)))

(defclass fixed-size-in-bits (size-in-bits) ())

(defclass string-data-encoding ()
  ((size-in-bits :initarg :size-in-bits
                 :type size-in-bits)
   (bit-order :initarg :bit-order)
   (error-detect-correct :initarg :error-detect-correct
                         :type error-detect-correct)
   (byte-order-list :initarg :byte-order-list
                    :type byte-order-list)
   (encoding :initarg :encoding)))

(defun make-string-data-encoding (size-in-bits &optional (bit-order)
                                                         (encoding)
                                                         (error-detect-correct)
                                                         (byte-order-list))
  
  )

(defun valid-string-encoding-p (enc)
  (let ((valid-encodings '(US-ASCII WINDOWS-1252 ISO-UTF-8 UTF-16 UTF-16LE UTF-16BE UTF-32 UTF-32LE UTF-32BE) )) 
    (assert (member enc valid-encodings) (enc) "String encoding ~A is not one of: ~A" enc valid-encodings)) t)

(defclass error-data-detect () ())

(defclass byte-order-list () ())


(defclass binary-data-encoding ()
  ((bit-order :initarg :bit-order)
   (byte-order :initarg :byte-order)
   (size-in-bits :initarg :size-in-bits :type size-in-bits)
   (error-detect-correct :initarg :error-detect-correct)
   (from-binary-transform-algorithm :initarg :from-binary-transform-algorithm)
   (to-binary-transform-algorithm :initarg :from-binary-transform-algorithm)))

(defun make-binary-data-encoding (size-in-bits &optional (bit-order)
                                                         (byte-order)
                                                         (error-detect-correct)
                                                         (from-binary-transform-algorithm)
                                                         (to-binary-transform-algorithm))
  (check-type size-in-bits size-in-bits)
  (make-instance 'binary-data-encoding
                 :size-in-bits size-in-bits
                 :bit-order bit-order
                 :byte-order byte-order
                 :error-detect-correct error-detect-correct
                 :from-binary-transform-algorithm from-binary-transform-algorithm
                 :to-binary-transform-algorithm to-binary-transform-algorithm))



;;;;;;;;;;;;;;;;
(defclass float-data-encoding ()
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
  (if (member size '(32 64 128 'non-32))
      t))

(defun make-float-data-encoding (size-in-bits &optional (bit-order 'MSB)
                                                        (byte-order 'BIG)
                                                        (encoding 'IEE)
                                                        (change-threshold nil)
                                                        (error-detection)
                                                        (default-calibrator)
                                                        (context-calibrator-list))
  (valid-bit-order-p bit-order)
  (or (context-calibrator-list-p context-calibrator-list) (null context-calibrator-list))
  (check-type size-bits integer)
  (assert (> size-bits 0) (size-bits) "size-bits ~A must be a positive integer" size-bits)
  (make-instance 'float-data-encoding
                 :bit-order bit-order
                 :byte-order byte-order
                 :size-in-bits size-in-bits
                 :encoding encoding
                 :error-detection error-detection
                 :default-calibrator default-calibrator
                 :change-threshold change-threshold
                 :context-calibrator-list context-calibrator-list))


;;;;;;;;;;;;;;;;;
(defun valid-bit-order-p (bit-order)
  (assert (member bit-order '(MSB LSB)) (bit-order) "Bit Order ~A is not <LSB | MSB>" bit-order) t)

(defun valid-float-encoding-p (enc)
  (assert (member enc '(IEE MIL)) (enc) "Encoding ~A is not <IEE | MIL>" enc) t)

(ql:quickload "lisp-binary")
