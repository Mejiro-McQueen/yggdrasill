(defstruct space-system
  (header nil)
  (telemetry-metadata nil))

(make-space-system)

(defstruct telemetry-metadata
  (parameter-type-set nil)
  (parameter-set nil)
  (container-set nil)
  (message-set nil)
  (stream-set nil)
  (algorithm-set nil))

(defun parameter-type-set-p (s)
  (and (listp s)
       (every #'parameter-type-p s)))

;;;;;;;;;;;;;;;;;;;;;

(defun valid-integer-encoding-p (enc)
  (assert (member enc '(unsigned sign-magnitude twos-compliment ones-compliment bcd packed-bcd)) (enc) "Int Encoding ~A is not <Unsigned | twos-compliment | ones-compliment | bcd | packed-bcd" enc) 
  t)

(defclass integer-data-encoding () ((encoding :documentation "Specifies integer numeric value to raw encoding method." :initarg :encoding)
                                  (size-in-bits :documentation "Number of bits to use for the raw encoding." :initarg :size-bits :type integer)
                                  (change-threshold :documentation "Error detection algorithm" :initarg :change-threshold)
                                  (default-calibrator :documentation "TODO" :initarg :default-calibrator)
                                  (context-calibrator-list :documentation "TODO" :initarg :context-calibrator-list)))

(defun make-int-data-encoding (&optional (size-bits 8) (encoding 'UNSIGNED) (change-threshold nil) (default-calibrator nil) (context-calibrator-list))
  (valid-integer-encoding-p encoding)
  (check-type size-bits integer)
  (assert (plusp size-bits) (size-bits) "size-bits ~A must be a positive integer" size-bits)
  (make-instance 'integer-data-encoding
                 :size-bits size-bits
                 :encoding encoding
                 :change-threshold change-threshold
                 :context-calibrator-list context-calibrator-list))

(make-int-data-encoding 8 'unsigned)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass numeric-calibrator () ((name :documentation "Optional name" :initarg :name)
                                 (short-description :documentation "Optional description" :initarg :short-description)
                                 (ancilliary-data-set :documentation "Optional additional information") :initarg :ancilliary-data-set))

(defclass spline-calibrator (numeric-calibrator) ((order :initarg order)
                                                  (extrapolate :initarg extrapolate)
                                                  (spline-point-list :initarg spline-point-list :type spline-point-list])
                                                  (ancilliary-data-set :initarg ancillary-data-set)))

(defclass spline-point () ((order :initarg :order :type integer)
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

(defclass polynomial-calibrator (numeric-calibrator) ((polynmial-term-list :type polynomial-term-list :initarg polynomial-term-list)))

(defclass polynomial-term-type () ((coefficient :initarg coefficient :type integer )
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
(defclass context-calibrator () ((context-match)
                                 (calibrator)))

(defclass context-match () ())

(defclass comparison (context-match) ((parameter-ref :initarg :parameter-ref)
                                      (instance :initarg :instance)
                                      (use-calibrated-value :initarg :use-calibrated-value)
                                      (comparison-operator :initarg :comparison-operator)
                                      (value :initarg :value)))

(defun make-comparison (parameter-ref value &optional (instance 0) (use-calibrated-value t) (comparison-operator '=))
  (make-instance 'comparison :parameter-ref parameter-ref :instance instance :use-calibrated-value use-calibrated-value :value value))

(deftype comparison-list ()
  '(satisfies comparison-list-p))

(defun comparison-list-p (l)
  (and (listp l) (every (lambda (i) (typep i 'comparison) ) l)))

(defclass boolean-expression (context-match) ((expression :initarg expression :documentation "a predicate")))

(defclass custom-algorithm (context-match) ((expression :initarg expression :documentation "LISP expression")))

(typep *TEST* 'comparison-list)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defstruct string-data-encoding 
  bit-order
  size-bits
  encoding
  byte-order
  error-detection)

(defstruct binary-data-encoding 
  bit-order
  size_bits
  encode
  error-detection)


;;;;;;;;;;;;;;;;
(defclass float-data-encoding () ((bit-order :documentation "Bit-Order" :initarg :bit-order)
                                  (byte-order :documentation "Byte-order" :initarg :byte-order)
                                  (size-bits :documentation "Size in bits" :initarg :size-bits)
                                  (encoding :documentation "<BIG | LITTLE>" :initarg :encoding)
                                  (change-threshold :documentation "change-threshold" :initarg :change-threshold)
                                  (error-detection :documentation "Error detection algorithm" :initarg :error-detection)
                                  (default-calibrator :documentation "Default calibrator" :initarg :default-calibrator)
                                  (context-calibrator-list :initarg :context-calibrator-list)))

(defun valid-float-size-bits (size)
  (if (member size '(32 64 128 'non-32))
      t))

(valid-float-size-bits 31)

(defun make-float-data-encoding (bit-order size-bits &optional (byte-order 'BIG) (encoding 'IEE) (change-threshold nil) error-detection default-calibrator context-calibrator-list)
  (valid-bit-order-p bit-order)
  (or (context-calibrator-list-p context-calibrator-list) (null context-calibrator-list))
  (check-type size-bits integer)
  (assert (> size-bits 0) (size-bits) "size-bits ~A must be a positive integer" size-bits)
  (make-instance 'float-data-encoding
                 :bit-order bit-order
                 :byte-order byte-order
                 :size-bits size-bits
                 :encoding encoding
                 :error-detection error-detection
                 :default-calibrator default-calibrator
                 :change-threshold change-threshold
                 :context-calibrator-list context-calibrator-list))

(make-float-data-encoding 'big -1)
(make-float-data-encoding 'big 0.5)
(make-float-data-encoding 'bigl -1)
(make-float-data-encoding 'big 1)

;;;;;;;;;;;;;;;;;
(defun valid-bit-order-p (bit-order)
  (assert (member bit-order '(BIG LITTLE)) (bit-order) "Bit Order ~A is not <Big | Little>" bit-order) t)

(defun valid-float-encoding-p (enc)
  (assert (member enc '(IEE MIL)) (enc) "Encoding ~A is not <IEE | MIL>" enc) t)

(defun valid-string-encoding-p (enc)
  (assert (member enc '(UTF-8 UTF-16)) (enc) "String encoding ~A is not <UTF-8 | UTF-16" enc) t)

(valid-int-encoding-p 'unsigned)
(valid-string-encoding-p 'UTF-8)

(make-sequence '(vector bit) 10)

(ql:quickload "lisp-binary")
