(in-package :xtce)

(defclass numeric-calibrator ()
  ((name :documentation "Optional name" :initarg :name)
   (short-description :documentation "Optional description" :initarg :short-description)
   (ancillary-data-set :documentation "Optional additional information" :initarg :ancillary-data-set)))

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

