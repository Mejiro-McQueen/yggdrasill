(in-package :xtce)

(defclass numeric-calibrator ()
  ((name :documentation "Optional name"
         :initarg :name)
   (short-description :documentation "Optional description"
                      :initarg :short-description)
   (ancillary-data-set :documentation "Optional additional information"
                       :initarg :ancillary-data-set)))

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

(defun make-comparison (parameter-ref value &optional (instance 0) (use-calibrated-value t) (comparison-operator 'equal))
  (make-instance 'comparison
                 :parameter-ref parameter-ref
                 :instance instance
                 :use-calibrated-value use-calibrated-value
                 :value value
				 :comparison-operator comparison-operator))

(defmethod cxml-marshall ((obj comparison))
  (with-slots (parameter-ref value instance use-calibrated-value comparison-operator) obj
	(cxml:with-element* ("xtce" "Comparison")
	  (cxml:attribute "parameterRef" (format-symbol parameter-ref))
	  (cxml:attribute "value" value)
	  (optional-xml-attribute "instance" instance)
	  (optional-xml-attribute "useCalibratedValue" (format-bool use-calibrated-value))
	  (optional-xml-attribute "comparisonOperator" (format-symbol comparison-operator)))))

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
  ((term-list :type term-list
              :initarg :term-list)))

(defclass term()
  ((coefficient :initarg :coefficient
                :type integer )
   (exponent :initarg :exponent
             :type float)))

(defmethod cxml-marshall ((obj term))
  (with-slots (coefficient exponent) obj
    (cxml:with-element* ("xtce" "Term")
      (cxml:attribute "coefficient" (format nil "~A" coefficient))
      (cxml:attribute "exponent" (format nil "~A" exponent)))))

(defmethod cxml::unparse-attribute ((obj term))
  (with-slots (coefficient exponent) obj
    "OK"))

(defun make-term (&key coefficient exponent)
  (check-type coefficient number)
  (check-type exponent number)
  (assert (>= exponent 0) (exponent) "Exponent value ~A must be non-negative, use math-operation-calibrator" exponent)
  (make-instance 'term :coefficient coefficient
                       :exponent exponent))

(defclass term-list (xtce-list) ())
                              
(defun make-term-list (&rest terms)
  (assert (>= (length terms) 1) (terms) "Parameter TERMS: ~A must have at least one polynomial term." terms)
  (dolist (i terms)
    (check-type i term))
  (make-instance 'term-list :items terms))

(defclass ancillary-data-set () ())

(defun make-polynomial-calibrator (&key name short-description ancillary-data-set term-list)
  (check-type term-list term-list)
  (check-optional-type name symbol)
  (check-optional-type short-description string)
  (check-optional-type ancillary-data-set ancillary-data-set)
  (make-instance 'polynomial-calibrator :term-list term-list
                                        :name name
                                        :ancillary-data-set ancillary-data-set
                                        :short-description short-description))

(defmethod cxml-marshall ((obj polynomial-calibrator))
  (with-slots (term-list name short-description ancillary-data-set) obj
    (cxml:with-element* ("xtce" "PolynomialCalibrator")
      (if name (cxml:attribute "name" (format nil "~A" name)))
      (if short-description (cxml:attribute "shortDescription" short-description))
      (if ancillary-data-set (cxml-marshall ancillary-data-set))
      (cxml-marshall term-list))))

