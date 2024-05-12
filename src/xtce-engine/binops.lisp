(in-package :xtce-engine)

(defun byte-array-to-uint (a)
  (parse-integer (byte-array-to-hex-string a) :radix 16))

(defun uint->hex (a)
  (format nil "0x~4,'0X" a ))

(defun byte-array-to-hex-string (a)
  (ironclad:byte-array-to-hex-string a))

(defun ascii-string-to-byte-array (s)
    (when (find #\space s)
	  (setf s (remove #\space s)))
  (ironclad::ascii-string-to-byte-array s))

;; (defun byte-array->hex-string (a)
;;   (ironclad:byte-array-to-hex-string
;;    (make-array (length a)
;; 			   :element-type '(unsigned-byte 8)
;; 			   :initial-contents a)))

(defun hex-string-to-byte-array (s)
  (when (find #\space s)
	(setf s (remove #\space s)))
  (ironclad::hex-string-to-byte-array s))

(defun hex-string-to-bit-vector (s)
  (u8-array->bit-vector (hex-string-to-byte-array s)))

(defun U8-Array->bit-vector (a)
  (xtce-engine::bit-array-list->concat-bit-vector
   (map 'list #'(lambda (i) (uint->bit-vector i 8)) a)))

(defun bit-array-list->concat-bit-vector (l)
  (apply #'concatenate-bit-arrays l))

(defun alist->bit-vector (l)
  (bit-array-list->concat-bit-vector (mapcar #'cdr l)))

(defun new-bit-vector () (make-array 1 :element-type 'bit :adjustable t :fill-pointer 0))

(defun concatenate-bit-arrays (&rest rest)
  (apply #'concatenate 'bit-vector rest))

(defun invert (bit)
  (declare (type bit bit))
  (logxor bit 1))

(defun bit-vector->twos-complement->integer (v)
  ;;(declare (optimize (speed 3) (safety 0)))
  (let ((neg (equal 1 (bit v 0)))
		(res nil))
	(if neg
		(setf res (map 'string #'digit-char (bit-not v)))
		(setf res (map 'string #'(lambda (bit) (digit-char bit)) v)))

	(setf res (parse-integer res :radix 2))
	;;(setf res (format nil "~b" res))
	(when neg
	  (setf res (- (+ res #b1))))
	res
	))

(defun uint->bit-vector (n &optional (pad (integer-length n))) 
  "~43 HAYAI!. Hex strings are represented as UINT in CL, so #x29A0 = 666 = #*1010011010
   Where n is an integer or a CL hex value
"
  ;;(declare (optimize (speed 3) (safety 0)))
  (let ((pos (- pad 1))
		(res (make-sequence 'bit-vector pad :initial-element 0)))
	(loop while (> n 0)
		  do
			 (setf (aref res pos) (logand n 1))
			 (setf n (ash n -1))
			 (setf pos (- pos 1)))
	res))

(defun bit-vector->twos-complement->dec (v)
  ;;(declare (optimize (speed 3) (safety 0)))
  (let ((neg (equal 1 (bit v 0)))
		(res nil))
	(if neg
		(setf res (map 'string #'digit-char (bit-not v)))
		(setf res (map 'string #'(lambda (bit) (digit-char bit)) v)))
	(setf res (parse-integer res :radix 2))
	(when neg
	  (setf res (- (+ res #b1))))
	res))

(defun bit-vector->ones-complement->dec (v)
  ;;(declare (optimize (speed 3) (safety 0)))
  (let ((neg (equal 1 (bit v 0)))
		(res nil))
	(if neg
		(setf res (map 'string #'digit-char (bit-not v)))
		(setf res (map 'string #'(lambda (bit) (digit-char bit)) v)))
	(setf res (parse-integer res :radix 2))
	(when neg
	  (setf res (- res)))
	res))

(defun bit-vector->uint (v)
  (parse-integer (map 'string #'digit-char v) :radix 2))

(defun bit-vector->hex (v)
  (let* ((uint (bit-vector->uint v))
		 (hex (format nil "0x~X" uint)))
	hex))

(defun twos-complement-representable-p (n bits)
  (let ((max (expt 2 (- bits 1))))
	(and (< n max) (>= n (- max)))))

(defun dec->twos-complement (integer pad)
  ;;(declare (optimize (speed 3) (safety 0)))
  (assert (twos-complement-representable-p integer pad) (integer pad) "Insufficient bits to represent this integer.")
  (if (< integer 0)
	  (uint->bit-vector (+ #b1 (bit-vector->uint (bit-not (uint->bit-vector (abs integer) pad)))) pad)
	  (pad-bit-vector (uint->bit-vector integer) pad)))

(defun dec->ones-complement (integer pad)
  (if (< integer 0)
	  (bit-not (uint->bit-vector (abs integer) pad)
			   (uint->bit-vector integer pad))))

(defun pad-bit-vector (v pad &key (pad-element 0) (position :left))
  (assert (or (equal position :left) (equal position :right)) (position) "~A is an invalid value: Position must be one of: :right :left" position)
  (if (< (length v) pad)
		(case position
		  (:left
		   (concatenate-bit-arrays (make-sequence 'bit-vector (- pad (length v)) :initial-element pad-element) v))
		  (:right
		   (concatenate-bit-arrays v (make-sequence 'bit-vector (- pad (length v)) :initial-element pad-element))))
		v))

(defun bit-vector->sign-mag->dec (v)
  (let* ((sign (bit v 0))
		 (neg (equal 1 sign))
		 (res nil))
	(setf (bit v 0) 0)
	(setf res (bit-vector->uint v))
	(if neg
		(- res)
		res)))

(defun dec->sign-mag (n pad)
  (let ((res (uint->bit-vector (abs n) pad)))
	(when (< n 0)
	  (setf (bit res 0) 1))
	res
	))

(defvar packedBCD-Table
  '((1 . #*0000)
	(2 . #*0001)
	(3 . #*0010)
	(4 . #*0100)
	(5 . #*0101)
	(6 . #*0110)
	(7 . #*0111)
	(8 . #*1000)
	(9 . #*1001)))

(defun bcd->dec (v byte-size)
  (loop for i from 0 to (length v) by byte-size
		while (< i (- (length v) (- byte-size 1)))
		collect (first(rassoc (subseq v i (+ i byte-size)) packedBCD-Table :test 'equal)) into res
		finally (return (mapcar #'identity res))))

(defun digit-list->integer (l)
  (reduce #'(lambda (acc digit) (+ (* acc 10) digit)) l))

(defun dec->bcd (n &optional (pad 4))
  (let ((digit-list (integer->digit-list n)) )
	(apply 'concatenate-bit-arrays (mapcar #'(lambda (digit) (pad-bit-vector (cdr (assoc digit packedBCD-Table :test 'equal)) pad)) digit-list))))

(defun integer->digit-list (n)
  (loop while (> n 1)
		collect (rem n 10 )
		do
		   (setf n (floor (/ n 10)))))
