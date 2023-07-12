(in-package :xtce)

(defclass encoding () ())

(defun valid-integer-encoding-p (enc)
  (assert (member enc '(unsigned sign-magnitude twos-compliment ones-compliment bcd packed-bcd)) (enc)
          "Int Encoding ~A is not <Unsigned | twos-compliment | ones-compliment | bcd | packed-bcd" enc) 
  t)

(defclass integer-data-encoding (encoding)
  ((encoding :documentation "Specifies integer numeric value to raw encoding method." :initarg :encoding)
   (size-in-bits :documentation "Number of bits to use for the raw encoding." :initarg :size-bits :type integer)
   (change-threshold :documentation "Error detection algorithm" :initarg :change-threshold)
   (default-calibrator :documentation "TODO" :initarg :default-calibrator)
   (context-calibrator-list :documentation "TODO" :initarg :context-calibrator-list)))

(defun make-integer-data-encoding (&optional (size-bits 8)
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
                 :context-calibrator-list context-calibrator-list
                 :default-calbrator default-calibrator))

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
                                                         (byte-order-list)))

(defun valid-string-encoding-p (enc)
  (let ((valid-encodings '(US-ASCII WINDOWS-1252 ISO-UTF-8 UTF-16 UTF-16LE UTF-16BE UTF-32 UTF-32LE UTF-32BE) )) 
    (assert (member enc valid-encodings) (enc) "String encoding ~A is not one of: ~A" enc valid-encodings)) t)



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
  (check-type size-in-bits integer)
  (assert (> size-in-bits 0) (size-in-bits) "size-in-bits ~A must be a positive integer" size-in-bits)
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
