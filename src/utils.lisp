(in-package :xtce)

(defun print-bin (n)
  (let ((pad (hex-length-in-bits n)))
	(format nil "~v,'0b" pad n)))

(defun print-hex (n)
  (format nil "~X" n))

(defun hex-length-in-bits (hex-val)
  "Use to count how large a frame is in bits"
  ;integer-length only counts significant bits (i.e. #x5555 = 010101... resulting in 15)  
  (let ((number-of-hex-characters (length (format nil "~X" hex-val)))
		(bits-per-hex 4))
	(if hex-val
		(* bits-per-hex number-of-hex-characters)
		0)))

(defun truncate-from-left (data bits)
  (let ((length-from-lsb (- (integer-length data) bits)))
	(ldb (byte length-from-lsb 0) data)))

(defun truncate-from-left-to-size (data bits)
  (truncate-from-left data (- (integer-length data) bits)))

(defun ldb-left (size position integer)
  "Extract (size) bits starting at MSB bit (position) from integer
   Example: 
     ldb-msb (16 0 #xABCD) = #xABCD
     ldb-msb (4 2) = #xA
     ldb-msb (4 4) = #xC
"
  (let* ((msb (hex-length-in-bits integer))
		 (msb-pos (- msb position size)))
	(ldb (byte size msb-pos) integer)))

(defun hamming-distance (integer-1 integer-2)
  (logcount (logxor integer-1 integer-2)))

(defun format-bool (a)
  (if a "True" "False"))

(defun format-symbol (a)
  (if a
	  (symbol-name a)))

(defun format-number (n)
  (format nil "~A" n))

(defun prompt-new-value (prompt)
  (format *query-io* prompt) 
  (force-output *query-io*)  
  (read *query-io*))
