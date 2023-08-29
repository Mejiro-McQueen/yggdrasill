(defpackage :filesystem-hash-table
  (:use :cl)
  (:documentation "Hash Tables accesibible by unix path names")
  (:export #:add-unique-key
           #:find-key-by-path
		   #:make-filesystem-hash-table
           #:register-filesystem-hash-table
		   ))

(in-package :filesystem-hash-table)

(define-condition non-unique-key (error)
  ((key :initarg :key :accessor key)
   (value :initarg :value :accessor value)
   (table :initarg :table :accessor table))
  (:report (lambda (condition stream) (format stream "key: ~a, already exists in table: ~a, with value: ~a"
										 (key condition) (table condition) (value condition)))))

(defun make-filesystem-hash-table (&key root)
  "ARGUMENTS:
     root: If true, signifies that this table is a root filesystem"
  (let ((res (make-hash-table)))
	(when root
	  (register-filesystem-hash-table res res '/)
	  (setf (gethash (intern "/") res) res))
	res))

(defun add-unique-key (key value table)
  (check-type key symbol)
  (check-type table hash-table)
  (when (gethash key table)
	(error 'non-unique-key :key key :value value :table table))
  (setf (gethash key table) value))

(defun register-filesystem-hash-table (root-table table table-key)
  "Add and register a table to a root table"
  (setf (gethash (intern "../") table) root-table)
  (setf (gethash (intern "./") table) table)
  (add-unique-key table-key table root-table)
  (let ((root (gethash (intern "/") root-table)))
	(setf (gethash (intern "/") table) root)))

(defun find-key-by-path (requested-key current-table)
  (unless current-table
	#+ *debug-mode*
	(print "No Hash Table Found: Are your references broken?")
	(return-from find-key-by-path nil))
  
  (labels ((format-path (string-list)
			 (reduce (lambda (str1 str2)
					   (concatenate 'string str1 "/" (if (equalp str2 :BACK) "../" str2))) string-list :initial-value ".")))
	(multiple-value-bind (flag path-components file) (uiop::split-unix-namestring-directory-components requested-key)
	  (let* ((target (intern file))
			 (match-in-current-table? (gethash target current-table))
			 (parent-table (gethash (intern "../") current-table))
			 (root-table (gethash (intern "/") current-table))
			 (next-requested-key (format-path (append (cdr path-components) (list (format nil "~A" target))))))

		;; #+*DEBUG-MODE*
		;; (PROGN
		;;   (print (format nil "~%"))
		;;   (print (format nil "Flag: ~A" flag))
		;;   (print (format nil "Target: ~A" target))
		;;   (print (format nil "Requested-Key: ~A" requested-key))
		;;   (print (format nil "Next-Key: ~A" next-requested-key))
		;;   (print (format nil "Target Components: ~A" path-components))
		;;   (print (format nil "Current Table Keys: ~A" (alexandria:hash-table-keys current-table))))
	
		(case flag
		  (:absolute
		   #+ *DEBUG-MODE*
		   (print 'ABSOLUTE)
		   ;Enforce Recursion
		   (return-from find-key-by-path (find-key-by-path next-requested-key root-table)))

		  (:relative
		   (when (member :BACK path-components)
			 #+ *DEBUG-MODE*
			 (print 'Go-Back)
			 (when (equal current-table root-table)
			   (warn "Cycle detected: Attempted to go to parent table but ended up at the same place"))
			 (return-from find-key-by-path (find-key-by-path next-requested-key parent-table)))

		   (when path-components
			 #+ *DEBUG-MODE*
			 (print 'Keep-Looking)
			 (return-from find-key-by-path (find-key-by-path next-requested-key
															 (gethash (intern (car path-components)) current-table))))

		   (when match-in-current-table?
			 #+ *DEBUG-MODE*
			 (print 'GET!)
			 (return-from find-key-by-path match-in-current-table?))

		   (unless match-in-current-table?
			 #+ *DEBUG-MODE*
			 (print 'Path-Exhausted-No-Match)
			 nil)))))))


(loop for s being the external-symbols of (find-package "FILESYSTEM-HASH-TABLE")
  collect s)

(print *package*)

(defmacro with-fixture-hash-table-tree (&body body)
  `(let* ((TEST (make-filesystem-hash-table :root t))
		  (SPICA (make-filesystem-hash-table))
		  (SPICA-1 (make-filesystem-hash-table))
		  (SPICA-2 (make-filesystem-hash-table))
		  (SPICA-Injured (make-filesystem-hash-table))
		  (SPICA-Tenno-Sho (make-filesystem-hash-table))
		  (SPICA-Tenno-Sho-Spring (make-filesystem-hash-table))
		  (SPICA-Tenno-Sho-Autumn (make-filesystem-hash-table))
		  (SYMBOLI (make-filesystem-hash-table))
		  (MEJIRO (make-filesystem-hash-table))
		  (MEJIRO-1 (make-filesystem-hash-table))
		  (MEJIRO-2 (make-filesystem-hash-table))
		  (MEJIRO-3 (make-filesystem-hash-table)))

										;TEST
	 (add-unique-key '|Admire| 'VEGA TEST)
	 (add-unique-key '|Machikane| 'TANEHAUSER TEST)
	 (add-unique-key '|Manhattan| 'CAFE TEST)
	   
										;SPICA
	 (register-filesystem-hash-table TEST SPICA 'SPICA)
	 (add-unique-key '|SPICA-Tenno-Sho| SPICA-Tenno-Sho SPICA)
	
										; SPICA-1
	 (register-filesystem-hash-table SPICA SPICA-1 'SPICA-1)
	 (add-unique-key '|Special| 'WEEK SPICA-1)
	 (add-unique-key '|Silence| 'SUZUKA SPICA-1)
	 (add-unique-key '|Mejiro| 'MCQUEEN SPICA-1)

										; SPICA-2
	 (register-filesystem-hash-table SPICA SPICA-2 'SPICA-2)
	 (add-unique-key '|Vodka| 'VODKA SPICA-2)
	 (add-unique-key '|Gold|  'SHIP SPICA-2)
	 (add-unique-key '|Daiwa| 'SCARLET SPICA-2)
	 (add-unique-key '|Tokai| 'TEIO SPICA-2)

										;SPICA-Injured (Disconnected-Table)
	 (add-unique-key' |Tokai| '|Teio-Injured| SPICA-Injured)
	 (add-unique-key' |Mejiro| '|McQueen-Injured| SPICA-Injured)

										;SPICA-Tenno-Sho
	 (register-filesystem-hash-table SPICA SPICA-Tenno-Sho 'SPICA-Tenno-Sho)
	 (add-unique-key '|SPICA-Tenno-Sho-Spring| SPICA-Tenno-Sho-Spring Spica-Tenno-Sho)
	 (add-unique-key '|SPICA-Tenno-Sho-Autumn| SPICA-Tenno-Sho-Autumn Spica-Tenno-Sho)
	 (add-unique-key '|Vodka| '|Vodka-Contender| Spica-Tenno-Sho)
	 (add-unique-key '|Mejiro|  '|Mejiro-McQueen-Contender| Spica-Tenno-Sho)
	 (add-unique-key '|Gold|  '|Gold-Ship-Contender| Spica-Tenno-Sho)
	 (add-unique-key '|Daiwa| '|Daiwa-Scarlet-Contender| Spica-Tenno-Sho)

										;SPICA-Tenno-Sho-Spring
	 (register-filesystem-hash-table SPICA-Tenno-Sho SPICA-Tenno-Sho-Spring 'SPICA-Tenno-Sho-Spring)
	 (add-unique-key '|Tokai| '|Teio-Spring-Champion| SPICA-Tenno-Sho-Spring)
	 (add-unique-key '|Mejiro| '|Mejiro-McQueen-Spring-Champion| SPICA-Tenno-Sho-Spring)
	 (add-unique-key '|Gold| '|Ship-Spring-Champion| SPICA-Tenno-Sho-Spring)

										;SPICA-Tenno-Sho-Autumn
	 (register-filesystem-hash-table SPICA-Tenno-Sho SPICA-Tenno-Sho-Autumn 'SPICA-Tenno-Sho-Autumn)
	 (add-unique-key '|Vodka| '|Vodka-Autumn-Champion| SPICA-Tenno-Sho-Autumn)
	 (add-unique-key '|Mejiro| '|Mejiro-McQueen-Autumn-Champion| SPICA-Tenno-Sho-Autumn)
	 (add-unique-key '|Gold| '|Gold-Ship-Autumn-Champion| SPICA-Tenno-Sho-Autumn)
	 
										;SYMBOLI
	 (register-filesystem-hash-table TEST SYMBOLI 'SYMBOLI)
	 (add-unique-key '|KRIS| 'KRIS SYMBOLI)
	 (add-unique-key '|RUDOLF| '|Symboli-Rudolf| SYMBOLI)
	 (add-unique-key '|SIRIUS| 'SIRIUS SYMBOLI)
	   
										;MEJIRO
	 (register-filesystem-hash-table TEST MEJIRO 'MEJIRO)
	 (add-unique-key '|McQueen| '|Mejiro-McQueen| MEJIRO)
	 (add-unique-key '|Palmer| 'PALMER MEJIRO)
	 (add-unique-key '|Ramonu| 'RAMONU MEJIRO)
	   
										;MEJIRO-1
	 (register-filesystem-hash-table MEJIRO MEJIRO-1 'MEJIRO-1)
	 (add-unique-key '|Ardan| 'ARDAN  MEJIRO-1)
	 (add-unique-key '|Bright| 'BRIGHT  MEJIRO-1)
	 
										;MEJIRO-2
	 (register-filesystem-hash-table MEJIRO MEJIRO-2 'MEJIRO-2)
	 (add-unique-key'|Dober| 'DOBER MEJIRO-2)
	   
										;MEJIRO-3
	 (register-filesystem-hash-table MEJIRO MEJIRO-3 'MEJIRO-3)
	 (add-unique-key'|Ryan| 'RYAN MEJIRO-3)
	 (progn ,@body)))

(with-fixture-hash-table-tree
  (find-key-by-path "./SPICA-1/Special" SPICA)
  ;(alexandria:hash-table-keys TEST)
  ;(gethash '/ TEST)
  )

(defparameter *DEBUG-MODE* t)
