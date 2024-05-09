(in-package :xtce)

(define-condition non-unique-key (error)
  ((key :initarg :key :accessor key)
   (value :initarg :value :accessor value)
   (table :initarg :table :accessor table))
  (:report (lambda (condition stream) (format stream "key: ~a, already exists in table: ~a, with value: ~a"
										 (key condition) (table condition) (value condition)))))

(defun make-filesystem-hash-table (&key root)
  "ARGUMENTS:
     root: If true, signifies that this table is a root filesystem"
  (let ((res (make-hash-table :test 'equal)))
	(when root
	  (link-filesystem-hash-tables res res "/")
	  (setf (gethash "/" res) res))
	res))

(defun add-unique-key (key value table)
  ;Used to contain symbols, but it devolves intro string comparison when making uninterned symbols, since uinterned is guaranteed never to eq
  ;Can not intern symbol since the entire point is to keep symbols from clashing in different space systems
  ;Just use strings to keep things simple.
  ;The current usecase only needs strings
  (check-type key string)
  (check-type table hash-table)
  (when (gethash key table)
	(error 'non-unique-key :key key :value value :table table))
  (setf (gethash key table) value))

(defun link-filesystem-hash-tables (root-table child-table child-table-key)
  "Link a table to a root table"
  (setf (gethash "../" child-table) root-table)
  (setf (gethash "./" child-table) child-table)
  (add-unique-key child-table-key child-table root-table)
  (let ((root (gethash "/" root-table)))
	(setf (gethash "/" child-table) root)))

(defun find-key-by-path (requested-key current-table)
  (unless current-table
	;;#+ *debug-mode*
	;;(print "No Hash Table Found: Are your references broken?")
	(return-from find-key-by-path nil))
  
  (labels ((format-path (string-list)
			 (reduce (lambda (str1 str2)
					   (concatenate 'string str1 "/" (if (equalp str2 :BACK) "../" str2))) string-list :initial-value ".")))
	(multiple-value-bind (flag path-components file) (uiop::split-unix-namestring-directory-components requested-key)
	  (let* ((target file)
			 (match-in-current-table? (gethash target current-table))
			 (parent-table (gethash "../" current-table))
			 (root-table (gethash "/" current-table))
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
		   ;#+ *DEBUG-MODE*
		   ;(print 'ABSOLUTE)
										;Force Recursion
		   (return-from find-key-by-path (find-key-by-path next-requested-key root-table)))

		  (:relative
		   (when (member :BACK path-components)
			; #+ *DEBUG-MODE*
			 ;(print 'Go-Back)
			 (when (equal current-table root-table)
			   (warn "Cycle detected: Attempted to go to parent table but ended up at the same place"))
			 (return-from find-key-by-path (find-key-by-path next-requested-key parent-table)))

		   (when path-components
			 ;#+ *DEBUG-MODE*
			 ;(print 'Keep-Looking)
			 (return-from find-key-by-path (find-key-by-path next-requested-key
															 (gethash (car path-components) current-table))))

		   (when match-in-current-table?
			 ;#+ *DEBUG-MODE*
			 ;(print 'GET!)
			 (return-from find-key-by-path match-in-current-table?))

		   (unless match-in-current-table?
			 ;#+ *DEBUG-MODE*
			 ;(print 'Path-Exhausted-No-Match)
			 nil)))))))

