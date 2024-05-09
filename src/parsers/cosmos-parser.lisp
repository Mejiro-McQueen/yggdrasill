(ql:quickload :serapeum)
(ql:quickload :cl-ppcre)
(ql:quickload :alexandria)

;; This is a proof of concept for a COSMOS -> yggdrasill translator
;; We parse the COSMOS dictionary format and write s-expressions that compose a yggdrasill dictionary by utilizing quasi quoting
;; I am abandoning this in favor of writing a new dictionary from scratch since 
;; COSMOS dictionaries do not take full advantage of XTCE's features.
;; If this ever picks up steam, we can use parsers like this to help projects quickly test yggdrasill before committing to writing new dictionaries

(defun collect-blocks (lines)
  (let ((accumulator ())
		(current ())
		(tokens ())
		(command nil))
  (dolist (line lines)
	(setq tokens (serapeum:tokens line))
	(setq command (car tokens))
	(when (equal command "COMMAND")
	  (if (not (null current))
		  (progn 
			(push (nreverse current) accumulator)
			(setq current nil))))
	(when tokens
	  (push line current)))
	(push (nreverse current) accumulator)
	accumulator))
	
(defstruct cosmos-telemetry
  target
  command
  description
  endianess)

(defstruct cosmos-command
  target
  command
  description
  endianess)

(defun identify-blocks (blocks)
  (mapcar #'identify blocks))

(defun clean-line (line)
  (let* ((chunks (uiop:split-string line :separator "\"")))
	(concatenate 'list (serapeum:tokens (serapeum:trim-whitespace (car chunks)))  (concatenate 'list (cdr chunks)))))

(defun identify (blk)
  (let* ((header (car blk))
		 (header-type (car (serapeum:tokens header)))
		 (parameters (cdr blk)))
	(cond
	  ((equal header-type "COMMAND")
		(command.process header parameters))
	  ((equal header-type "TELEMETRY")
	   (telemetry.process header parameters)))))

(defun telemetry.process (header parameter))

(defun command.process (header parameters)
  (make-command 
   :command (command.process.header header)
   :parameters (command.process.parameters parameters)))


(defun command.process.header (header)
  (setq header (clean-line header))
  (apply #'make-cosmos-command
		 (list :target (make-symbol (nth 1 header))
			   :command (make-symbol (nth 2 header))
			   :endianess (make-symbol (nth 3 header))
			   :description (or (nth 4 header) ""))))

(defun command.process.parameters (parameters)
  (mapcar #'command.process.parameter parameters))

(defun command.process.parameter (parameter)
  (let* ((args-list (uiop:split-string parameter :separator "\""))
		 (simple-tokens (serapeum:tokens (car args-list)))
		 (trailing-string-tokens (cdr args-list))
		 (name (intern (nth 1 simple-tokens)))
		 (bit-size (parse-integer (serapeum:trim-whitespace (nth 2 simple-tokens))))
		 (data-type (intern (serapeum:trim-whitespace (nth 3 simple-tokens)))))
	(cond 
	  ((eq (or 'UINT 'FLOAT 'DERIVED 'STRING 'BLOCK) data-type)
	   (let* ((min (intern (serapeum:trim-whitespace (nth 4 simple-tokens))))
			  (max (intern (serapeum:trim-whitespace (nth 5 simple-tokens))))
			  (default (if (alexandria:starts-with-subseq "0x" (nth 6 simple-tokens))
						   (parse-integer (subseq (nth 6 simple-tokens) 2) :radix 16)
						   (parse-integer (nth 6 simple-tokens))))
			  (description (car trailing-string-tokens))
			  (endianess (nth 2 trailing-string-tokens))
			  (command-parameter-numerical (make-command.parameter.numerical
											:min min
											:max max
											:default default
											:description description
											:endianess (if (serapeum:blankp endianess)
														   nil
														   (intern endianess)))))
		 (make-command.parameter :name name
								 :bit-size bit-size
								 :data-type data-type
								 :remaining-parameters command-parameter-numerical)))
	  ((eq (or 'STRING 'BLOCK) data-type)
	   (print (nth 3 trailing-string-tokens))
	   (let* ((default (car trailing-string-tokens))
			  (description (nth 2 trailing-string-tokens))
			  (endianess (nth 3 trailing-string-tokens))
			  (command-parameter-string-block
				(make-command.parameter.string-block :default default
													 :description description
													 :endianess (if (serapeum:blankp endianess)
																	nil
																	(intern endianess)))))
	   (make-command.parameter :name name
							   :bit-size bit-size
							   :data-type data-type
							   :remaining-parameters command-parameter-string-block)))
	  (t (print "UNKNOWN")))))

(defstruct command command parameters)

(defstruct command.parameter name bit-size data-type remaining-parameters)

(defstruct command.parameter.numerical description min max default endianess)

(defstruct command.parameter.string-block default description endianess)



(defun main (filepath)
  (let* ((data (uiop:read-file-lines filepath))
		 (data-blocks (collect-blocks data))
		 (structs (identify-blocks data-blocks)))
	
  structs
  ))


(defun dump-command (command)
  (let ((types ()))
	(dolist (i (slot-value command 'parameters))
	  (push (generate-type i) types)
	  )
	(initialize-system 'NOS-3 :telemetry-metadata `((make-telemetry-metadata :parameter-type-set ,types )))
	)
  )

(defun generate-type (parameter)
  (typecase (command.parameter-remaining-parameters parameter)
	  (command.parameter.numerical
	   	(let ((signed (if (equal 'UINT (command.parameter-data-type parameter)) t nil)))
		  `(make-integer-parameter-type
			,(slot-value parameter 'name)
			:short-description ,(slot-value (slot-value parameter 'remaining-parameters) 'description)
			:signed ,signed
			:size-in-bits ,(slot-value parameter 'bit-size))))
	  (command.parameter.string-block
	   ;The concept of endianess for strings doesn't really make sense.
	   ;Didn't we see a mission where the string was stored backwards in the array?
	   ;Might be useful when decoding pascal strings?
	   `(make-string-parameter-type
		 ,(slot-value parameter 'name)
		 :short-description ,(slot-value (slot-value parameter 'remaining-parameters) 'description)
		 :initial-value ,(slot-value (slot-value parameter 'remaining-parameters) 'default)
		 :size-in-bits ,(slot-value parameter 'bit-size)))))

(defun initialize-system (system-name &key root description telemetry-metadata command-metadata space-system-list)
  `(make-space-system ,system-name :root ,root :description ,description :telemetry-metadata ,@telemetry-metadata)
  )


;;; Quick Test
(defvar data (uiop:read-file-lines "~/Desktop/nos3/gsw/cosmos/build/openc3-cosmos-nos3/targets/TO_DEBUG/cmd_tlm/TO_DEBUG_CMD.txt"))
(clean-line "COMMAND CFS TO_DEBUG_REMOVE_ALL_PKT_CC BIG_ENDIAN \"Remove all packets\"")
(defvar blocks (collect-blocks data))
(initialize-system 'NOS-3)
(defparameter test (main "~/Desktop/nos3/gsw/cosmos/build/openc3-cosmos-nos3/targets/TO_DEBUG/cmd_tlm/TO_DEBUG_CMD.txt"))
(dump-command (car test))

