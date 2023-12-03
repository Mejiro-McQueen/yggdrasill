(ql:quickload "bifrost-yggdrasill")
(in-package :nasa-cfs)

(defparameter NASA-cFS
  (make-space-system
   '|NASA-cFS|
   :root t
   :short-description
   "Root system for the NASA-cFS"

   :telemetry-metadata
   (make-telemetry-metadata
	:stream-set
	(stc:with-ccsds.aos.stream 1024 (list))

	:container-set
	(stc::with-ccsds.aos.containers ())
	
	:parameter-type-set
	(stc::with-ccsds.aos.header.types
		(stc::with-ccsds.mpdu.types
			(stc:with-ccsds.space-packet.header.types
				(list
				 (make-array-parameter-type
				  '|APPDATA-Type|
				  '|/U64-Type|
				  :short-description "Array of registered application table data."
				  :dimension-list (list (make-dimension
										 (make-starting-index (make-fixed-value 0))
										 (make-ending-index (make-fixed-value 15)))))

				 (make-array-parameter-type
				  '|APRESULTS-Type|
				  '|/U8-Type|
				  :short-description "Packed actionpoint results data, 4 bits per actionpoint."
				  :dimension-list (list (make-dimension
										 (make-starting-index (make-fixed-value 0))
										 (make-ending-index (make-fixed-value 91)))))

				 (make-array-parameter-type
				  '|ATPFREEBYTES-Type|
				  '|/U32-Type|
				  :short-description "Free Bytes in each ATS."
				  :dimension-list (list (make-dimension
										 (make-starting-index (make-fixed-value 0))
										 (make-ending-index (make-fixed-value 7)))))

				 (make-array-parameter-type
				  '|App-Mon-Enables-Type|
				  '|/U32-Type|
				  :dimension-list (list (make-dimension
										 (make-starting-index (make-fixed-value 0))
										 (make-ending-index (make-fixed-value 1)))))

				 (make-array-parameter-type
				  '|BYTECOUNT-Type|
				  '|/U16-Type|
				  :short-description "Number of bytes of data specified by table."
				  :dimension-list (list (make-dimension
										 (make-starting-index (make-fixed-value 0))
										 (make-ending-index (make-fixed-value 7)))))

				 (make-array-parameter-type
				  '|COUNTDOWN-Type|
				  '|/U16-Type|
				  :short-description "Current value of countdown timer."
				  :dimension-list (list (make-dimension
										 (make-starting-index (make-fixed-value 0))
										 (make-ending-index (make-fixed-value 7)))))

				 (make-array-parameter-type
				  '|DATA-Type|
				  '|/U8-Type|
				  :short-description "Dwell data ( number of bytes varies up to MD_DWELL_TABLE_SIZE *4)."
				  :dimension-list (list (make-dimension
										 (make-starting-index (make-fixed-value 0))
										 (make-ending-index (make-fixed-value 99)))))

				 (make-array-parameter-type
				  '|DWELLPKTOFFSET-Type|
				  '|/U16-Type|
				  :short-description "Current write offset within dwell pkt data region."
				  :dimension-list (list (make-dimension
										 (make-starting-index (make-fixed-value 0))
										 (make-ending-index (make-fixed-value 7)))))

				 (make-array-parameter-type
				  '|DWELLTBLADDRCOUNT-Type|
				  '|/U16-Type|
				  :short-description "Number of dwell addresses in table."
				  :dimension-list (list (make-dimension
										 (make-starting-index (make-fixed-value 0))
										 (make-ending-index (make-fixed-value 7)))))

				 (make-array-parameter-type
				  '|DWELLTBLENTRY-Type|
				  '|/U16-Type|
				  :short-description "Next dwell table entry to be processed."
				  :dimension-list (list (make-dimension
										 (make-starting-index (make-fixed-value 0))
										 (make-ending-index (make-fixed-value 7)))))

				 (make-array-parameter-type
				  '|ENG_SPARE|
				  '|/U8-Type|
				  :short-description "3 Spare bytes."
				  :dimension-list (list
								   (make-dimension
									(make-starting-index (make-fixed-value 0))
									(make-ending-index (make-fixed-value 2)))))

				 (make-array-parameter-type
				  '|ENTRYSTATES-Type|
				  '|/U16-Type|
				  :short-description "" 
				  :dimension-list (list (make-dimension
										 (make-starting-index (make-fixed-value 0))
										 (make-ending-index (make-fixed-value 127)))))

				 (make-array-parameter-type
				  '|MSGIDS-Type|
				  '|/U16-Type|
				  :short-description ""
				  :dimension-list (list (make-dimension
										 (make-starting-index (make-fixed-value 0))
										 (make-ending-index (make-fixed-value 999)))))

				 (make-array-parameter-type
				  '|NUMWAITSPERPKT-Type|
				  '|/U16-Type|
				  :short-description "Numer of delay counts in table."
				  :dimension-list (list (make-dimension
										 (make-starting-index (make-fixed-value 0))
										 (make-ending-index (make-fixed-value 7)))))

				 (make-array-parameter-type
				  '|PERFFILTERMASK-Type|
				  '|/U32-Type|
				  :short-description "Current Setting of Performance Analyzer Filter Masks."
				  :dimension-list (list (make-dimension
										 (make-starting-index (make-fixed-value 0))
										 (make-ending-index (make-fixed-value 3)))))

				 (make-array-parameter-type
				  '|PERFTRIGGERMASK-Type|
				  '|/U32-Type|
				  :short-description "Current Setting of Performance Analyzer Trigger Masks."
				  :dimension-list (list (make-dimension
										 (make-starting-index (make-fixed-value 0))
										 (make-ending-index (make-fixed-value 3)))))

				 (make-array-parameter-type
				  '|RTSDISABLEDSTATUS-Type|
				  '|/U16-Type|
				  :short-description "RTS disabled status bit map where each uint16 represents 16 RTS numbers. 
Note: array index numbers and bit numbers use base zero indexing, but RTS numbers use base one indexing. Thus, the LSB (bit zero) of uint16 array index zero represents RTS number 1, and bit one of uint16 array index zero represents RTS number 2,etc. If an RTS is ENABLED, then the corresponding bit is zero. If an RTS is DISABLED, then the corresponding bit is one."
				  :dimension-list (list (make-dimension
										 (make-starting-index (make-fixed-value 0))
										 (make-ending-index (make-fixed-value 9)))))

				 (make-array-parameter-type
				  '|RTSEXECUTINGSTATUS-Type|
				  '|/U16-Type|
				  :short-description "RTS executing status bit map where each uint16 represents 16 RTS numbers.
      Note: array index numbers and bit numbers use base zero indexing, but RTS numbers
      use base one indexing. Thus, the LSB (bit zero) of uint16 array index zero represents
      RTS number 1, and bit one of uint16 array index zero represents RTS number 2,
      etc. If an RTS is IDLE, then the corresponding bit is zero. If an RTS is EXECUTING,
      then the corresponding bit is one."
				  :dimension-list (list (make-dimension
										 (make-starting-index (make-fixed-value 0))
										 (make-ending-index (make-fixed-value 9)))))

				 (make-array-parameter-type
				  '|SPARE-Type|
				  '|/U8-Type|
				  :short-description "Structure padding."
				  :dimension-list (list (make-dimension
										 (make-starting-index (make-fixed-value 0))
										 (make-ending-index (make-fixed-value 1)))))

				 (make-array-parameter-type
				  '|WPRESULTS-Type|
				  '|/U8-Type|
				  :short-description "Packed watchpoint results data, 2 bits per watchpoint."
				  :dimension-list (list (make-dimension
										 (make-starting-index (make-fixed-value 0))
										 (make-ending-index (make-fixed-value 47)))))

				 (make-binary-parameter-type
				  '|16-Bit-Checksum-Type|
				  :short-description
				  "16 bit checksum")

				 (make-binary-parameter-type
				  '|32-Bit-Checksum-Type|
				  :short-description
				  "32 bit checksum")

				 (make-enumerated-parameter-type
				  '|ATPSTATE-Type|
				  :enumeration-list
				  (list
				   (make-enumeration '|IDLE| 2)
				   (make-enumeration '|EXECUTING| 5)))

				 (make-enumerated-parameter-type
				  '|ATSNUMBER-Type|
				  :enumeration-list
				  (list
				   (make-enumeration '|ATS-A| 1)
				   (make-enumeration '|ATS-B| 2)))

				 (make-enumerated-parameter-type
				  '|on-off-enum-type|
				  :short-description "On/Off enumeration."
				  :enumeration-list (list (make-enumeration 'ON 1) (make-enumeration 'OFF 0)))

				 (make-float-parameter-type
				  '|Battery-Voltage-Type|
				  :short-description "Battery voltage type."
				  :size-in-bits 32
				  :unit-set (list (make-unit :description "Volts" :form 'raw)))

				 (make-float-parameter-type
				  '|F32-Type|
				  :short-description
				  "32 bit float.")

				 (make-float-parameter-type
				  '|F64-Type|
				  :short-description
				  "64 bit float."
				  :size-in-bits 64)

				 (make-integer-parameter-type
				  '|U16-Type|
				  :short-description "Unsigned 16 bit integer"
				  :signed nil
				  :size-in-bits 16)


				 (make-integer-parameter-type
				  '|U32-Type|
				  :short-description "Unsigned 32 bit integer"
				  :signed nil
				  :size-in-bits 32)

				 (make-integer-parameter-type
				  '|U64-Type|
				  :short-description
				  "Unsigned 64 bit integer."
				  :signed nil
				  :size-in-bits 64)

				 (make-integer-parameter-type
				  '|U8-Type|
				  :short-description "Unsigned 8bit integer with no encoding or calibration."
				  :signed nil
				  :size-in-bits 8)

				 (make-string-parameter-type
				  '|ASCII-String-Type|
				  :short-description "ASCII string")))))

	:parameter-set
	(stc::with-ccsds.mpdu.parameters
		(stc::with-ccsds.aos.header.parameters
			(stc::with-ccsds.space-packet.header.parameters
				(list 
				 (make-parameter '|ACTIVEAPS| '|/U16-Type| :short-description "How many actionpoints are currently active.")
				 (make-parameter '|ACTIVEBUFFER| '|/U8-Type| :short-description "Indicator of whether table buffer validated was 0=Inactive, 1=Active.") ;TODO: Change to enum type
				 (make-parameter '|ADDRCOUNT| '|/U8-Type| :short-description "Number of addresses being sent - 1..MD_DWELL_TABLE_SIZE valid.")
				 (make-parameter '|ADDRESS| '|/U32-Type| :short-description "Fully resolved address used for last command.")
				 (make-parameter '|APPCSERRCOUNTER| '|/U16-Type| :short-description "App miscompare counter.")
				 (make-parameter '|APPCSSTATE| '|/U8-Type| :short-description "CS App table checksum state.")
				 (make-parameter '|APPDATA| '|APPDATA-Type| :short-description "Array of registered application table data.")
				 (make-parameter '|APPENABLESTATE| '|/U8-Type| :short-description "Application enable/disable state.")
				 (make-parameter '|APPENDBYTECOUNT| '|/U16-Type| :short-description "Size of cmd entries in current Append ATS table.")
				 (make-parameter '|APPENDCMDARG| '|/U16-Type| :short-description "ATS selection argument from most recent Append ATS command.")
				 (make-parameter '|APPENDENTRYCOUNT| '|/U16-Type| :short-description "Number of cmd entries in current Append ATS table.")
				 (make-parameter '|APPENDLOADCOUNT| '|/U16-Type| :short-description "Total number of Append ATS table loads.")
				 (make-parameter '|APPMONENABLES| '|App-Mon-Enables-Type| :short-description "Enable states of App Monitor Entries.")
				 (make-parameter '|APP_BUFFERPOOLHANDLE| '|/U32-Type| :short-description "")
				 (make-parameter '|APP_CHANNUM| '|/U32-Type| :short-description "")
				 (make-parameter '|APP_CLASS| '|/ASCII-String-Type| :short-description "")
				 (make-parameter '|APP_CONDCODE| '|/U32-Type| :short-description "")
				 (make-parameter '|APP_DSTFILE| '|/ASCII-String-Type| :short-description "")
				 (make-parameter '|APP_ENGINECYCLECOUNT| '|/U32-Type| :short-description "")
				 (make-parameter '|APP_LASTFAILEDTRANS| '|/ASCII-String-Type| :short-description "")
				 (make-parameter '|APP_LOWMEMORYMARK| '|/U32-Type| :short-description "")
				 (make-parameter '|APP_MAXMEMNEEDED| '|/U32-Type| :short-description "")
				 (make-parameter '|APP_MEMALLOCATED| '|/U32-Type| :short-description "")
				 (make-parameter '|APP_MEMINUSE| '|/U32-Type| :short-description "")
				 (make-parameter '|APP_NODETYPE| '|/U32-Type| :short-description "")
				 (make-parameter '|APP_PDUSRECEIVED| '|/U32-Type| :short-description "")
				 (make-parameter '|APP_PDUSREJECTED| '|/U32-Type| :short-description "")
				 (make-parameter '|APP_PEAKMEMINUSE| '|/U32-Type| :short-description "")
				 (make-parameter '|APP_PRIORITY| '|/U32-Type| :short-description "")
				 (make-parameter '|APP_QNODESALLOCATED| '|/U32-Type| :short-description "")
				 (make-parameter '|APP_QNODESDEALLOCATED| '|/U32-Type| :short-description "")
				 (make-parameter '|APP_SOURCE| '|/U32-Type| :short-description "")
				 (make-parameter '|APP_SRCENTITYID| '|/ASCII-String-Type| :short-description "")
				 (make-parameter '|APP_SRCFILE| '|/ASCII-String-Type| :short-description "")
				 (make-parameter '|APP_STATUS| '|/U32-Type| :short-description "")
				 (make-parameter '|APP_TOTALABANDONTRANS| '|/U32-Type| :short-description "")
				 (make-parameter '|APP_TOTALCOMPLETEDTRANS| '|/U32-Type| :short-description "")
				 (make-parameter '|APP_TOTALFAILEDTRANS| '|/U32-Type| :short-description "")
				 (make-parameter '|APP_TOTALINPROGTRANS| '|/U32-Type| :short-description "")
				 (make-parameter '|APP_TOTALSUCCESSTRANS| '|/U32-Type| :short-description "")
				 (make-parameter '|APP_TRANSNUM| '|/U32-Type| :short-description "")
				 (make-parameter '|APP_WAKEUPFORFILEPROC| '|/U32-Type| :short-description "")
				 (make-parameter '|APRESULTS| '|APRESULTS-Type| :short-description "Packed actionpoint results data, 4 bits per actionpoint.")
				 (make-parameter '|APSAMPLECOUNT| '|/U32-Type| :short-description "Total count of Actionpoints sampled.")
				 (make-parameter '|ATPCMDNUMBER| '|/U32-Type| :short-description "current command number")
				 (make-parameter '|ATPFREEBYTES| '|ATPFREEBYTES-Type| :short-description "Free Bytes in each ATS.")
				 (make-parameter '|ATPSTATE| '|ATPSTATE-Type| :short-description "'current ATP state valid values are: 2 = IDLE, 5 = EXECUTING'")
				 (make-parameter '|ATSCMDCTR| '|/U16-Type| :short-description "Total ATS cmd cnter counts commands sent by the ATS.")
				 (make-parameter '|ATSCMDERRCTR| '|/U16-Type| :short-description "Total ATS cmd Error ctr command errors in the ATS.")
				 (make-parameter '|ATSNUMBER| '|ATSNUMBER-Type| :short-description "current ATS number 1 = ATS A, 2 = ATS B")
				 (make-parameter '|AUTOSUSPEND_ENFLAG| '|/U32-Type| :short-description "")
				 (make-parameter '|AUTOSUSPEND_LOWFREEMAR| '|/U32-Type| :short-description "")
				 (make-parameter '|BADTABLEDATACOUNT| '|/U16-Type| :short-description "'# of times corrupted table entries were processed'")
				 (make-parameter '|BAT_VOLTAGE| '|Battery-Voltage-Type| :short-description "Battery Voltage (Sine Wave)")
				 (make-parameter '|BOOTSOURCE| '|/U32-Type| :short-description "Boot source ( as provided from BSP ).")
				 (make-parameter '|BYTECOUNT-Array| '|BYTECOUNT-Type| :short-description "Number of bytes of data specified by table.")
				 (make-parameter '|BYTECOUNT| '|/U16-Type| :short-description "Number of bytes of dwell data contained in packet.")
				 (make-parameter '|BYTESPROCESSED| '|/U32-Type| :short-description "Bytes processed for last command.")
				 (make-parameter '|CFECOREBASELINE| '|/U32-Type| :short-description "Basline checksum for the cFE core.")
				 (make-parameter '|CFECORECHECKSUM| '|/U16-Type|  :short-description "Checksum of cFE Core Code.")
				 (make-parameter '|CFECORECSERRCOUNTER| '|/U16-Type| :short-description "cFE core miscompare counter.")
				 (make-parameter '|CFECORECSSTATE| '|/U8-Type| :short-description "cFE Core code segment checksum state.")
				 (make-parameter '|CFEMAJORVERSION| '|/U8-Type| :short-description "Major Version Number of cFE.")
				 (make-parameter '|CFEMINORVERSION| '|/U8-Type| :short-description "Minor Version Number of cFE.")
				 (make-parameter '|CFEMISSIONREVISION| '|/U8-Type| :short-description "Mission Version Number of cFE.")
				 (make-parameter '|CFEREVISION| '|/U8-Type| :short-description "Sub-Minor Version Number of cFE.")
				 (make-parameter '|CHECKSUMSTATE| '|/U8-Type| :short-description "CS Application global checksum state.")
				 (make-parameter '|CHILDCMDCOUNTER| '|/U8-Type| :short-description "Child task command counter.")
				 (make-parameter '|CHILDCMDERRCOUNTER| '|/U8-Type| :short-description "Child task command error counter.")
				 (make-parameter '|CHILDCMDWARNCOUNTER| '|/U8-Type| :short-description "Child task command warning counter.")
				 (make-parameter '|CHILDCURRENTCC| '|/U8-Type| :short-description "Command code currently executing.")
				 (make-parameter '|CHILDPREVIOUSCC| '|/U8-Type| :short-description "Command code previously executed.")
				 (make-parameter '|CHILDQUEUECOUNT| '|/U8-Type| :short-description "Number of pending commands in queue.")
				 (make-parameter '|CHILDTASKINUSE| '|/U8-Type| :short-description " CS 'Child Task In Use' flag.")
				 (make-parameter '|CLOCKSTATEAPI| '|/U16-Type| :short-description "API State.")
				 (make-parameter '|CLOCKSTATEFLAGS| '|/U16-Type| :short-description "State Flags.")
				 (make-parameter '|CMDCTR| '|/U8-Type| :short-description "Counts Ground Requests.")
				 (make-parameter '|CMDERRCTR| '|/U8-Type| :short-description "Counts Request Errors.")
				 (make-parameter '|COMBINEDPACKETSSENT| '|/U16-Type| :short-description "Count of combined tlm pkts sent.")
				 (make-parameter '|COMMANDCOUNTER| '|U8-Type| :short-description "EVS Command Counter.")
				 (make-parameter '|COND_CANCELNUM| '|/U8-Type| :short-description "")
				 (make-parameter '|COND_FILECHECKSUMNUM| '|/U8-Type| :short-description "")
				 (make-parameter '|COND_FILESIZENUM| '|/U8-Type| :short-description "")
				 (make-parameter '|COND_FILESTOREREJNUM| '|/U8-Type| :short-description "")
				 (make-parameter '|COND_INACTIVENUM| '|/U8-Type| :short-description "")
				 (make-parameter '|COND_NAKLIMITNUM| '|/U8-Type| :short-description "")
				 (make-parameter '|COND_POSACKNUM| '|/U8-Type| :short-description "")
				 (make-parameter '|COND_SUSPENDNUM| '|/U8-Type| :short-description "")
				 (make-parameter '|CONTINUEATSONFAILUREFLAG| '|/U8-Type| :short-description "In the event of ATS execution failure (ats command fails checksum) , the ATS execution will continue if this flag is set to TRUE and will stop if this flag is set to FALSE.")
				 (make-parameter '|COUNTDOWN| '|COUNTDOWN-Type| :short-description "Current value of countdown timer.")
				 (make-parameter '|CREATEPIPEERRCNT| '|/U8-Type| :short-description "Count of errors in create pipe API.")
				 (make-parameter '|CURRENTALIVENESSSTATE| '|/U8-Type| :short-description "Status of HS Aliveness Indicator.")
				 (make-parameter '|CURRENTAPPMONSTATE| '|/U8-Type| :short-description "Status of HS Critical Application Monitor.")
				 (make-parameter '|CURRENTCPUHOGSTATE| '|/U8-Type| :short-description "Status of HS Hogging Indicator.")
				 (make-parameter '|CURRENTCSTABLE| '|/U16-Type| :short-description "Current table being checksummed.")
				 (make-parameter '|CURRENTENTRYINTABLE| '|/U16-Type| :short-description "Current entry ID in the table being checksummed.")
				 (make-parameter '|CURRENTEVENTMONSTATE| '|/U8-Type| :short-description "Status of HS Critical Events Monitor.")
				 (make-parameter '|CURRENTLCSTATE| '|/U8-Type| :short-description "Current LC application operating state.")
				 (make-parameter '|Command-Counter-8| '|/U8-Type| :short-description "EVS Command Counter.")
				 (make-parameter '|Command-Error-Counter-8| '|/U8-Type| :short-description "EVS Command Error Counter.")
				 (make-parameter '|DATAVALUE| '|/U32-Type| :short-description "Last command data value -- may be fill pattern or peek/poke value.")
				 (make-parameter '|DATA| '|DATA-Type| :short-description "Dwell data ( number of bytes varies up to MD_DWELL_TABLE_SIZE *4).")
				 (make-parameter '|DESTTBLERRCOUNTER| '|/U8-Type| :short-description "Count of failed attempts to get table data pointer.")
				 (make-parameter '|DESTTBLLOADCOUNTER| '|/U8-Type| :short-description "Count of destination file table loads.")
				 (make-parameter '|DIRNAME| '|/U8-Type| :short-description "Directory Name.")
				 (make-parameter '|DISABLEDPKTCOUNTER| '|/U16-Type| :short-description "Count of packets discarded (DS was disabled).")
				 (make-parameter '|DUPSUBSCRIPTIONSCNT| '|/U8-Type| :short-description "Count of duplicate subscriptions.")
				 (make-parameter '|DWELLENABLEDMASK| '|/U16-Type| :short-description "Each bit in bit mask enables a table 0x0001=TBL1 enable bit,0x0002=TBL2 enable bit, 0x0004=TBL3 enable bit,0x0008=TBL4 enable bit, etc.") ;TODO: Explore new types
				 (make-parameter '|DWELLPKTOFFSET| '|DWELLPKTOFFSET-Type| :short-description "Current write offset within dwell pkt data region.")
				 (make-parameter '|DWELLTBLADDRCOUNT| '|DWELLTBLADDRCOUNT-Type| :short-description "Number of dwell addresses in table.")
				 (make-parameter '|DWELLTBLENTRY| '|DWELLTBLENTRY-Type| :short-description "Next dwell table entry to be processed.")
				 (make-parameter '|EEPROMBASELINE| '|/U32-Type| :short-description "Baseline checksum for all of Eeprom.")
				 (make-parameter '|EEPROMCSERRCOUNTER| '|/U16-Type| :short-description "Eeprom miscompare counter.")
				 (make-parameter '|EEPROMCSSTATE| '|/U8-Type| :short-description "CS Eeprom table checksum state.")
				 (make-parameter '|ENG_ATTEMPTS| '|/U32-Type| :short-description "")
				 (make-parameter '|ENG_CHECKSUM| '|/U32-Type| :short-description "")
				 (make-parameter '|ENG_CONDCODE| '|/U32-Type| :short-description "")
				 (make-parameter '|ENG_DELICODE| '|/U32-Type| :short-description "")
				 (make-parameter '|ENG_DSTFILE| '|/ASCII-String-Type| :short-description "")
				 (make-parameter '|ENG_FDLENGTH| '|/U32-Type| :short-description "")
				 (make-parameter '|ENG_FDOFFSET| '|/U32-Type| :short-description "")
				 (make-parameter '|ENG_FILESIZE| '|/U32-Type| :short-description "")
				 (make-parameter '|ENG_FINALSTAT| '|/U32-Type| :short-description "")
				 (make-parameter '|ENG_FLAGS| '|/U32-Type| :short-description "")
				 (make-parameter '|ENG_FLIGHTENGINEENTITYID| '|/ASCII-String-Type| :short-description "")
				 (make-parameter '|ENG_MACHINESALLOCATED| '|/U32-Type| :short-description "")
				 (make-parameter '|ENG_MACHINESDEALLOCATED| '|/U32-Type| :short-description "")
				 (make-parameter '|ENG_NAKS| '|/U8-Type| :short-description "")
				 (make-parameter '|ENG_PARTLEN| '|/U8-Type| :short-description "")
				 (make-parameter '|ENG_PARTVAL| '|/U8-Type| :short-description "")
				 (make-parameter '|ENG_PHASE| '|/U8-Type| :short-description "")
				 (make-parameter '|ENG_RCVDFILESIZE| '|/U32-Type| :short-description "")
				 (make-parameter '|ENG_ROLE| '|/U32-Type| :short-description "")
				 (make-parameter '|ENG_SRCFILE| '|/ASCII-String-Type| :short-description "")
				 (make-parameter '|ENG_STARTTIME| '|/U32-Type| :short-description "")
				 (make-parameter '|ENG_STATE| '|/U32-Type| :short-description "")
				 (make-parameter '|ENG_TMPFILE| '|/ASCII-String-Type| :short-description "")
				 (make-parameter '|ENG_TRANSLEN| '|/U8-Type| :short-description "")
				 (make-parameter '|ENG_TRANSNUM| '|/U32-Type| :short-description "")
				 (make-parameter '|ENG_TRANSVAL| '|/U8-Type| :short-description "")
				 (make-parameter '|ENTRYSTATES| '|ENTRYSTATES-Type| :short-description "States of each Schedule Entry.")
				 (make-parameter '|ERLOGENTRIES| '|/U32-Type| :short-description "Number of entries made in the ER Log since the power on.")
				 (make-parameter '|ERLOGINDEX| '|/U32-Type| :short-description "Current index of the ER Log (wraps around).")
				 (make-parameter '|ERRCOUNTER| '|/U8-Type|  :short-description "The ES Application Command Error Counter.")
				 (make-parameter '|ES_CMDCOUNTER| '|/U8-Type| :short-description "The ES Application Command Counter.")
				 (make-parameter '|ES_ERRCOUNTER| '|/U8-Type| :short-description "The ES Application Command Error Counter.")
				 (make-parameter '|EVENTSMONITOREDCOUNT| '|/U16-Type| :short-description "Total count of Event Messages Monitored by the Critical Events Monitor.")
				 (make-parameter '|FAILEDVALCTR| '|/U8-Type| :short-description "Total number of unsuccessful table validations.")
				 (make-parameter '|FILENAME| '|/ASCII-String-Type| :short-description "Name of file.")
				 (make-parameter '|FILESIZE| '|/U32-Type| :short-description "File Size.")
				 (make-parameter '|FILESTATUS| '|/U32-Type| :short-description "Status indicating whether the file is open or closed.")
				 (make-parameter '|FILEUPDATECOUNTER| '|/U16-Type| :short-description "Count of good updates to secondary header.")
				 (make-parameter '|FILEUPDATEERRCOUNTER| '|/U16-Type| :short-description "Count of bad updates to secondary header.")
				 (make-parameter '|FILEWRITECOUNTER| '|/U16-Type| :short-description "Count of good destination file writes.")
				 (make-parameter '|FILEWRITEERRCOUNTER| '|/U16-Type| :short-description "Count of bad destination file writes.")
				 (make-parameter '|FILTEREDPKTCOUNTER| '|/U32-Type| :short-description "Count of packets discarded (failed filter test).")
				 (make-parameter '|FILTERTBLERRCOUNTER| '|/U8-Type| :short-description "Count of failed attempts to get table data pointer.")
				 (make-parameter '|FILTERTBLLOADCOUNTER| '|/U8-Type| :short-description "Count of packet filter table loads.")
				 (make-parameter '|FIRSTFILE| '|/U32-Type| :short-description "Index into directory files of first packet file.")
				 (make-parameter '|HEAPBLOCKSFREE| '|/U32-Type| :short-description "Number of free blocks remaining in the OS heap.")
				 (make-parameter '|HEAPBYTESFREE| '|/U32-Type| :short-description "Number of free bytes remaining in the OS heap.")
				 (make-parameter '|HEAPMAXBLOCKSIZE| '|/U32-Type| :short-description "Number of bytes in the largest free block.")
				 (make-parameter '|HEATERSTATUS| '|/on-off-enum-type| :short-description "1 = ON, 0 = OFF")
				 (make-parameter '|IGNOREDPKTCOUNTER| '|/U32-Type| :short-description "Count of packets discarded. Incoming packets will be discarded when:.")
				 (make-parameter '|IGNOREMAJORFRAME| '|/U8-Type| :short-description "Major Frame too noisy to trust.")
				 (make-parameter '|INTERNALERRCNT| '|/U8-Type| :short-description "Count of queue read or write errors.")
				 (make-parameter '|INVALIDCMDCNTR| '|/U8-Type| :short-description "Count of invalid commands received.")
				 (make-parameter '|INVALIDEVENTMONCOUNT| '|/U32-Type| :short-description "Total count of Invalid Event Monitors Monitored by the Critical Events Monitor.")
				 (make-parameter '|LASTACTION| '|/U8-Type| :short-description "Last command action executed.")
				 (make-parameter '|LASTATSERRCMD| '|/U16-Type| :short-description "Last ATS Errant Command Num.")
				 (make-parameter '|LASTATSERRSEQ| '|/U16-Type| :short-description "'Last ATS Errant Sequence Num Values: 1 or 2.'")
				 (make-parameter '|LASTFILEDUMPED| '|/ASCII-String-Type| :short-description "Path and Name of last file dumped to.")
				 (make-parameter '|LASTFILELOADED| '|/ASCII-String-Type| :short-description "Path and Name of last table image file loaded.")
				 (make-parameter '|LASTMODIFIEDTIME| '|/U32-Type| :short-description "Last Modification Time of File.")
				 (make-parameter '|LASTONESHOTADDRESS| '|/U32-Type| :short-description "Address used in last one shot checksum command.")
				 (make-parameter '|LASTONESHOTCHECKSUM| '|/U32-Type| :short-description "Checksum of the last one shot checksum command.")
				 (make-parameter '|LASTONESHOTSIZE| '|/U32-Type| :short-description "Size used in the last one shot checksum command.")
				 (make-parameter '|LASTRTSERRCMD| '|/U16-Type| :short-description "The OFFSET in the RTS buffer of the command that had an error It will be a WORD value i.e. 1st command had an error, this value would be 0, if the 2nd command started at int8 10 in the buffer, this value would be 5.")
				 (make-parameter '|LASTRTSERRSEQ| '|/U16-Type| :short-description "Last RTS Errant Sequence Num.")
				 (make-parameter '|LASTSYNCMETSLOT| '|/U16-Type| :short-description "Slot number where Time Sync last occurred.")
				 (make-parameter '|LASTTABLELOADED| '|/ASCII-String-Type| :short-description "Name of the last table loaded.")
				 (make-parameter '|LASTUPDATEDTBL| '|/ASCII-String-Type| :short-description "Name of the last table updated.")
				 (make-parameter '|LASTUPDATETIME_SECONDS| '|/U32-Type| :short-description "")
				 (make-parameter '|LASTUPDATETIME_SUBSECONDS| '|/U32-Type| :short-description "")
				 (make-parameter '|LASTVALCRC| '|/32-Bit-Checksum-Type| :short-description "Data Integrity Value computed for last table validated.")
				 (make-parameter '|LASTVALSTATUS| '|/U8-Type| :short-description "Returned status from validation function for last table validated.")
				 (make-parameter '|LASTVALTABLENAME| '|/ASCII-String-Type| :short-description "Name of last table validated.")
				 (make-parameter '|LEAPSECONDS| '|/U16-Type| :short-description "Current Leaps Seconds.")
				 (make-parameter '|LOGENABLED| '|/U8-Type| :short-description "Current event log enable/disable state.")
				 (make-parameter '|LOGFULLFLAG| '|/U8-Type| :short-description "Local event log full flag.")
				 (make-parameter '|LOGMODE| '|/U8-Type| :short-description "Local event logging mode (overwrite/discard).")
				 (make-parameter '|LOGOVERFLOWCOUNTER| '|/U8-Type| :short-description "Local event log overflow counter.")
				 (make-parameter '|MAJORFRAMESOURCE| '|/U8-Type| :short-description "Major Frame Signal source identifier.")
				 (make-parameter '|MAXPROCESSORRESETS| '|/U32-Type| :short-description "Max processor resets before a power on is done.")
				 (make-parameter '|MAXRESETS| '|/U16-Type| :short-description "HS Maximum Processor Reset Count.")
				 (make-parameter '|MEMINUSE| '|/U32-Type| :short-description "Memory in use.")
				 (make-parameter '|MEMORYCSERRCOUNTER| '|/U16-Type| :short-description "Memory miscompare counter.")
				 (make-parameter '|MEMORYCSSTATE| '|/U8-Type| :short-description "CS Memory table checksum state.")
				 (make-parameter '|MEMPOOLHANDLE| '|/U32-Type| :short-description "Handle to TBL's memory pool.")
				 (make-parameter '|MEMTYPE| '|/U8-Type| :short-description "Memory type for last command.")
				 (make-parameter '|MESSAGEFORMATMODE| '|/U8-Type| :short-description "Event message format mode (short/long).")
				 (make-parameter '|MESSAGESENDCOUNTER| '|/U8-Type| :short-description "Event message send counter.")
				 (make-parameter '|MESSAGETRUNCCOUNTER| '|/U8-Type| :short-description "Event message truncation counter.")
				 (make-parameter '|MESSAGE| '|/ASCII-String-Type| :short-description "Event message string.")
				 (make-parameter '|MINORFRAMESSINCETONE| '|/U16-Type| :short-description "'# of Minor Frames since last Major Frame tone'")
				 (make-parameter '|MISSEDMAJORFRAMECOUNT| '|/U32-Type| :short-description "'# of missing Major Frame tones'")
				 (make-parameter '|MISSINGDATACTR| '|/U16-Type| :short-description "Number of times missing data was detected.")
				 (make-parameter '|MONITOREDMSGCOUNT| '|/U32-Type| :short-description "Total count of messages monitored for watchpoints.")
				 (make-parameter '|MSGACTEXEC| '|/U32-Type| :short-description "Number of Software Bus Message Actions Executed.")
				 (make-parameter '|MSGIDS| '|/U8-Type| :short-description "Message ID of msg associated with each entry.")
				 (make-parameter '|MSGLIMERRCNT| '|/U16-Type| :short-description "Count of msg id to pipe errors.")
				 (make-parameter '|MSGRECEIVEERRCNT| '|/U8-Type| :short-description "Count of message receive errors.")
				 (make-parameter '|MSGSENDERRCNT| '|/U8-Type| :short-description "Count of message send errors.")
				 (make-parameter '|MULTIPLESLOTSCOUNT| '|/U16-Type| :short-description "Number of times that multiple slots processed.")
				 (make-parameter '|NEXTATSTIME| '|/U32-Type| :short-description "Next ATS Command Time (seconds).")
				 (make-parameter '|NEXTRTSTIME| '|/U32-Type| :short-description "next RTS cmd Absolute Time")
				 (make-parameter '|NEXTSLOTNUMBER| '|/U16-Type| :short-description "Next Minor Frame to be processed.")
				 (make-parameter '|NOSUBSCRIBERSCNT| '|/U8-Type| :short-description "Count pkts sent with no subscribers.")
				 (make-parameter '|NUMFREESHAREDBUFS| '|/U8-Type| :short-description "Number of free Shared Working Buffers.")
				 (make-parameter '|NUMLOADPENDING| '|/U16-Type| :short-description "Number of Tables pending on Applications for their update.")
				 (make-parameter '|NUMOPENFILES-32| '|/U32-Type| :short-description "Number of files opened via cFE.")
				 (make-parameter '|NUMOPENFILES-8| '|/U8-Type| :short-description "Number of open files in the system.")
				 (make-parameter '|NUMRTSACTIVE| '|/U16-Type| :short-description "number of RTSs currently active")
				 (make-parameter '|NUMTABLES| '|/U16-Type| :short-description "Number of Tables Registered.")
				 (make-parameter '|NUMVALREQUESTS| '|/U8-Type| :short-description "Number of times Table Services has requested validations from Apps.")
				 (make-parameter '|NUMWAITSPERPKT| '|NUMWAITSPERPKT-Type| :short-description "Number of delay counts in table.")
				 (make-parameter '|ONESHOTTASKINUSE| '|/U8-Type| :short-description "CS 'OneShot Task In Use' flag.")
				 (make-parameter '|ON_ORBIT_ENUM| '|/U8-Type| :short-description "FSW Enumeration") ;TODO: Investigate correct type
				 (make-parameter '|OSALMAJORVERSION| '|/U8-Type| :short-description "OS Abstraction Layer Major Version Number.")
				 (make-parameter '|OSALMINORVERSION| '|/U8-Type| :short-description "OS Abstraction Layer Minor Version Number.")
				 (make-parameter '|OSALMISSIONREVISION| '|/U8-Type| :short-description "OS Abstraction Layer MissionRevision Number.")
				 (make-parameter '|OSALREVISION| '|/U8-Type| :short-description "OS Abstraction Layer Revision Number.")
				 (make-parameter '|OSBASELINE| '|/U32-Type| :short-description "Baseline checksum for the OS code segment.")
				 (make-parameter '|OSCSERRCOUNTER| '|/U16-Type| :short-description "OS code segment miscopmare counter.")
				 (make-parameter '|OSCSSTATE| '|/U8-Type| :short-description "OS code segment checksum state.")
				 (make-parameter '|OUTPUTPORT| '|/U8-Type| :short-description "Output port mask.")
				 (make-parameter '|PACKETFILES| '|/U32-Type| :short-description "Number of files in this packet.")
				 (make-parameter '|PACKETID_APPNAME| '|/ASCII-String-Type| :short-description "")
				 (make-parameter '|PACKETID_EVENTID| '|/U16-Type| :short-description "")
				 (make-parameter '|PACKETID_EVENTTYPE| '|/U16-Type| :short-description "")
				 (make-parameter '|PACKETID_PROCESSORID| '|/U32-Type| :short-description "")
				 (make-parameter '|PACKETID_SPACECRAFTID| '|/U32-Type| :short-description "")
				 (make-parameter '|PASSCOUNTER| '|/U32-Type| :short-description "Number of times CS has passed through all of its tables.")
				 (make-parameter '|PASSEDPKTCOUNTER| '|/U32-Type| :short-description "Count of packets that passed filter test.")
				 (make-parameter '|PASSIVERTSEXECCOUNT| '|/U16-Type| :short-description "Total count of RTS sequences not initiated because the LC state is set to LC_STATE_PASSIVE.")
				 (make-parameter '|PERFDATACOUNT| '|/U32-Type| :short-description "Number of Entries Put Into the Performance Analyzer Log.")
				 (make-parameter '|PERFDATAEND| '|/U32-Type| :short-description "Identifies Last Stored Entry in Performance Analyzer Log.")
				 (make-parameter '|PERFDATASTART| '|/U32-Type| :short-description "Identifies First Stored Entry in Performance Analyzer Log.")
				 (make-parameter '|PERFDATATOWRITE| '|/U32-Type| :short-description "Number of Performance Analyzer Log Entries Left to be Written to Log DumpFile.")
				 (make-parameter '|PERFFILTERMASK| '|PERFFILTERMASK-Type| :short-description "Current Setting of Performance Analyzer Filter Masks.")
				 (make-parameter '|PERFMODE| '|/U32-Type| :short-description "Current mode of Performance Analyzer.")
				 (make-parameter '|PERFSTATE| '|/U32-Type| :short-description "Current state of Performance Analyzer.")
				 (make-parameter '|PERFTRIGGERCOUNT| '|/U32-Type| :short-description "Number of Times Perfomance Analyzer has Triggered.")
				 (make-parameter '|PERFTRIGGERMASK| '|PERFTRIGGERMASK-Type| :short-description "Current Setting of Performance Analyzer Trigger Masks.")
				 (make-parameter '|PIPEOVERFLOWERRCNT| '|/U16-Type| :short-description "Count of pipe overflow errors.")
				 (make-parameter '|PROCESSORRESETS| '|/U32-Type| :short-description "Number of processor resets since last power on.")
				 (make-parameter '|RATE| '|/U32-Type| :short-description "Number of counts between packet sends.")
				 (make-parameter '|REGISTEREDCOREAPPS| '|/U32-Type| :short-description "Number of Applications registered with ES.")
				 (make-parameter '|REGISTEREDEXTERNALAPPS| '|/U32-Type| :short-description "Number of Applications registered with ES.")
				 (make-parameter '|REGISTEREDLIBS| '|/U32-Type| :short-description "Number of Libraries registered with ES.")
				 (make-parameter '|REGISTEREDTASKS| '|/U32-Type| :short-description "Number of Tasks ( main AND child tasks ) registered with ES.")
				 (make-parameter '|RESETSPERFORMED| '|/U16-Type| :short-description "HS Performed Processor Reset Count.")
				 (make-parameter '|RESETSUBTYPE| '|/U32-Type| :short-description "Reset Sub Type.")
				 (make-parameter '|RESETTYPE| '|/U32-Type| :short-description "Reset type ( PROCESSOR or POWERON ).")
				 (make-parameter '|RTSACTIVECTR| '|/U16-Type| :short-description "Increments when an RTS is started without error.")
				 (make-parameter '|RTSACTIVEERRCTR| '|/U16-Type| :short-description "Increments when an attempt to start an RTS fails.")
				 (make-parameter '|RTSCMDCTR| '|/U16-Type| :short-description "Counts TOTAL rts cmds that were sent out from ALL active RTSs.")
				 (make-parameter '|RTSCMDERRCTR| '|/U16-Type| :short-description "Counts TOTAL number of errs from ALL RTSs that are active.")
				 (make-parameter '|RTSDISABLEDSTATUS| '|RTSDISABLEDSTATUS-Type| :short-description "")
				 (make-parameter '|RTSEXECCOUNT| '|/U32-Type| :short-description "Total count of RTS sequences initiated.")
				 (make-parameter '|RTSEXECUTINGSTATUS| '|RTSEXECUTINGSTATUS-Type| :short-description "")
				 (make-parameter '|RTSNUMBER| '|/U16-Type| :short-description "next RTS number")
				 (make-parameter '|SAMESLOTCOUNT| '|/U16-Type| :short-description "'# of times SCH woke up in the same slot as last time'")
				 (make-parameter '|SB_CMDERRCNT| '|/U8-Type| :short-description "SB Count of invalid commands received.")
				 (make-parameter '|SB_COMMANDCNT| '|/U8-Type| :short-description "SB Count of valid commands received.")
				 (make-parameter '|SCHEDULEACTIVITYFAILURECOUNT| '|/U32-Type| :short-description "Number of unsuccessful activities attempted.")
				 (make-parameter '|SCHEDULEACTIVITYSUCCESSCOUNT| '|/U32-Type| :short-description "Number of successfully performed activities.")
				 (make-parameter '|SECONDS1HZADJ| '|/U32-Type| :short-description "Current 1 Hz SCTF adjustment (seconds).")
				 (make-parameter '|SECONDSMET| '|/U32-Type| :short-description "Current MET (seconds).")
				 (make-parameter '|SECONDSSTCF| '|/U32-Type| :short-description "Current STCF (seconds).")
				 (make-parameter '|SKIPPEDSLOTSCOUNT| '|/U16-Type| :short-description "Number of times that slots were skipped.")
				 (make-parameter '|SLOTSPROCESSEDCOUNT| '|/U32-Type| :short-description "'Total # of Schedule Slots (Minor Frames) Processed.'")
				 (make-parameter '|SPARE| '|/U8-Type| :short-description "Structure padding.")
				 (make-parameter '|STATUSFLAGS| '|/U8-Type| :short-description "Internal HS Error States.")
				 (make-parameter '|SUBSCRIBEERRCNT| '|/U8-Type| :short-description "Count of errors in subscribe API.")
				 (make-parameter '|SUBSECS1HZADJ| '|/U32-Type| :short-description "Current 1 Hz SCTF adjustment (sub-seconds).")
				 (make-parameter '|SUBSECSMET| '|/U32-Type| :short-description "Current MET (sub-seconds).")
				 (make-parameter '|SUBSECSSTCF| '|/U32-Type| :short-description "Current STCF (sub-seconds).")
				 (make-parameter '|SUCCESSVALCTR| '|/U8-Type| :short-description "Total number of successful table validations.")
				 (make-parameter '|SWITCHPENDFLAG| '|/on-off-enum-type| :short-description "Is an ats switch pending? 0 = NO, 1 = YES This means that the ATS switch is waiting until a safe time")
				 (make-parameter '|SYNCH| '|/U16-Type| :short-description "")
				 (make-parameter '|SYNCTOMET| '|/U8-Type| :short-description "Status indicating whether slots are synched to MET.")
				 (make-parameter '|SYSLOGBYTESUSED| '|/U32-Type| :short-description "Total number of bytes used in system log.")
				 (make-parameter '|SYSLOGENTRIES| '|/U32-Type| :short-description "Number of entries in the system log.")
				 (make-parameter '|SYSLOGMODE| '|/U32-Type| :short-description "Write/Overwrite Mode.")
				 (make-parameter '|SYSLOGSIZE| '|/U32-Type| :short-description "Total size of the system log.")
				 (make-parameter '|TABLEID| '|/U8-Type| :short-description "TableId from 1 to MD_NUM_DWELL_TABLES.")
				 (make-parameter '|TABLEPASSCOUNT| '|/U32-Type| :short-description "'# of times Schedule Table has been processed'")
				 (make-parameter '|TABLESCSERRCOUNTER| '|/U16-Type| :short-description "Tables miscompare counter.")
				 (make-parameter '|TABLESCSSTATE| '|/U8-Type| :short-description "CS Tables table checksum state.")
				 (make-parameter '|TABLEVERIFYFAILURECOUNT| '|/U16-Type| :short-description "'# of times table loads unsuccessfully verified'")
				 (make-parameter '|TABLEVERIFYSUCCESSCOUNT| '|/U16-Type| :short-description "'# of times table loads successfully verified'")
				 (make-parameter '|TBL_CMDCOUNTER| '|/U8-Type| :short-description "TBL  Count of valid commands received.")
				 (make-parameter '|TBL_ERRCOUNTER| '|/U8-Type| :short-description "TBL  Count of invalid commands received.")
				 (make-parameter '|TIME_CMDCOUNTER| '|/U8-Type| :short-description "TIME Time Command Execution Counter.")
				 (make-parameter '|TIME_ERRCOUNTER| '|/U8-Type| :short-description "TIME Time Command Error Counter.")
				 (make-parameter '|TOTALFILES| '|/U32-Type| :short-description "Number of files in the directory.")
				 (make-parameter '|TO_BITS| '|/U16-Type| :short-description "'16 bits total, broken down as: bit1:1; bit2:1; bit34:2; bit56:2; bit78:2; nibble1:4; nibble2:4;'")
				 (make-parameter '|TO_BL_1| '|/U8-Type| :short-description "")
				 (make-parameter '|TO_BL_2| '|/U8-Type| :short-description "")
				 (make-parameter '|TO_B_1| '|/U8-Type| :short-description "") 
				 (make-parameter '|TO_B_2| '|/U8-Type| :short-description "") 
				 (make-parameter '|TO_B_3| '|/U8-Type| :short-description "") 
				 (make-parameter '|TO_B_4| '|/U8-Type| :short-description "") 
				 (make-parameter '|TO_CONFIG_ROUTES| '|/U16-Type| :short-description "Current mask of configured routes")
				 (make-parameter '|TO_DF_1| '|/F64-Type| :short-description "")
				 (make-parameter '|TO_DF_2| '|/F64-Type| :short-description "")
				 (make-parameter '|TO_DW_1| '|/U16-Type| :short-description "")
				 (make-parameter '|TO_DW_2| '|/U16-Type| :short-description "")
				 (make-parameter '|TO_ENABLED_ROUTES| '|/U16-Type| :short-description "Current mask of enabled routes")
				 (make-parameter '|TO_F_1| '|/F32-Type| :short-description "") 
				 (make-parameter '|TO_F_2| '|/F32-Type| :short-description "") 
				 (make-parameter '|TO_MESSAGE_SUB_COUNT| '|/U16-Type| :short-description "Count of subscribed messages by all telemetry pipe.")
				 (make-parameter '|TO_MESSAGE_SUB_ERROR_COUNT| '|/U16-Type| :short-description "Count of subscription errors")
				 (make-parameter '|TO_STR| '|/ASCII-String-Type| :short-description "")
				 (make-parameter '|TO_TABLE_UPDATE_COUNT| '|/U16-Type| :short-description "Count of table updates through CFE_TBL")
				 (make-parameter '|TO_TABLE_UPDATE_ERROR_COUNT| '|/U16-Type| :short-description "Count of table update errors")
				 (make-parameter '|TO_W_1| '|/U16-Type| :short-description "") 
				 (make-parameter '|TO_W_2| '|/U16-Type| :short-description "") 
				 (make-parameter '|UNEXPECTEDMAJORFRAMECOUNT| '|/U32-Type| :short-description "'# of unexpected Major Frame tones'")
				 (make-parameter '|UNEXPECTEDMAJORFRAME| '|/U8-Type| :short-description "Most Recent Major Frame signal was unexpected.")
				 (make-parameter '|UNMARKEDMEM| '|/U32-Type| :short-description "cfg param CFE_SB_BUF_MEMORY_BYTES minus Peak Memory in use")
				 (make-parameter '|UNREGISTEREDAPPCOUNTER| '|/U8-Type| :short-description "Unregistered application message send counter.")
				 (make-parameter '|UP_FAILEDCOUNTER| '|/U32-Type| :short-description "")
				 (make-parameter '|UP_LASTFILEUPLINKED| '|/ASCII-String-Type| :short-description "")
				 (make-parameter '|UP_METACOUNT| '|/U32-Type| :short-description "")
				 (make-parameter '|UP_SUCCESSCOUNTER| '|/U32-Type| :short-description "")
				 (make-parameter '|UP_UPLINKACTIVEQFILECNT| '|/U32-Type| :short-description "")
				 (make-parameter '|UTILCPUAVG| '|/U32-Type| :short-description "Current CPU Utilization Average.")
				 (make-parameter '|UTILCPUPEAK| '|/U32-Type| :short-description "Current CPU Utilization Peak.")
				 (make-parameter '|VALIDATIONCTR| '|/U16-Type| :short-description "Number of completed table validations.")
				 (make-parameter '|VALIDCMDCNTR| '|/U8-Type| :short-description "Count of valid commands received.")
				 (make-parameter '|VALIDMAJORFRAMECOUNT| '|/U32-Type| :short-description "'# of valid Major Frame tones received'")
				 (make-parameter '|WPRESULTS| '|WPRESULTS-Type| :short-description "Packed watchpoint results data, 2 bits per watchpoint.")
				 (make-parameter '|WPSINUSE| '|/U16-Type| :short-description "How many watchpoints are currently in effect.")))))

	
	)))


(log:info (dump-xml NASA-cFS))

										;TODO: There is an electronic data sheet that can be used to interpret the AMPCS xml
; Route VCID -> Space System
