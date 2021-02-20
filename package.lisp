(in-package #:cl-user)

(defpackage #:cl-modbus
  (:use #:cl)
  (:export #:read-coils
	   #:read-discrete-inputs
	   #:read-holding-registers
	   #:read-input-registers
	   #:write-single-coil
	   #:write-single-register
	   #:write-multiple-coils
	   #:write-multiple-registers
	   #:file-record-request
	   #:read-file-record
	   #:write-file-record
	   #:mask-write-register
	   #:read/write-multiple-registers
	   #:read-fifo-queue
	   #:encapsulated-interface-transport
	   ;; Main API
	   #:make-modbus-gateway
	   #:modbus-function-name
	   #:modbus-function-string
	   ;; Conditions
	   #:cl-modbus-error
	   #:modbus-error-text
	   ;; Parse functions
	   #:slave-id-p
	   #:addressp
	   ))

(defpackage #:cl-modbus-tests
  (:use #:cl #:lisp-unit)
  (:export #:do-tests))
