(in-package #:cl-modbus-tests)

;; PDU tests
(define-test test-common-pdu-data
  (let ((pdu (cl-modbus::common-pdu #x1 #x0013 :data #x0013))
	(result #(#x1 #x0 #x13 #x0 #x13)))
    (assert-equalp result pdu)))


(define-test test-common-pdu-bytes
  (let ((pdu (cl-modbus::common-pdu #x10 #x0001
				    :bytes '(#x00 #x0a #x01 #x02)))
	(result #(#x10 #x0 #x1 #x0 #xa #x1 #x2)))
    (assert-equalp result pdu)))

(define-test test-common-pdu-data-and-bytes
  (let ((pdu (cl-modbus::common-pdu #x10 #x0001 :data #x0002
				    :bytes '(#x00 #x0a #x01 #x02)))
	(result #(#x10 #x0 #x1 #x0 #x2 #x0 #xa #x1 #x2)))
    (assert-equalp result pdu)))

(define-test test-common-pdu-no-data
  (let ((pdu (cl-modbus::common-pdu #x18 #x04de))
	(result #(#x18 #x4 #xde)))
    (assert-equalp result pdu)))

;; Complete request test

(define-test test-1bit-data-requests
  (let ((server (local-rtu-server)))
    (when server
      (unwind-protect
	   (let* ((gw (cl-modbus:make-modbus-gateway
		       "127.0.0.1" 39001 :rtu))
		  (request1 (cl-modbus:read-coils
			     gw 1 #x13 #x13))
		  (result1 '(t nil t t nil nil t t
			     t t nil t nil t t nil
			     t nil t))
		  (request2 (cl-modbus:read-discrete-inputs
			     gw 34 #xc4 #x16))
		  (result2 '(nil nil t t nil t nil t
			     t t nil t t nil t t
			     t nil t nil t t))
		  (request3 (cl-modbus:write-single-coil gw 23 #xac t))
		  (request4
		   (cl-modbus:write-multiple-coils
		    gw 62 #x13 '(t nil t t nil nil t t
				 t nil))))
	     
	     (assert-equalp result1 request1)
	     (assert-equalp result2 request2)
	     (assert-true request3)
	     (assert-true request4))
	(sb-thread:terminate-thread *test-server*)))))

(define-test test-16bits-data-requests
  (let ((server (local-rtu-server)))
    (when server
      (unwind-protect
	   (let* ((gw (cl-modbus:make-modbus-gateway
		       "127.0.0.1" 39001 :rtu))
		  (request1 (cl-modbus:read-holding-registers
			     gw 230 #x6b 3))
		  (result1 '(#x22b 0 #x64))
		  (request2 (cl-modbus:read-input-registers
			     gw 135 8 1))
		  (result2 '(#xa))
		  (request3 (cl-modbus:write-single-register gw 2 1 3))
		  (request4 (cl-modbus:write-multiple-registers
			     gw 82 1 '(#xa 258)))
		  (request5 (cl-modbus:mask-write-register
			     gw 18 4 #xf2 #x25))
		  (request6
		   (cl-modbus:read/write-multiple-registers
		    gw 26 3 6 #xe 3 '(#xff #xff #xff)))
		  (result6 '(#xfe #x0acd 1 3 #xd #xff))
		  (request7
		   (cl-modbus:read-fifo-queue
		    gw 117 #x4de))
		  (result7 '(#x1b8 #x1284)))
	     
	     (assert-equalp result1 request1)
	     (assert-equalp result2 request2)
	     (assert-true request3)
	     (assert-true request4)
	     (assert-true request5)
	     (assert-equalp result6 request6)
	     (assert-equalp result7 request7))
	(sb-thread:terminate-thread *test-server*)))))

(define-test test-tcp-16bits-data-requests
  (let ((server (local-tcp-server)))
    (when server
      (unwind-protect
	   (let* ((gw (cl-modbus:make-modbus-gateway
		       "127.0.0.1" 39001 :tcp))
		  (request1 (cl-modbus:read-holding-registers
			     gw 230 #x6b 3))
		  (result1 '(#x22b 0 #x64))
		  )
	     
	     (assert-equalp result1 request1)
	     )
	(sb-thread:terminate-thread *test-server*)))))


(defun do-tests ()
  (setq *print-failures* t)
  (setq *print-errors* t)
  (run-tests :all :cl-modbus-tests))
