(in-package #:cl-modbus-tests)

(defparameter *test-modbus-function-table*
  '((#x1 test-read-coils)
    (#x2 test-read-discrete-inputs)
    (#x3 test-read-holding-registers)
    (#x4 test-read-input-registers)
    (#x5 test-write-single-coil)
    (#x6 test-write-single-register)
    (#xf test-write-multiple-coils)
    (#x10 test-write-multiple-registers)
    (#x14 test-read-file-record)
    (#x15 test-write-file-record)
    (#x16 test-mask-write-register)
    (#x17 test-read/write-multiple-registers)
    (#x18 test-read-fifo-queue)
    (#x2b test-encapsulated-interface-transport))
  "MODBUS functions table. For each function name, define the modbus identification code. ")

(defun test-modbus-request-function-name (function-code)
  (nth 1 (assoc function-code *test-modbus-function-table*)))

(defvar *test-server*)

;; A set of tests data
(defparameter *modbus-network-test*
  '(
    ;; read coils
    (:slave 1
     :address #x13
     :data #x13
     :values (#xcd #x6b #x05))
    ;; read discrete inputs
    (:slave 34
     :address #xc4
     :data #x16
     :values (#xac #xdb #x35))
     ;; read holding registers
    (:slave 230
     :address #x6b
     :data 3
     :values (2 #x2b 0 0 0 #x64))
    ;; read input registers
    (:slave 135
     :address #x8
     :data 1
     :values (0 #xa))
    ;; write single coil
    (:slave 23
     :address #xac
     :data #xff00
     :values (0 #xac #xff 0))
    ;; write single register
    (:slave 2
     :address 1
     :data 3
     :values (0 1 0 3))
    ;; write multiple coils
    (:slave 62
     :address #x13
     :data #xa
     :values (0 #x13 0 #xa))
    ;; write multiple registers
    (:slave 82
     :address #x1
     :data 2
     :values (0 1 0 2))
    ;; mask write register
    (:slave 18
     :address #x4
     :data #xf2
     :values (0 #xf2 0 #x25))
    ;; read/write multiple registers
    (:slave 26
     :address 3
     :data 6
     :values (0 #xfe #xa #xcd 0 1 0 3 0 #xd 0 #xff))
    ;; read fifo queue
    (:slave 117
     :address #x4de
     :values (0 6 0 2 1 #xb8 #x12 #x84))
    ))

(defun read-16bits-data (stream byte-count)
  (loop with hi-value = 0
     for index from 0 to (1- byte-count)
     if (evenp index) ;; hi 16bits word value
     do (setf hi-value (* 256 (read-byte stream)))
     else
     collect (+ hi-value (read-byte stream))))

(defun slave-data (slave-id)
  (find slave-id *modbus-network-test*
	:key (lambda (place)
	       (getf place :slave))))

(defun verify-address-request (slave-id
			      stream
			      resp-header
			      resp-error)
  (let ((address (car (read-16bits-data stream 2)))
	(slave-data (slave-data slave-id)))
    (format t "Slave data = ~x~%" slave-data)
    (format t "Request :~%  Address = ~x~%" address)
    (if (= address (getf slave-data :address))
	(append resp-header (getf slave-data :values))
	resp-error)))

(defun verify-address-and-data-request (slave-id
				       stream
				       resp-header
				       resp-error)
  (let* ((slave-data (slave-data slave-id))
	 (bytes (read-16bits-data stream 4))
	 (address (car bytes))
	 (data (cadr bytes)))
    (format t "Slave data = ~x~%" slave-data)
    (format t "Request :~%  Address = ~x, Data=~x~%" address data)
    (if (and (= address (getf slave-data :address))
	     (= data (getf slave-data :data)))
	(append resp-header (getf slave-data :values))
	resp-error)))

;; Functions to process requests

(defun test-read-coils (slave-id stream)
  (verify-address-and-data-request slave-id stream '(1 3) '(#x81 2)))

(defun test-read-discrete-inputs (slave-id stream)
  (verify-address-and-data-request slave-id stream '(2 3) '(#x82 2)))

(defun test-read-holding-registers (slave-id stream)
  (verify-address-and-data-request slave-id stream '(3 6) '(#x83 2)))

(defun test-read-input-registers (slave-id stream)
  (verify-address-and-data-request slave-id stream '(4 2) '(#x84 2)))

(defun test-write-single-coil (slave-id stream)
  (verify-address-and-data-request slave-id stream '(5) '(#x85 2)))

(defun test-write-single-register (slave-id stream)
  (verify-address-and-data-request slave-id stream '(6) '(#x86 2)))

(defun test-write-multiple-coils (slave-id stream)
  (verify-address-and-data-request slave-id stream '(#xf) '(#x8f 2)))

(defun test-write-multiple-registers (slave-id stream)
  (verify-address-and-data-request slave-id stream '(#x10) '(#x90 2)))

(defun test-mask-write-register (slave-id stream)
  (verify-address-and-data-request slave-id stream '(#x16) '(#x96 2)))

(defun test-read/write-multiple-registers (slave-id stream)
  (verify-address-and-data-request slave-id stream '(#x17 #xc) '(#x97 2)))

(defun test-read-fifo-queue (slave-id stream)
  (verify-address-request slave-id stream '(#x18) '(#x98 2)))

;; Write answer to stream
(defun send-answer (stream data)
  (dolist (byte data)
    (write-byte byte stream))
  (force-output stream)) 

;; Get request, process request and answer
(defun modbus-server-rtu-handler (stream)
  (declare (type stream stream))
  (let (result
	(slave-id (read-byte stream))
	(function-code (read-byte stream)))
    (setf result (funcall (test-modbus-request-function-name
    			   function-code)
    			  slave-id stream))
    (format t "Server Answer : ~x~%" result)
    (if result
	(send-answer stream (cons slave-id result))
	(send-answer stream (cons slave-id '(#x80 1))))))

(defparameter *mbap-header-response-size* 7)
(defun read-from-stream (stream nb-bytes)
  (loop repeat nb-bytes
     collect (read-byte stream)))

(defun modbus-server-tcp-handler (stream)
  (declare (type stream stream))
  (let (result
	slave-id
	(mbap-header (read-from-stream stream
				       *mbap-header-response-size*))
	(function-code (read-byte stream)))
    (setf slave-id (car (last mbap-header)))
    (format t "TCP mode - Slave-ID : ~a~%" slave-id)
    (setf result (funcall (test-modbus-request-function-name
    			   function-code)
    			  slave-id stream))
    (format t "TCP mode - Result : ~a~%" result)
    (if result
	(send-answer stream (append mbap-header result))
	(send-answer stream (append mbap-header '(#x80 1))))))

;; A simple Test server
(defun local-rtu-server ()
  (setf *test-server*
	(usocket:socket-server "127.0.0.1"
			       39001
			       'modbus-server-rtu-handler
			       '()
			       :in-new-thread t
			       :element-type 'unsigned-byte
			       :reuse-address t)))

(defun local-tcp-server ()
  (setf *test-server*
	(usocket:socket-server "127.0.0.1"
			       39001
			       'modbus-server-tcp-handler
			       '()
			       :in-new-thread t
			       :element-type 'unsigned-byte
			       :reuse-address t)))

