(in-package #:cl-modbus)

;; !! Warning !!
;; To be implemented

(defclass file-record-request ()
  ((number :accessor file-number
	   :initarg :number)
   (start :accessor record-start
	  :initarg :start)
   (length :accessor record-length
	   :initarg :length)
   (data :accessor record-data
	 :initarg :data)))

(defun record-pdu (function-code records)
  )

;;;;;;;;;;;;;;;;;;;
;; API Functions ;;
;;;;;;;;;;;;;;;;;;;

(defun read-file-record (records)
  ;; 20 (0x14) Read File Record
  ;; Byte Count : 0x07 to 0xF5 bytes - 1 byte
  ;; Sub-Req. x, Reference Type : 06 - 1 byte
  ;; Sub-Req. x, File Number : 0x0001 to 0xFFFF - 2 bytes
  ;; Sub-Req. x, Record Number : 0x0001 to 0x270F - 2 bytes
  ;; Sub-Req. x, Record Length : N - 2 bytes
  (let ((ref-type 6)
	(byte-count (* (length records) 7)))
    (when (data-valid-p byte-count #x7 #xf5)
      (pdu-message #x14
		 (cons
		  (cons byte-count 1)
		  (apply #'append
		   (mapcar
		    #'(lambda (record)
			(list
			 (cons ref-type 1)
			 (cons (file-number record) 2)
			 (cons (record-start record) 2)
			 (cons (record-length record) 2)))
		    records)))))))


(defun write-file-record (records)
  ;; 21 (0x15) Write File Record
  ;; Request data length : 0x09 to 0xFB - 1 byte
  ;; Sub-Req. x, Reference Type : 6 - 1 byte
  ;; Sub-Req. x, File Number : 0x0001 to 0xFFFF - 2 byte
  ;; Sub-Req. x, Record Number : 0x0000 to 0x270F - 2 byte
  ;; Sub-Req. x, Record length : N - 2 bytes
  ;; Sub-Req. x, Record data : N * 2 bytes
  (let ((ref-type 6)
	(byte-count
	 (+ (* (length records) 7)
	    (reduce
	     #'+
	     records
	     :key #'(lambda (record)
		      (* 2 (length (record-data record))))))))
    (when (data-valid-p byte-count #x7 #xf5)
      (pdu-message #x15
		 (cons
		  (cons byte-count 1)
		  (apply #'append
		   (mapcar
		    #'(lambda (record)
			(append
			 (list
			  (cons ref-type 1)
			  (cons (file-number record) 2)
			  (cons (record-start record) 2)
			  (cons (record-length record) 2))
			 (mapcar #'(lambda (data)
				     (cons data 2))		 
				 (record-data record))))
		    records)))))))
