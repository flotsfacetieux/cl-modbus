(in-package #:cl-modbus)

;; Part of Modbus Application Protocol

;; Responses

(defun read-16bits-data (stream byte-count)
  (loop with hi-value = 0
     for index from 0 to (1- byte-count)
     if (evenp index) ;; hi 16bits word value
     do (setf hi-value (* 256 (read-byte stream)))
     else
     collect (+ hi-value (read-byte stream))))

(defun registers-data (stream)
  (read-16bits-data stream (read-byte stream)))

(defun coils-data (stream)
  (let* ((byte-count (read-byte stream)))
    (loop for index from 0 to (1- byte-count)
       collect (read-byte stream))))

(defun write-status (stream)
  (read-16bits-data stream 4))

(defun fifo-queue-data (stream)
  (let ((byte-count (car (read-16bits-data stream 2))))
    (read-16bits-data stream byte-count)))
