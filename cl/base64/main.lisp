(in-package :common-lisp)

(defpackage liutos.cli.base64
  (:use :common-lisp)
  (:export :base64))

(in-package :liutos.cli.base64)

(defparameter *base64-chars*
  "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/")

(defun read-bytes-num (stream)
  (let* ((bytes (make-array 3
                            :element-type '(unsigned-byte 8)
                            :initial-element 0))
         (len (read-sequence bytes stream))
         (num 0))
    (when (zerop len)
      (return-from read-bytes-num nil))
    (dotimes (i len num)
      (setf num (+ (ash num 8) (aref bytes i))))))

;;; 先右移4位，使其位数为12位，即两个6比特
;;; 再取出其右边的6个比特，再右移6比特
;;; 取出低位的6个比特的方法，是将原始数字与6位均为1的数字实行按位与操作
;;; 输出高6位对应的字符，再输出底6位的字符，最后输出两个等号
(defun base64-stream-encode1 (stream num)
  (setf num (ash num 4))
  (let (byte1 byte2)
    (declare (ignorable byte1))
    (setf byte2 (logand num (1- (ash 1 6))))
    (setf num (ash num -6))
    (format stream "~C~C=="
            (char *base64-chars* num)
            (char *base64-chars* byte2))))

;;; 与处理一字节时的情况类似
(defun base64-stream-encode2 (stream num)
  (setf num (ash num 2))
  (let (byte2 byte3)
    (setf byte3 (logand num (1- (ash 1 6))))
    (setf num (ash num -6))
    (setf byte2 (logand num (1- (ash 1 6))))
    (setf num (ash num -6))
    (format stream "~C~C~C="
            (char *base64-chars* num)
            (char *base64-chars* byte2)
            (char *base64-chars* byte3))))

(defun base64-stream-encode3 (stream num)
  (let ((bytes '()))
    (dotimes (i 4)
      (push (logand num (1- (ash 1 6))) bytes)
      (setf num (ash num -6)))
    (dolist (byte bytes)
      (format stream "~C" (char *base64-chars* byte)))))

;;; base64-stream-encode: Stream -> IO ()
;;; 从一个字节流中读取字节，经过Base64编码后输出到一个字符流中
;;; 如果读取到的数字小于256，说明仅从字节流中读出了一个字节，另行处理
;;; 如果读取到的数字小于65536，说明仅从字节流中读出了两个字节，另行处理
;;; 否则，代表可以从字节流中读出三个字节，按照规则转换为4个新的字符输出到字符流中
(defun base64-stream-encode (stream)
  (declare (ignorable stream))
  (with-output-to-string (str)
    (loop
       :for num = (read-bytes-num stream)
       :while num
       :do (cond ((< num 256)
                  (base64-stream-encode1 str num) (loop-finish))
                 ((< num 65536)
                  (base64-stream-encode2 str num) (loop-finish))
                 (t (base64-stream-encode3 str num))))))

;;; base64-encode: String|Pathname -> IO ()
(defun base64-encode (file)
  (with-open-file (stream file :element-type '(unsigned-byte 8))
    (base64-stream-encode stream)))

;;; base64: String|Pathname -> IO ()
(defun base64 (file &rest args &key (decode nil))
  "Base64 encode/decode data and print to standard output."
  (declare (ignorable args))
  (when (not decode)
    (base64-encode file)))
