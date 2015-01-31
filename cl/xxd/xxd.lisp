(in-package :common-lisp)

(defpackage :liutos.cli.xxd
  (:use :common-lisp)
  (:export :xxd))

(in-package :liutos.cli.xxd)

;;; offset-print: Int -> IO ()
(defun offset-print (offset)
  "Print a hex of offset."
  (format t "~7,'0,X:" offset))

;;; hex-print: [Byte] -> Int -> IO ()
;;; 每输出两个字节，就输出一个空格作为分隔符
(defun hex-print (bytes pos group)
  "Print a hexdump of bytes."
  (dotimes (i pos)
    (when (zerop (mod i group))
      (format t " "))
    (let ((byte (aref bytes i)))
      (format t "~2,'0,X" byte)))
  (format t " "))

;;; bytes-print: [Byte] -> Int -> IO ()
;;; 1. 输出偏移量
;;; 2. 输出字节序列的十六进制表示
;;; 3. 输出字节序列的ASCII字符表示
(defun bytes-print (bytes pos offset &key (group 2) &allow-other-keys)
  "Print a hexdump and content of bytes."
  (offset-print offset)
  (hex-print bytes pos group)
  (dotimes (i pos)
    (let* ((byte (aref bytes i))
           (c (code-char byte)))
      (if (graphic-char-p c)
          (format t "~C" c)
          (format t "."))))
  (format t "~%"))

;;; xxd: String|Pathname -> IO ()
(defun xxd (infile &rest args &key (column 16) (group 2))
  (declare (ignorable column group))
  "Make a hexdump or do the reverse."
  (with-open-file (s infile :element-type '(unsigned-byte 8))
    (let* ((len column)
           (bytes (make-array len :element-type '(unsigned-byte 8)))
           (offset 0))
      (loop
         (let ((pos (read-sequence bytes s)))
           (cond ((< pos len)
                  (apply #'bytes-print bytes pos offset args)
                  (return))
                 (t (apply #'bytes-print bytes pos offset args)))
           (incf offset pos))))))
