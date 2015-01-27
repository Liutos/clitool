(in-package :common-lisp)

(defpackage liutos.cli.tail
  (:use :common-lisp)
  (:export :tail))

(in-package :liutos.cli.tail)

;;; tail: String|Pathname -> Int -> IO ()
;;; 开辟一个给定长度的数组，用于存储文件中的最后的给定数量的行
;;; 不断地读取文件，并按照环形队列的形式填充数组中的槽
;;; 从最后一个填充的位置开始，按照环形队列的形式输出每一个槽中的内容
(defun tail (path)
  "Output the last part of a file."
  (let ((lines (make-array 10 :element-type 'string :initial-element ""))
        (i 0))
    (with-open-file (s path)
      (loop
         :for line = (read-line s nil)
         :while line
         :do (progn
               (setf (aref lines (mod i 10)) line)
               (incf i))))
    (dotimes (j 10)
      (format t "~A~%" (aref lines (mod (+ i j) 10))))))
