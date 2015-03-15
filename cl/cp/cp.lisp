(in-package :common-lisp)

(defpackage liutos.cli.cp
  (:use :common-lisp)
  (:export :cp))

(in-package :liutos.cli.cp)

;;; 粗略测试的结果是仍然要慢于原生的cp命令的复制速度
;;; cp: String|Pathname -> String|Pathname -> IO ()
(defun cp (from to)
  "Copy files and directories."
  (with-open-file (src from :element-type '(unsigned-byte 8))
    (with-open-file (dest to
                          :direction :output
                          :element-type '(unsigned-byte 8)
                          :if-does-not-exist :create
                          :if-exists :supersede)
      (let* ((size (* 4 1024))
             (buf (make-array size :element-type '(unsigned-byte 8))))
        (loop
           :for len = (read-sequence buf src)
           :do (write-sequence buf dest :end len)
           :if (< len size)
           :do (loop-finish))))))
