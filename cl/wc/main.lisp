(in-package :common-lisp)

(defpackage liutos.cli.wc
  (:use :common-lisp
        :flexi-streams
        :split-sequence)
  (:export :wc))

(in-package :liutos.cli.wc)

;;; 被空格符分割的字符序列即为word，计算这些word的数量
(defun count-word (line)
  "Count number of words in a line."
  (length
   (split-sequence #\Space line
                   :remove-empty-subseqs t
                   :test #'char=)))

;;; wc: String -> IO ()
(defun wc (file &rest args
           &key
             (bytesp nil)
             (charsp nil)
             (linesp nil)
             (wordsp nil))
  "Print newline, word, and byte counts for each file."
  (let ((cnt-newline 0)
        (cnt-word 0)
        (cnt-byte 0)
        (cnt-char 0))
    (with-open-file (s file)
      (loop
         :for (line missing-newline-p) = (multiple-value-list (read-line s nil)) ; TODO: 这里是否有更好的方法可以在loop宏中绑定多重返回值到变量上
         :while line
         :if (not missing-newline-p)
         :do (progn
               (incf cnt-newline)
               (incf cnt-byte)
               (incf cnt-char))
         :do (incf cnt-char (length line))
         :do (incf cnt-word (count-word line))
         :do (incf cnt-byte (octet-length line :external-format :utf-8)))) ;这里需要指定external-format为:utf-8才能正确计算字节数
    (when charsp
      (format t "~D " cnt-char))
    (when (or linesp (every #'null args))
      (format t "~D " cnt-newline))
    (when (or wordsp (every #'null args))
      (format t "~D " cnt-word))
    (when (or bytesp (every #'null args))
      (format t "~D " cnt-byte))
    (format t "~A~%" file)))
