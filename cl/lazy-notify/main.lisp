(in-package :lazy-notify)

(defun read-file (file)
  (with-open-file (s file :element-type '(unsigned-byte 8))
    (let* ((size (file-length s))
	   (cont (make-array size :element-type '(unsigned-byte 8))))
      (read-sequence cont s)
      cont)))

;;; Callback Helper
(defun callback-helper-pipe (file-src file-dest processor)
  "Process the content of `file-src' by `processor' and write into file `file-dest'."
  (let* ((bytes (read-file file-src))
	 (cont (funcall processor bytes)))
    (with-open-file (s file-dest
		       :direction :output
		       :element-type '(unsigned-byte 8)
		       :if-exists :supersede)
      (write-sequence cont s))))

;;; Callback
(defun callback-copy (file-src file-dest)
  "Copy contents of `file-src' to `file-dest'."
  (callback-helper-pipe file-src file-dest #'identity))

(defun callback-info (file-src file-dest)
  "Print a text of form `file-src' => `file-dest'."
  (format t "~A => ~A~%" file-src file-dest))

;;; Mapper
(defun mapper-directory (file-src dir-dest)
  (let ((name (file-namestring file-src)))
    (merge-pathnames name dir-dest)))

;;; Test
(defun test-mtime> (file-src file-dest)
  "Return true if the write date of `file-src' is later than `file-dest'."
  (let ((wd-src (file-write-date file-src))
	(wd-dest (file-write-date file-dest)))
    (let ((src (universal-to-timestamp wd-src))
	  (dest (universal-to-timestamp wd-dest)))
      (timestamp> src dest))))

(defun test-true (file-src file-dest)
  "Always return true."
  (declare (ignore file-src file-dest))
  t)

;;; Public
(defun with-directory (dir-src fn-mapper test callback
		       &key (verbose nil))
  "Iterate all files in directory `dir-src', get an target path by passing it to function `fn-mapper', test the source and target files by function `test', and if the test passed, invoke the `callback' on source and target files."
  (flet ((aux (file-src)
	   (let ((file-dest (funcall fn-mapper file-src)))
	     (when (funcall test file-src file-dest)
	       (when verbose
		 (callback-info file-src file-dest))
	       (funcall callback file-src file-dest)))))
    (walk-directory dir-src #'aux)))
