(in-package :common-lisp)

(defpackage liutos.cli.tar
  (:use :common-lisp
        :flexi-streams
        :osicat)
  (:export :tar))

(in-package :liutos.cli.tar)

(defclass tar ()
  ((name :documentation "File name"
         :accessor tar-name)
   (mode :documentation "File mode"
         :accessor tar-mode)
   (uid :documentation "Owner's numeric user ID"
        :accessor tar-uid)
   (gid :documentation "Group's numeric user ID"
        :accessor tar-gid)
   (size :documentation "File size in bytes (octal base)"
         :accessor tar-size)
   (mtime :documentation "Last modification time in numeric Unix time format (octal)"
          :accessor tar-mtime)
   (checksum :documentation "Checksum for header record"
             :accessor tar-checksum)
   (linkflag :documentation "Link indicator (file type)"
             :accessor tar-linkflag)
   (linkname :documentation "Name of linked file"
             :accessor tar-linkname)
   (pad)))

(defun bytes-to-num (bytes)
  "Convert bytes into a integer."
  (let ((sum 0) (len (length bytes)))
    (dotimes (i len sum)
      (setf sum (+ (* sum 256) (aref bytes (- len i 1)))))))

(defun read-bytes (s len &key (numberp nil) (stringp nil))
  "Read bytes in given number."
  (let ((bytes (make-array len)))
    (read-sequence bytes s)
    (cond (numberp (bytes-to-num bytes))
          (stringp (octets-to-string bytes :external-format :utf-8))
          (t bytes))))

(defun tar-parse (file)
  "Read from tar file and generate an instance of class TAR."
  (with-open-file (s file :element-type '(unsigned-byte 8))
    (let* ((name (read-bytes s 100 :stringp t))
           (mode (read-bytes s 8 :stringp t))
           (uid (read-bytes s 8 :stringp t))
           (gid (read-bytes s 8 :stringp t))
           (size (read-bytes s 12 :stringp t))
           (mtime (read-bytes s 12 :stringp t))
           (checksum (read-bytes s 8 :stringp t))
           (linkflag (read-byte s))
           (linkname (read-bytes s 100 :stringp t))
           (ins (make-instance 'tar)))
      (setf (tar-name ins) name)
      (setf (tar-mode ins) mode)
      (setf (tar-uid ins) uid)
      (setf (tar-gid ins) gid)
      (setf (tar-size ins) size)
      (setf (tar-mtime ins) mtime)
      (setf (tar-checksum ins) checksum)
      (setf (tar-linkflag ins) linkflag)
      (setf (tar-linkname ins) linkname)
      ins)))

(defun perm-print (mode)
  (format t "-")
  (if (char= (char mode 0) #\1)
      (format t "r")
      (format t "-"))
  (if (char= (char mode 1) #\1)
      (format t "w")
      (format t "-"))
  (if (char= (char mode 2) #\1)
      (format t "x")
      (format t "-")))

(defun tar-mode-print (mode)
  (let ((user (string (char mode (- (length mode) 4))))
        (group (string (char mode (- (length mode) 3))))
        (other (string (char mode (- (length mode) 2)))))
    (perm-print (format nil "~3,'0,B" (parse-integer user :radix 8)))
    (perm-print (format nil "~3,'0,B" (parse-integer group :radix 8)))
    (perm-print (format nil "~3,'0,B" (parse-integer other :radix 8)))))

(defun tar-u/gid-print (uid gid)
  (declare (ignorable gid))
  (let* ((info (user-info (parse-integer uid :radix 8 :junk-allowed t)))
         (name (cdr (assoc :name info))))
    (format t "~A/~A" name name)))

(defun tar-size-print (size)
  (format t " ~D" (parse-integer size :radix 8 :junk-allowed t)))

;;; TODO: 我也不知道为什么这里的年份会差70年
(defun tar-mtime-print (mtime)
  (let ((ts (parse-integer mtime :radix 8 :junk-allowed t)))
    (multiple-value-bind (sec min hour date month year)
        (decode-universal-time ts)
      (declare (ignore sec))
      (format t " ~4,D-~2,'0,D-~2,'0,D ~2,'0,D:~2,'0,D"
              (+ year 70) month date hour min))))

(defun tar-name-print (name)
  (let* ((pos (position #\Nul name :test #'equal))
         (name (subseq name 0 pos)))
    (format t " ~A~%" name)))

(defun tar-list (ins)
  "List the contents of an archive."
  (tar-mode-print (tar-mode ins))
  (format t " ")
  (tar-u/gid-print (tar-uid ins) (tar-gid ins))
  (tar-size-print (tar-size ins))
  (tar-mtime-print (tar-mtime ins))
  (tar-name-print (tar-name ins)))

(defun tar (file)
  "The Liutos version of the tar archiving utility."
  (let ((ins (tar-parse file)))
    (tar-list ins)))
