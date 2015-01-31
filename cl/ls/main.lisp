(in-package :common-lisp)

(defpackage liutos.cli.ls
  (:use :common-lisp
        :osicat)
  (:export :ls))

(in-package :liutos.cli.ls)

;;; matchp: String -> Bool
;;; 隐藏文件不会输出，除非指定了输出全部的文件
(defun matchp (cont &key (almost-all-p nil) &allow-other-keys)
  "Check whether the entry should be printed."
  (when almost-all-p
    (return-from matchp t))
  (let ((name (if (pathnamep cont)
                  (namestring cont)
                  cont)))
    (not (char= (char name 0) #\.))))

;;; cont-name: String -> String
;;; 若最后一个字符为斜杆，表示这是一个目录，则返回的字符串中不包含这个斜杆，否则原样返回
(defun cont-name (cont)
  "Get the name of entry without type indicator."
  (let ((c (char cont (1- (length cont)))))
    (if (char= c #\/)
        (subseq cont 0 (1- (length cont)))
        cont)))

;;; permission-print: String -> IO
;;; 将Osicat给出的权限描述转换为rwx类型的文字描述
(defun permission-print (cont)
  "Print the permissions of given entry."
  (let ((perm (file-permissions (absolute-pathname cont))))
    (format t "~C" (if (find :user-read perm) #\r #\-))
    (format t "~C" (if (find :user-write perm) #\w #\-))
    (format t "~C" (if (find :user-exec perm) #\x #\-))
    (format t "~C" (if (find :group-read perm) #\r #\-))
    (format t "~C" (if (find :group-write perm) #\w #\-))
    (format t "~C" (if (find :group-exec perm) #\x #\-))
    (format t "~C" (if (find :other-read perm) #\r #\-))
    (format t "~C" (if (find :other-write perm) #\w #\-))
    (format t "~C" (if (find :other-exec perm) #\x #\-)))
  (format t " "))

;;; file-type-print: String -> IO ()
(defun file-type-print (cont)
  "Print the type of given entry."
  (cond ((directory-exists-p cont)
         (format t "d"))
        (t (format t "-"))))

;;; file-size-print: String -> IO ()
(defun file-size-print (cont)
  "Print the size of given entry."
  (with-open-file (s cont)
    (format t "~D " (file-length s))))

;;; file-time-print: String -> IO ()
(defun file-time-print (cont)
  "Print the last modify time of given entry."
  (let ((time (file-write-date cont)))
    (multiple-value-bind (sec min hour day mon year)
        (decode-universal-time time)
      (declare (ignorable sec year))
      (format t "~D月 ~D ~D:~D " mon day hour min))))

;;; cont-print: Pathname -> IO ()
;;; 输出详细信息时，每一列依次为
;;; 文件类型，文件权限，文件体积，最后修改时间，文件名
(defun cont-print (cont longp)
  "Print the information of given entry."
  (when longp
    (file-type-print cont))
  (when longp
    (permission-print cont))
  (when longp
    (file-size-print cont))
  (when longp
    (file-time-print cont))
  (let ((name (cont-name (namestring cont))))
    (format t "~A" name)))

;;; cont-ls: Pathname -> IO ()
;;; 若classifyp为真，则输出区分不同文件类型的后缀
(defun cont-ls (cont
                &key
                  (classifyp nil)
                  (longp nil)
                  &allow-other-keys)
  "List a content."
  (cont-print cont longp)
  (when classifyp
    (cond ((directory-exists-p cont)
           (format t "/"))))
  (format t "~%"))

;;; ls: String|Pathname -> IO ()
;;; 列出目录下的所有内容
;;; 遍历每一个元素，输出文件/目录的名称
(defun ls (pathspec &rest args
           &key
             (almost-all-p nil)
             (classifyp nil)
             (longp nil))
  "List directory contents."
  (declare (ignorable almost-all-p
                      classifyp
                      longp))
  (multiple-value-bind (path type)
      (file-exists-p pathspec)
    (declare (ignore path))
    (unless (eq type :directory)
      (apply #'cont-ls pathspec args)
      (return-from ls)))
  (let ((conts (list-directory pathspec :bare-pathnames t)))
    (dolist (cont conts)
      (when (apply #'matchp cont args)
        (apply #'cont-ls cont args)))))
