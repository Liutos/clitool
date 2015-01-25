;; 1. 定义包

(in-package :common-lisp)

(defpackage liutos.cli.tree
  (:use :common-lisp
        :cl-fad)
  (:export :tree))

(in-package :liutos.cli.tree)

;; 2. 定义函数

;;; directoryp: Pathname|String -> Bool
;;; 若`path'表示的路径指向一个目录，则其pathname-name为NIL
(defun directoryp (path)
  "Check whether a path is a directory."
  (let ((it (probe-file path)))
    (and it (null (pathname-name it)))))

;;; indent: Int -> IO ()
(defun indent (level lastp)
  "Print indentation."
  (when (> level 0)
    (dotimes (i (1- level))
      (format t "|   "))
    (format t "~C-- " (if lastp #\` #\|))))

;;; file: Pathname -> IO ()
(defun display-file (file level lastp)
  "Print file information."
  (indent level lastp)
  (format t "~A~%" (lastname file)))

;;; last1: List 'a -> 'a
(defun last1 (list)
  "Get last element in list."
  (first (last list)))

;;; lastname: Pathname -> String
(defun lastname (path)
  "Return name of regular file or directory."
  (cond ((directoryp path)
         (last1 (pathname-directory path)))
        ((pathname-type path)
         (concatenate 'string
                      (pathname-name path)
                      "."
                      (pathname-type path)))
        (t (pathname-name path))))

;;; my-alnumsort: Pathname -> Pathname -> Bool
;;; 忽略非字母和数字的字符，并忽略大小写的差异进行比较
(defun my-alnumsort (p1 p2)
  "Compare the names of two pathname objects."
  (let* ((n1 (lastname p1))
         (n2 (lastname p2))
         (s1 (position-if #'alphanumericp n1))
         (s2 (position-if #'alphanumericp n2)))
    (string-lessp n1 n2 :start1 s1 :start2 s2)))

;;; my-directory: String -> [Pathname]
;;; 1. 调用原生的directory函数
;;; 2. 若返回的列表近包含一个元素，则将`pathspec'置为wild并再次调用原生的directory函数
;;; 3. 否则，返回directory的结果
(defun my-directory (pathspec)
  "Return a fresh list of pathnames corresponding to those files under given directory."
  (let* ((conts (list-directory pathspec)))
    (sort conts #'my-alnumsort)))

;;; display-dir: Pathname -> IO ()
(defun display-dir (dir level lastp)
  "Print directory name."
  (indent level lastp)
  (let ((name (lastname dir)))
    (format t "~A~%" name)))

;;; tree-core: String -> IO ()
;;; 1. 取出`dir'目录下的所有内容，遍历每一项
;;; 2. 若取得的内容为普通文件，则打印到标准输出
;;; 3. 若取得的为目录，则对该目录调用tree进行处理
(defun tree-core (dir &optional (level 0))
  ;; Describe as function TREE.
  (let ((conts (my-directory dir)))
    (loop
       :for (cont . rest) :on conts
       :do (if (directoryp cont)
               (progn
                 (display-dir cont level (null rest))
                 (tree-core (namestring cont) (1+ level)))
               (display-file cont level (null rest))))))

;;; tree: String -> IO ()
(defun tree (dir)
  "List contents of directories in a tree-like format."
  (assert (directoryp dir))
  (display-dir dir 0 nil)
  (tree-core dir 1))
