(in-package :common-lisp)

(defpackage liutos.cli.readelf
  (:use :common-lisp)
  (:export :readelf))

(in-package :liutos.cli.readelf)

(defparameter *byte1*
  (parse-integer "7F" :radix 16))
(defparameter *byte2*
  (char-code #\E))
(defparameter *byte3*
  (char-code #\L))
(defparameter *byte4*
  (char-code #\F))

(defun abi-print (abi)
  "Displays information about OS ABI."
  (let ((msg (case abi
               (#x00 "System V")
               (#x01 "HP-UX")
               (#x02 "NetBSD")
               (#x03 "Linux")
               (#x06 "Solaris")
               (#x07 "AIX")
               (#x08 "IRIX")
               (#x09 "FreeBSD")
               (#x0C "OpenBSD"))))
    (format t "OS/ABI: ~A~%" msg)))

(defun type-print (type)
  "Displays information about ELF type."
  (let ((msg (case type
               (1 "REL (Relocatable file)")
               (2 "EXEC (Executable file)")
               (3 "DYN (Shared object file)")
               (4 "CORE (Core file)"))))
    (format t "Type: ~A~%" msg)))

(defun readelf (file)
  "Displays information about ELF files."
  (with-open-file (s file :element-type '(unsigned-byte 8))
    ;;; 检查头部，第一个字节为0x7F，后面三个字节为ELF
    (let ((part (make-array 4)))
      (read-sequence part s)
      (unless (= *byte1* (aref part 0))
        (error "第一个字节必须为0x7F"))
      (unless (and (= *byte2* (aref part 1))
                   (= *byte3* (aref part 2))
                   (= *byte4* (aref part 3)))
        (error "第二、三、四个字节必须依次为E、L和F")))
    ;;; 该字节标记文件用于32位还是64位系统
    (let ((clz (read-byte s)))
      (if (= clz 1)
          (format t "Class: ELF32~%")
          (format t "Class: ELF64~%")))
    ;;; 该字节标记文件用于小端还是大端字节序
    (let ((dat (read-byte s)))
      (if (= dat 1)
          (format t "Data: 2's complement, little endian~%")
          (format t "Date: 2's complement, big endian~%")))
    ;;; 该字节始终为1
    (let ((ver (read-byte s)))
      (format t "Version: ~D (current)~%" ver))
    ;;; 该字节描述了文件用于的操作系统二进制应用接口(ABI)
    (let ((abi (read-byte s)))
      (abi-print abi))
    ;;; 该字节描述ABI的版本
    (format t "ABI Version: ~D~%" (read-byte s))
    ;;; PAD字段，未使用
    (let ((pad (make-array 7)))
      (read-sequence pad s))
    (let ((type (read-byte s)))
      (type-print type))))
