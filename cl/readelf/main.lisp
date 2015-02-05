(in-package :common-lisp)

(defpackage liutos.cli.readelf
  (:use :common-lisp)
  (:export :readelf))

(in-package :liutos.cli.readelf)

(defclass elf ()
  ((mag :documentation "The first 16 bytes"
        :accessor elf-mag)
   (clz :documentation "The byte to signify 32- or 64-bit format"
        :accessor elf-clz)
   (dat :documentation "The byte to signify little or big endianness"
        :accessor elf-dat)
   (ver :documentation "Set to 1 for the original version of ELF"
        :accessor elf-ver)
   (osabi :documentation "Identifies the target operating system ABI"
          :accessor elf-osabi)
   (abiver :documentation "Specifies the ABI version"
           :accessor elf-abiver)
   (pad :documentation "Currently unused"
        :accessor elf-pad)
   (type :documentation "Specifies the relocatable/executable/shared/core file type"
         :accessor elf-type)
   (machine :documentation "Specifies target instruction set architecture"
            :accessor elf-machine)
   (ver2 :documentation "Set to 1 for the original version of ELF"
         :accessor elf-ver2)
   (entry :documentation "Memory address where the process start executing"
          :accessor elf-entry)
   (phoff :documentation "Start of the program header table"
          :accessor elf-phoff)
   (shoff :documentation "Start of the section header table"
          :accessor elf-shoff)
   (flags :documentation "Depends on the target architecture"
          :accessor elf-flags)
   (ehsize :documentation "Size of ELF header"
           :accessor elf-ehsize)
   (phentsize :documentation "Size of program header table entry"
              :accessor elf-phentsize)
   (phnum :documentation "Number of entries in program header table"
          :accessor elf-phnum)
   (shentsize :documentation "Size of section header table entry"
              :accessor elf-shentsize)
   (shnum :documentation "Number of entries in section header table"
          :accessor elf-shnum)
   (shstrndx :documentation "Contains index of the section header table entry that contains the section name"
             :accessor elf-shstrndx)))

(defun bytes-to-num (bytes)
  "Convert bytes into a integer."
  (let ((sum 0) (len (length bytes)))
    (dotimes (i len sum)
      (setf sum (+ (* sum 256) (aref bytes (- len i 1)))))))

(defun read-bytes (s len &optional (numberp nil))
  "Read bytes in given number."
  (let ((bytes (make-array len)))
    (read-sequence bytes s)
    (if numberp (bytes-to-num bytes) bytes)))

(defun elf-parse (file)
  "Read from ELF file and generate an instance of class ELF."
  (with-open-file (s file :element-type '(unsigned-byte 8))
    (let* ((mag (read-bytes s 4))
           (clz (read-bytes s 1 t))
           (dat (read-bytes s 1 t))
           (ver (read-bytes s 1 t))
           (osabi (read-bytes s 1 t))
           (abiver (read-bytes s 1 t))
           (pad (read-bytes s 7))
           (type (read-bytes s 2 t))
           (machine (read-bytes s 2 t))
           (ver2 (read-bytes s 4 t))
           (entry (read-bytes s (if (= clz 1) 4 8) t))
           (phoff (read-bytes s (if (= clz 1) 4 8) t))
           (shoff (read-bytes s (if (= clz 1) 4 8) t))
           (flags (read-bytes s 4 t))
           (ehsize (read-bytes s 2 t))
           (phentsize (read-bytes s 2 t))
           (phnum (read-bytes s 2 t))
           (shentsize (read-bytes s 2 t))
           (shnum (read-bytes s 2 t))
           (shstrndx (read-bytes s 2 t))
           (ins (make-instance 'elf)))
      (setf (elf-mag ins) mag)
      (setf (elf-clz ins) clz)
      (setf (elf-dat ins) dat)
      (setf (elf-ver ins) ver)
      (setf (elf-osabi ins) osabi)
      (setf (elf-abiver ins) abiver)
      (setf (elf-pad ins) pad)
      (setf (elf-type ins) type)
      (setf (elf-machine ins) machine)
      (setf (elf-ver2 ins) ver2)
      (setf (elf-entry ins) entry)
      (setf (elf-phoff ins) phoff)
      (setf (elf-shoff ins) shoff)
      (setf (elf-flags ins) flags)
      (setf (elf-ehsize ins) ehsize)
      (setf (elf-phentsize ins) phentsize)
      (setf (elf-phnum ins) phnum)
      (setf (elf-shentsize ins) shentsize)
      (setf (elf-shnum ins) shnum)
      (setf (elf-shstrndx ins) shstrndx)
      ins)))

(defun abi2string (osabi)
  "Convert OS ABI to correspoding name."
  (case osabi
    (#x00 "UNIX - System V")
    (#x01 "HP-UX")
    (#x02 "NetBSD")
    (#x03 "Linux")
    (#x06 "Solaris")
    (#x07 "AIX")
    (#x08 "IRIX")
    (#x09 "FreeBSD")
    (#x0C "OpenBSD")))

(defun type2string (type)
  "Convert ELF type to correspoding name."
  (case type
    (1 "REL (Relocatable file)")
    (2 "EXEC (Executable file)")
    (3 "DYN (Shared object file)")
    (4 "CORE (Core file)")))

(defun machine2string (machine)
  "Convert machine field to corresponding name."
  (case machine
    (#x02 "SPARC")
    (#x03 "Intel 80386")
    (#x08 "MIPS")
    (#x14 "PowerPC")
    (#x28 "ARM")
    (#x2A "SuperH")
    (#x32 "IA-64")
    (#x3E "x86-64")
    (#xB7 "AArch64")))

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
  (let ((msg (abi2string abi)))
    (elf-kv-print "OS/ABI" msg)))

(defun type-print (type)
  "Displays information about ELF type."
  (let ((msg (type2string type)))
    (elf-kv-print "Type" msg)))

(defun machine-print (machine)
  "Displays information about target machine."
  (let ((msg (machine2string machine)))
    (elf-kv-print "Machine" msg)))

(defun entry-print (entry)
  "Displays information about entry address."
  (elf-kv-print "Entry point address"
                (format nil "0x~(~X~)" entry)))

(defun phoff-print (phoff)
  "Displays information about header table."
  (elf-kv-print "Start of program headers"
                (format nil "~D (bytes into file)" phoff)))

(defun shoff-print (shoff)
  "Displays information about section header table."
  (elf-kv-print "Start of section headers"
                (format nil "~D (bytes into file)" shoff)))

(defun elf-kv-print (key value)
  (format t "~A: ~A~%" key value))

(defun elf-file-header-print (elf)
  "Displays the information contained in the ELF header at the start of the file."
  (let ((clz (elf-clz elf)))
    (elf-kv-print "Class" (format nil "ELF~D" (if (= clz 1) 32 64))))
  (let ((dat (elf-dat elf)))
    (elf-kv-print "Data" (format nil "2' complement, ~A endian"
                                 (if (= dat 1) "littel" "big"))))
  (elf-kv-print "Version" (format nil "~D (current)" (elf-ver elf)))
  (abi-print (elf-osabi elf))
  (elf-kv-print "ABI Version" (elf-abiver elf))
  (type-print (elf-type elf))
  (machine-print (elf-machine elf))
  (elf-kv-print "Version" (format nil "0x~X" (elf-ver2 elf)))
  (entry-print (elf-entry elf))
  (phoff-print (elf-phoff elf))
  (shoff-print (elf-shoff elf))
  (elf-kv-print "Flags" (format nil "0x~X" (elf-flags elf)))
  (elf-kv-print "Size of this header"
                (format nil "~D (bytes)" (elf-ehsize elf)))
  (elf-kv-print "Size of program headers"
                (format nil "~D (bytes)" (elf-phentsize elf)))
  (elf-kv-print "Number of program headers" (elf-phnum elf))
  (elf-kv-print "Size of section headers"
                (format nil "~D (bytes)" (elf-shentsize elf)))
  (elf-kv-print "Number of section headers" (elf-shnum elf))
  (elf-kv-print "Section header string table index" (elf-shstrndx elf)))

(defun readelf (file)
  "Displays information about ELF files."
  (let ((elf (elf-parse file)))
    (elf-file-header-print elf)))
