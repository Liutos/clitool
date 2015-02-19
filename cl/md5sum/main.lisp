(in-package :common-lisp)

(defpackage liutos.cli.md5sum
  (:use :common-lisp)
  (:export :md5sum))

(in-package :liutos.cli.md5sum)

;;; 下列数字来自于维基百科
(defparameter *s*
  #(7 12 17 22 7 12 17 22 7 12 17 22 7 12 17 22
    5 9 14 20 5 9 14 20 5 9 14 20 5 9 14 20
    4 14 16 23 4 14 16 23 4 14 16 23 4 14 16 23
    6 10 15 21 6 10 15 21 6 10 15 21 6 10 15 21))

(defparameter *K*
  #(#xd76aa478 #xe8c7b756 #x242070db #xc1bdceee
    #xf57c0faf #x4787c62a #xa8304613 #xfd469501
    #x698098d8 #x8b44f7af #xffff5bb1 #x895cd7be
    #x6b901122 #xfd987193 #xa679438e #x49b40821
    #xf61e2562 #xc040b340 #x265e5a51 #xe9b6c7aa
    #xd62f105d #x02441453 #xd8a1e681 #xe7d3fbc8
    #x21e1cde6 #xc33707d6 #xf4d50d87 #x455a14ed
    #xa9e3e905 #xfcefa3f8 #x676f02d9 #x8d2a4c8a
    #xfffa3942 #x8771f681 #x6d9d6122 #xfde5380c
    #xa4beea44 #x4bdecfa9 #xf6bb4b60 #xbebfbc70
    #x289b7ec6 #xeaa127fa #xd4ef3085 #x04881d05
    #xd9d4d039 #xe6db99e5 #x1fa27cf8 #xc4ac5665
    #xf4292244 #x432aff97 #xab9423a7 #xfc93a039
    #x655b59c3 #x8f0ccc92 #xffeff47d #x85845dd1
    #x6fa87e4f #xfe2ce6e0 #xa3014314 #x4e0811a1
    #xf7537e82 #xbd3af235 #x2ad7d2bb #xeb86d391
    ))

(defparameter *a0* #x67452301)
(defparameter *b0* #xefcdab89)
(defparameter *c0* #x98badcfe)
(defparameter *d0* #x10325476)

(defun num2bytes (num)
  (let ((bytes (make-array 8)))
    (dotimes (i 8 bytes)
      (when (zerop num)
        (return-from num2bytes bytes))
      (setf (aref bytes (- 8 i 1))
            (logand num (1- (ash 1 8))))
      (setf num (ash num -8)))))

(defun read-align-448 (bits)
  (declare (ignorable bits))
  (vector-push-extend #x80 bits)
  (let* ((len (length bits))
         (target (/ (+ (floor (* len 8) 512) 448) 8)))
    (dotimes (i (- target len))
      (vector-push-extend 0 bits))
    (let ((bytes (num2bytes (1- len))))
      (dotimes (i 8)
        (vector-push-extend (aref bytes i) bits)))))

;;; 计算流的长度，分配足够大的比特数组并将所有内容读入
;;; 在比特数组中添加一个比特1，之后持续追加比特0，直到长度len满足：存在正整数N，使得len = 512N + 448
;;; 计算文件长度，换算成对应的比特数，从文件流中读取字节并填充到比特数组中
;;; 如果最后一个字节是奇数，就需要新增一个#x80并填充剩余字节对齐到512N + 448
;;; 如果最后一个字节是偶数，就需要追加一个比特1并填充剩余字节对齐到512N + 448
;;; FIXME: 我怀疑我对这里的evenp成立时的处理方法有误解
(defun read-align (stream)
  (let* ((len (file-length stream))
         (bits (make-array len :element-type '(unsigned-byte 8) :fill-pointer 0)))
    (dotimes (i len)
      (let ((byte (read-byte stream nil)))
        (vector-push byte bits)))
    (read-align-448 bits)
    bits))

(defun main-loop (blk)
  (declare (ignorable blk))
  (let ((a *a0*) (b *b0*) (c *c0*) (d *d0*))
    ))

(defun md5sum-stream (stream)
  (declare (ignorable stream))
  (let ((bits (read-align stream)))
    (flexi-streams:with-input-from-sequence (s bits)
      (let ((blk (make-array (/ 512 8))))
        (loop
           :for len = (read-sequence blk s)
           :do (main-loop blk)
           :if (< len (/ 512 8))
           :do (loop-finish))))))

(defun md5sum (file)
  "Compute and check MD5 message digest."
  (with-open-file (stream file :element-type '(unsigned-byte 8))
    (md5sum-stream stream)))
