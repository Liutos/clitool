(asdf:defsystem #:lt-cl-ext
  :author "Liutos <mat.liutos@gmail.com>"
  :components ((:file "ext" :depends-on ("package"))
               (:file "package")))
