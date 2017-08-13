(asdf:defsystem #:lt-cl-ext-test
  :author "Liutos <mat.liutos@gmail.com>"
  :depends-on (#:prove)
  :components ((:file "ext" :depends-on ("package"))
               (:file "package")))
