(asdf:defsystem #:gnu-find-test
  :version "0.0.1"
  :author "Liutos <mat.liutos@gmail.com>"
  :depends-on (#:gnu-find
               #:prove)
  :components ((:file "test")))

