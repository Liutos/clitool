#!/bin/bash
ln -s '/app/cl-accounting.asd' '/root/.roswell/lisp/quicklisp/local-projects/cl-accounting.asd'

sbcl --load '/app/main.lisp' --quit
