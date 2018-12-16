#|
  This file is a part of smart-args-parser project.
  Copyright (c) 2018 Windymelt
|#

#|
  Author: Windymelt
|#

(defsystem "smart-args-parser"
  :version "0.1.0"
  :author "Windymelt"
  :license "MIT"
  :depends-on (:cl-ppcre :iterate :esrap :esrap-peg)
  :components ((:module "src"
                :components
                ((:file "smart-args-parser"))))
  :description ""
  :long-description
  #.(read-file-string
     (subpathname *load-pathname* "README.markdown"))
  :in-order-to ((test-op (test-op "smart-args-parser-test"))))
