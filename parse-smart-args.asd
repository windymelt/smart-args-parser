#|
  This file is a part of parse-smart-args project.
  Copyright (c) 2018 Windymelt
|#

#|
  Author: Windymelt
|#

(defsystem "parse-smart-args"
  :version "0.1.0"
  :author "Windymelt"
  :license "MIT"
  :depends-on (:cl-ppcre :iterate :esrap :esrap-peg)
  :components ((:module "src"
                :components
                ((:file "parse-smart-args"))))
  :description ""
  :long-description
  #.(read-file-string
     (subpathname *load-pathname* "README.markdown"))
  :in-order-to ((test-op (test-op "parse-smart-args-test"))))
