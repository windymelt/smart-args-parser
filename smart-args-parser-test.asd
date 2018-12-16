#|
  This file is a part of smart-args-parser project.
  Copyright (c) 2018 Windymelt
|#

(defsystem "smart-args-parser-test"
  :defsystem-depends-on ("prove-asdf")
  :author "Windymelt"
  :license ""
  :depends-on ("smart-args-parser"
               "prove")
  :components ((:module "tests"
                :components
                ((:test-file "smart-args-parser"))))
  :description "Test system for smart-args-parser"

  :perform (test-op (op c) (symbol-call :prove-asdf :run-test-system c)))
