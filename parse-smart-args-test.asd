#|
  This file is a part of parse-smart-args project.
  Copyright (c) 2018 Windymelt
|#

(defsystem "parse-smart-args-test"
  :defsystem-depends-on ("prove-asdf")
  :author "Windymelt"
  :license ""
  :depends-on ("parse-smart-args"
               "prove")
  :components ((:module "tests"
                :components
                ((:test-file "parse-smart-args"))))
  :description "Test system for parse-smart-args"

  :perform (test-op (op c) (symbol-call :prove-asdf :run-test-system c)))
