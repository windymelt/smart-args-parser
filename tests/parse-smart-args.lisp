(defpackage parse-smart-args-test
  (:use :cl
        :parse-smart-args
        :prove
        :esrap-peg :esrap))
(in-package :parse-smart-args-test)

;; NOTE: To run this test file, execute `(asdf:test-system :parse-smart-args)' in your Lisp.

(plan nil)

(subtest "extract-subs-from-stream"
  (is (length (with-open-file (s #P"tests/Test.pm") (extract-subs-from-stream s)))
      2))

(parse-smart-args:load-peg)

(subtest "def-peg-fun"
  (is (ast-eval (parse 'parse-smart-args::spaces "   ")) 'parse-smart-args::spaces)
  (is (ast-eval (parse 'parse-smart-args::argsheading "args")) 'parse-smart-args::args)
  (is (ast-eval (parse 'parse-smart-args::argsposheading "args_pos")) 'parse-smart-args::args-pos)
  (is (ast-eval (parse 'parse-smart-args::comma ",")) 'parse-smart-args::comma)
  (is (ast-eval (parse 'parse-smart-args::semicolon ";")) 'parse-smart-args::semicolon)
  (is (ast-eval (parse 'parse-smart-args::colon ":")) #\:)
  (is (ast-eval (parse 'parse-smart-args::my "my")) 'parse-smart-args::my)
  (is (ast-eval (parse 'parse-smart-args::dollar "$")) 'parse-smart-args::dollar)
  (is (ast-eval (parse 'parse-smart-args::alpha "a")) #\a)
  (is (ast-eval (parse 'parse-smart-args::digit "1")) #\1)
  (is (ast-eval (parse 'parse-smart-args::number "100")) 100)
  (is (ast-eval (parse 'parse-smart-args::underscore "_")) #\_)
  (is (ast-eval (parse 'parse-smart-args::alnumsign "a")) #\a)
  (is (ast-eval (parse 'parse-smart-args::plussign "+")) 'parse-smart-args::plussign)
  
  (is (ast-eval (parse 'parse-smart-args::variable "$foobar")) (cons 'parse-smart-args::variable "foobar"))
  (is (ast-eval (parse 'parse-smart-args::type "Test::WWW::Stub")) (cons 'parse-smart-args::type "Test::WWW::Stub"))
  (is (ast-eval (parse 'parse-smart-args::argtypeobject "{ isa => \"Int\" }")) (cons 'parse-smart-args::argtypeobject (list (cons 'parse-smart-args::argtypedefinition (cons "isa" (cons 'parse-smart-args::string "Int"))))))
  (is (ast-eval (parse 'parse-smart-args::string "\"foo\"")) (cons 'parse-smart-args::string "foo"))
  (is (ast-eval (parse 'parse-smart-args::string "'foo'")) (cons 'parse-smart-args::string "foo"))
  (is (ast-eval (parse 'parse-smart-args::argtypedefinition "isa => \"Int\"")) (cons 'parse-smart-args::argtypedefinition (cons "isa" (cons 'parse-smart-args::string "Int"))))
  (is (ast-eval (parse 'parse-smart-args::onearg "my $foobar => \"Int\""))
      (cons 'parse-smart-args::onearg
            (cons (cons 'parse-smart-args::variable "foobar")
                  (cons 'parse-smart-args::simpleargtype (cons 'parse-smart-args::type "Int")))))

  (is (ast-eval (parse 'parse-smart-args::onearg "my $foobar => { isa => 'Int', optional => 1 }"))
      (cons 'parse-smart-args::onearg
            (cons (cons 'parse-smart-args::variable "foobar")
                  (cons 'parse-smart-args::argtypeobject
                        (list
                         (cons 'parse-smart-args::argtypedefinition
                               (cons "isa" (cons 'parse-smart-args::string "Int")))
                         (cons 'parse-smart-args::argtypedefinition (cons "optional" 1)))))))
 (is (ast-eval (parse 'parse-smart-args::onearg "my $foobar => { isa => 'HashRef', default => +{ buzz => 1} }"))
      (cons 'parse-smart-args::onearg
            (cons (cons 'parse-smart-args::variable "foobar")
                  (cons 'parse-smart-args::argtypeobject
                        (list
                         (cons 'parse-smart-args::argtypedefinition
                               (cons "isa" (cons 'parse-smart-args::string "HashRef")))
                         (cons 'parse-smart-args::argtypedefinition
                               (cons "default"
                                     (cons 'parse-smart-args::hashref (list (cons "buzz" 1))))))))))
  )

(subtest "hashref"
  (is (ast-eval (parse 'parse-smart-args::hashref "{}"))
      (cons 'parse-smart-args::hashref nil))
  (is (ast-eval (parse 'parse-smart-args::hashref "+{}"))
      (cons 'parse-smart-args::hashref nil))
  (is (ast-eval (parse 'parse-smart-args::hashref "{foo => 'bar'}"))
      (cons 'parse-smart-args::hashref (list (cons "foo" (cons 'string "bar")))))
  (is (ast-eval (parse 'parse-smart-args::hashref "{foo => 'bar', piyo => 42}"))
      (cons 'parse-smart-args::hashref (list (cons "foo" (cons 'string "bar"))
                                             (cons "piyo" 42))))
  (is (ast-eval (parse 'parse-smart-args::hashref "{foo => 'bar', piyo => 42, buz => 1}"))
      (cons 'parse-smart-args::hashref (list (cons "foo" (cons 'string "bar"))
                                             (cons "piyo" 42)
                                             (cons "buz" 1))))
  (is (ast-eval (parse 'parse-smart-args::hashref "{foo => {bar => 1}}"))
      (cons 'parse-smart-args::hashref
            (list
             (cons "foo" (cons 'parse-smart-args::hashref
                               (list
                                (cons "bar" 1)))))))
  )

(finalize)
