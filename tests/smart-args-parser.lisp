(defpackage smart-args-parser-test
  (:use :cl
        :smart-args-parser
        :prove
        :esrap-peg :esrap))
(in-package :smart-args-parser-test)

;; NOTE: To run this test file, execute `(asdf:test-system :smart-args-parser)' in your Lisp.

(plan nil)

(subtest "extract-subs-from-stream"
  (is (length (with-open-file (s #P"tests/Test.pm") (extract-subs-from-stream s)))
      2))

(smart-args-parser:load-peg)

(subtest "def-peg-fun"
  (is (ast-eval (parse 'smart-args-parser::spaces "   ")) 'smart-args-parser::spaces)
  (is (ast-eval (parse 'smart-args-parser::argsheading "args")) 'smart-args-parser::args)
  (is (ast-eval (parse 'smart-args-parser::argsposheading "args_pos")) 'smart-args-parser::args-pos)
  (is (ast-eval (parse 'smart-args-parser::comma ",")) 'smart-args-parser::comma)
  (is (ast-eval (parse 'smart-args-parser::semicolon ";")) 'smart-args-parser::semicolon)
  (is (ast-eval (parse 'smart-args-parser::colon ":")) #\:)
  (is (ast-eval (parse 'smart-args-parser::my "my")) 'smart-args-parser::my)
  (is (ast-eval (parse 'smart-args-parser::dollar "$")) 'smart-args-parser::dollar)
  (is (ast-eval (parse 'smart-args-parser::alpha "a")) #\a)
  (is (ast-eval (parse 'smart-args-parser::digit "1")) #\1)
  (is (ast-eval (parse 'smart-args-parser::number "100")) 100)
  (is (ast-eval (parse 'smart-args-parser::underscore "_")) #\_)
  (is (ast-eval (parse 'smart-args-parser::alnumsign "a")) #\a)
  (is (ast-eval (parse 'smart-args-parser::plussign "+")) 'smart-args-parser::plussign)
  
  (is (ast-eval (parse 'smart-args-parser::variable "$foobar")) (cons 'smart-args-parser::variable "foobar"))
  (is (ast-eval (parse 'smart-args-parser::type "Test::WWW::Stub")) (cons 'smart-args-parser::type "Test::WWW::Stub"))
  (is (ast-eval (parse 'smart-args-parser::argtypeobject "{ isa => \"Int\" }")) (cons 'smart-args-parser::argtypeobject (list (cons 'smart-args-parser::argtypedefinition (cons "isa" (cons 'smart-args-parser::string "Int"))))))
  (is (ast-eval (parse 'smart-args-parser::string "\"foo\"")) (cons 'smart-args-parser::string "foo"))
  (is (ast-eval (parse 'smart-args-parser::string "'foo'")) (cons 'smart-args-parser::string "foo"))
  (is (ast-eval (parse 'smart-args-parser::argtypedefinition "isa => \"Int\"")) (cons 'smart-args-parser::argtypedefinition (cons "isa" (cons 'smart-args-parser::string "Int"))))
  (is (ast-eval (parse 'smart-args-parser::onearg "my $foobar => \"Int\""))
      (cons 'smart-args-parser::onearg
            (cons (cons 'smart-args-parser::variable "foobar")
                  (cons 'smart-args-parser::simpleargtype (cons 'smart-args-parser::type "Int")))))

  (is (ast-eval (parse 'smart-args-parser::onearg "my $foobar => { isa => 'Int', optional => 1 }"))
      (cons 'smart-args-parser::onearg
            (cons (cons 'smart-args-parser::variable "foobar")
                  (cons 'smart-args-parser::argtypeobject
                        (list
                         (cons 'smart-args-parser::argtypedefinition
                               (cons "isa" (cons 'smart-args-parser::string "Int")))
                         (cons 'smart-args-parser::argtypedefinition (cons "optional" 1)))))))
 (is (ast-eval (parse 'smart-args-parser::onearg "my $foobar => { isa => 'HashRef', default => +{ buzz => 1} }"))
      (cons 'smart-args-parser::onearg
            (cons (cons 'smart-args-parser::variable "foobar")
                  (cons 'smart-args-parser::argtypeobject
                        (list
                         (cons 'smart-args-parser::argtypedefinition
                               (cons "isa" (cons 'smart-args-parser::string "HashRef")))
                         (cons 'smart-args-parser::argtypedefinition
                               (cons "default"
                                     (cons 'smart-args-parser::hashref (list (cons "buzz" 1))))))))))
  )

(subtest "hashref"
  (is (ast-eval (parse 'smart-args-parser::hashref "{}"))
      (cons 'smart-args-parser::hashref nil))
  (is (ast-eval (parse 'smart-args-parser::hashref "+{}"))
      (cons 'smart-args-parser::hashref nil))
  (is (ast-eval (parse 'smart-args-parser::hashref "{foo => 'bar'}"))
      (cons 'smart-args-parser::hashref (list (cons "foo" (cons 'string "bar")))))
  (is (ast-eval (parse 'smart-args-parser::hashref "{foo => 'bar', piyo => 42}"))
      (cons 'smart-args-parser::hashref (list (cons "foo" (cons 'string "bar"))
                                             (cons "piyo" 42))))
  (is (ast-eval (parse 'smart-args-parser::hashref "{foo => 'bar', piyo => 42, buz => 1}"))
      (cons 'smart-args-parser::hashref (list (cons "foo" (cons 'string "bar"))
                                             (cons "piyo" 42)
                                             (cons "buz" 1))))
  (is (ast-eval (parse 'smart-args-parser::hashref "{foo => {bar => 1}}"))
      (cons 'smart-args-parser::hashref
            (list
             (cons "foo" (cons 'smart-args-parser::hashref
                               (list
                                (cons "bar" 1)))))))
  )

(subtest "extract-package-definition-from-stream"
  (is (extract-package-definition-from-stream (make-string-input-stream "package Foo::Bar1;"))
      (cons 'smart-args-parser::pkg "Foo::Bar1")))

(finalize)
