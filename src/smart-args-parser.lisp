(defpackage smart-args-parser
  (:use :cl :cl-ppcre :iterate :esrap-peg)
  (:import-from :alexandria :ends-with)
  (:export :load-peg :extract-subs-from-stream :parse-args-definition))
(in-package :smart-args-parser)

(defparameter *sub-heading-regex* (ppcre:create-scanner "^sub ([a-zA-Z0-9_]+) \\{"))

(defun extract-sub-heading-line (line)
  (multiple-value-bind (_ name) (ppcre:scan-to-strings *sub-heading-regex* line)
    (declare (ignorable _))
    (and name (elt name 0))))

(defun remove-comment (str)
  (subseq str 0 (or (position-if #'(lambda (c) (char= c #\#)) str)
                    (length str))))

(defun take-to-last-argument (strlis)
  (mapcar #'remove-comment
          (subseq strlis 0
                  (1+ (or
                       (position-if #'(lambda (str) (find #\; str))
                                    strlis)
                       (1- (length strlis)))))))

(defun extract-subs-from-stream (s)
  (let ((sub nil)
        (lines nil)
        (sub-string-map nil))
    (flet ((after-sub-p () sub)
           (pack-lines ()
             (setf sub-string-map
                   (acons sub (format nil "~{~A~^
~}" (take-to-last-argument (reverse lines))) sub-string-map))
             (setf lines nil))
           (wipe-lines () (setf lines nil))
           (enter-sub (sub-now) (setf sub sub-now))
           (append-line (line) (setf lines (cons line lines))))
      (iter (initially line nil)
        (for line next (read-line s nil :eof nil))
        (when (eq line :eof)
          (when (after-sub-p)
            (pack-lines))
          (return-from extract-subs-from-stream sub-string-map))
        (let ((subname-or-nil (extract-sub-heading-line line)))
          (if subname-or-nil
              (progn
                (if (after-sub-p)
                    (pack-lines)
                    (wipe-lines))
                (enter-sub subname-or-nil))
              (append-line line)))))))

(defun load-peg ()
    (peg-compile
     (parse-peg-file #P"src/smart-args.peg")))

(def-peg-fun spaces (x) 'spaces)
(def-peg-fun argsheading (x) 'args)
(def-peg-fun argsposheading (x) 'args-pos)
(def-peg-fun comma (x) 'comma)
(def-peg-fun semicolon (x) 'semicolon)
(def-peg-fun my (x) 'my)
(def-peg-fun dollar (x) 'dollar)
(def-peg-fun alpha (x) (char x 0))
(def-peg-fun digit (x) (char x 0))
(def-peg-fun leftbracket (x) (char x 0))
(def-peg-fun rightbracket (x) (char x 0))
(def-peg-fun number (x) (parse-integer (map 'string #'ast-eval x))) ; int only
(def-peg-fun colon (x) (char x 0))
(def-peg-fun underscore (x) (char x 0))
(def-peg-fun alnumsign (x) (ast-eval x))
(def-peg-fun variable (x) (cons 'variable (map 'string #'ast-eval (elt x 1))))
(def-peg-fun string (x)
             (cons 'string
                   (map
                    'string
                    #'(lambda (xx) (char (cadr xx) 0))
                    (elt x 1))))
(def-peg-fun plussign (x) 'plussign)

(def-peg-fun argtype (x)
             (ast-eval x))
(def-peg-fun simpleargtype (x)
             (cons 'simpleargtype (ast-eval (elt x 1))))
(def-peg-fun type (x)
             (cons 'type (map 'string #'(lambda (xx) (if (atom (car xx)) (ast-eval xx) (mapcar #'ast-eval xx))) x)))
(def-peg-fun argtypedefinition (x)
             (let ((key (map 'string #'ast-eval (elt x 0)))
                   (val (ast-eval (elt x 4))))
               (cons 'argtypedefinition (cons key val))))

(def-peg-fun argtypeobject (x)
             (let ((argtypedefinitions (elt x 3))
                   (last-argtypedefinition (elt x 4)))
               (cons 'argtypeobject
                     (append (if (null argtypedefinitions)
                                 nil
                                 (mapcar #'ast-eval (mapcar #'first argtypedefinitions)))
                             (list (ast-eval last-argtypedefinition))))))

(def-peg-fun onearg (x)
             (cons 'onearg (cons (ast-eval (elt x 2)) (ast-eval (elt x 6)))))

(def-peg-fun argsdefinition (x)
             (let ((heading (elt x 1))
                   (oneargs (mapcar #'first (elt x 3)))
                   (lastarg (first (elt x 4))))
               (list (ast-eval heading) (append (mapcar #'ast-eval oneargs) (if (null lastarg) nil (list (ast-eval lastarg)))))))

(def-peg-fun hashpair (x)
             (let* ((key (elt x 0))
                    (parsed-key (map 'string #'ast-eval key))
                    (val (elt x 4))
                    (parsed-val (ast-eval val)))
               (cons parsed-key parsed-val)))

(def-peg-fun hashref (x)
             (let* ((hashpairs (elt x 4))
                    (parsed-hashpairs (mapcar #'ast-eval (mapcar #'first hashpairs)))
                    (maybe-last-pair (elt x 6))
                    (parsed-last-pair (if maybe-last-pair (ast-eval maybe-last-pair) nil)))
               (cons 'hashref (if parsed-last-pair
                                  (append parsed-hashpairs (list parsed-last-pair))
                                  parsed-hashpairs))))

(defun parse-args-definition (str)
  (ast-eval (esrap:parse 'argsdefinition str)))
