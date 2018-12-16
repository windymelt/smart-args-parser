# Smart-Args-Parser

Parser for `Smart::Args`

## Usage

`parse-smart-args.ros` parses perl module file and prints parsed s-exps.

```shell
$ ros -S . roswell/parse-smart-args.ros /path/to/perl/module.pm
```

### Example

```shell
$ ros -S . roswell/parse-smart-args.ros tests/Test.pm 

(("sub2" SMART-ARGS-PARSER::ARGS
  ((SMART-ARGS-PARSER::ONEARG (VARIABLE . "class")
    SMART-ARGS-PARSER::SIMPLEARGTYPE TYPE . "ClassName")
   (SMART-ARGS-PARSER::ONEARG (VARIABLE . "arg2")
    SMART-ARGS-PARSER::SIMPLEARGTYPE TYPE . "Int")
   (SMART-ARGS-PARSER::ONEARG (VARIABLE . "arg3")
    SMART-ARGS-PARSER::SIMPLEARGTYPE TYPE . "ArrayRef")))
 ("sub1" SMART-ARGS-PARSER::ARGS
  ((SMART-ARGS-PARSER::ONEARG (VARIABLE . "class")
    SMART-ARGS-PARSER::SIMPLEARGTYPE TYPE . "ClassName")
   (SMART-ARGS-PARSER::ONEARG (VARIABLE . "arg1")
    SMART-ARGS-PARSER::SIMPLEARGTYPE TYPE . "Str"))))
```

## Installation

You can install smart-args-parser with [qlot](https://github.com/fukamachi/qlot):

```
; qlfile
github smart-args-parser windymelt/smart-args-parser :branch master
```

## Author

* Windymelt

## Copyright

Copyright (c) 2018 Windymelt
