.PHONY: test

test:
	ros run -S . -e '(asdf:compile-system :smart-args-parser :force t)' -e '(asdf:test-system :smart-args-parser)' -q
