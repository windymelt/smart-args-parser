.PHONY: test

test:
	ros run -S . -e '(asdf:compile-system :parse-smart-args :force t)' -e '(asdf:test-system :parse-smart-args)' -q
