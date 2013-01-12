test:
	mit-scheme --compiler -heap 6000 --batch-mode --no-init-file --eval '(set! load/suppress-loading-message? #t)' --eval '(begin (load "load") (load "test/load") (run-tests-and-exit))'

.PHONY: test
