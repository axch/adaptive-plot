oops.png: oops.scm
	mit-scheme --compiler -heap 6000 --batch-mode --no-init-file \
	--eval '(set! load/suppress-loading-message? #t)' \
	--eval '(load "$<")'
