LISP ?= sbcl

build:
	$(LISP) --load nordvpn-client.asd \
	  --eval '(ql:quickload :nordvpn-client)' \
	  --eval '(asdf:make :nordvpn-client)' \
	  --eval '(quit)'

