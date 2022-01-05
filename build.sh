#!/usr/bin/env bash

sbcl --eval "(ql:quickload :nordvpn-client)" --eval "(asdf:make :nordvpn-client)"
