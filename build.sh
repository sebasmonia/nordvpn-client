#!/usr/bin/env bash

sbcl --eval "(ql:quickload :nordlocations)" --eval "(asdf:make :nordlocations)"
