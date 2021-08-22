;;;; package.lisp

(defpackage #:nordlocations
  (:nicknames :nordl)
  (:use #:common-lisp #:nodgui)
  (:import-from :alexandria)
  (:import-from :uiop)
  (:import-from :jonathan)
  (:import-from :dexador)
  (:export
   #:init))

(in-package #:nordlocations)
