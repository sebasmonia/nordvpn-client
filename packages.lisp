;;;; packages.lisp

(defpackage #:nordlocations-api
  (:nicknames :nordapi)
  (:use #:common-lisp)
  (:import-from :alexandria)
  (:import-from :uiop)
  (:import-from :jonathan)
  (:import-from :dexador)
  (:export
   #:get-nord-servers
   #:get-coordinates-location))

(defpackage #:nordlocations-ui
  (:nicknames :nordui)
  (:use #:common-lisp #:nodgui)
  (:import-from :alexandria)
  (:import-from :uiop)
  (:import-from :nordlocations-api)
  (:export
   #:init))
