;;;; packages.lisp

(defpackage #:nordvpn-api
  (:nicknames :nordapi)
  (:use #:common-lisp)
  (:import-from :alexandria)
  (:import-from :uiop)
  (:import-from :shasht)
  (:import-from :dexador)
  (:export
   #:get-nord-servers
   #:get-coordinates-location))

(defpackage #:nordvpn-client-ui
  (:nicknames :nordui)
  (:use #:common-lisp #:nodgui)
  (:import-from :alexandria)
  (:import-from :uiop)
  (:import-from :nordvpn-api)
  (:export
   #:init))
