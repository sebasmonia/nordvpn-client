;;;; packages.lisp

(defpackage #:nordvpn-api
  (:nicknames :nordapi)
  (:use #:common-lisp)
  (:import-from :alexandria)
  (:import-from :uiop)
  (:import-from :shasht)
  (:import-from :dexador)
  (:export
   #:get-countries-cities
   #:get-best-server-current-location
   #:get-best-server-for-city))

(defpackage #:nordvpn-client-ui
  (:nicknames :nordui)
  (:use #:common-lisp #:nodgui)
  (:import-from :alexandria)
  (:import-from :uiop)
  (:import-from :nordvpn-api)
  (:export
   #:init))
