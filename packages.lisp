;;;; packages.lisp

(defpackage #:nordvpn-api
  (:use #:common-lisp)
  (:import-from :alexandria)
  (:import-from :uiop)
  (:import-from :shasht)
  (:import-from :drakma)
  (:export
   #:get-countries-cities
   #:get-best-server-current-location
   #:get-best-server-for-city
   #:download-openvpn-config-file))

(defpackage #:nmcli-wrapper
  (:use #:common-lisp)
  (:import-from :alexandria)
  (:import-from :uiop)
  (:export
   #:*nmcli-executable*
   #:create-connection
   #:open-connection))

(defpackage #:nordvpn-client-ui
  (:use #:common-lisp #:nodgui)
  (:import-from :alexandria)
  (:import-from :uiop)
  (:import-from :nordvpn-api)
  (:export
   #:init))
