;;;; nordvpn-api.lisp
;;; Everything related to consuming the API lives here

(in-package #:nordvpn-api)

(defvar *nord-servers-url* "https://api.nordvpn.com/server"
  "URL to retrieve the list of VPN servers.")

(defvar *reverse-geocode-url*
  "https://geocode.arcgis.com/arcgis/rest/services/World/GeocodeServer/reverseGeocode?location=~$,~$&f=json"
  "URL template to replace the lat/long values and retrieve the location data.")

(defun get-nord-servers ()
  "Fetch and parse the list of servers from `*nord-servers-url*'."
  (jonathan:parse (dex:get "https://api.nordvpn.com/server")
                  :as :hash-table))

(defun get-coordinates-location (longitude latitude)
  "Call the API at `*reverse-geocode-url*' to resolve the location at LONGITUDE and LATITUDE."
  (gethash "address"
           (jonathan:parse (dex:get (format nil *reverse-geocode-url*
                                            longitude
                                            latitude))
                           :as :hash-table)))
