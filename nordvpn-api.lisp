;;;; nordvpn-api.lisp
;;; Everything related to consuming the API lives here

(in-package #:nordvpn-api)

(defvar *nord-api-url* "https://api.nordvpn.com/v1/servers"
  "Base URL of the NordVPN API.")

(defun get-countries-cities ()
  "Fetch and parse the list of countries and their cities."
  (flet ((extract-countries-cities (country-ht)
           `((:name . ,(gethash "name" country-ht))
             (:id . ,(gethash "id" country-ht))
             (:cities . ,(loop for city-ht across (gethash "cities" country-ht)
                               collect (gethash "name" city-ht))))))
    (let ((response (dex:get (format nil "~a~a" *nord-api-url* "/countries"))))
      (loop for parsed-node across (shasht:read-json* :stream response)
            collect (extract-countries-cities parsed-node)))))

;; (defun json-to-lisp (string)
;;   (shasht:read-json* :stream string))

;; (defun lisp-to-json (alist)
;;   (shasht:write-json* alist :stream nil :alist-as-object t))
