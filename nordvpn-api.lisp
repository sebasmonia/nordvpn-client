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

(defun get-recommended-servers (&optional country-id city-name)
  "Fetch and parse the recommended servers. If provided, COUNTRY-ID is passed in the API call."
  ;; I couldn't find online, nor figure out, how to filter by city in the API call, so I am taking
  ;; the same approach I've seen in a few other tools of getting all the servers for a country and
  ;; then filter the lowest-load one that matches the desired city
  (let* ((base-url (format nil "~a~a" *nord-api-url* "/recommendations?limit=1024"))
         (response (dex:get (if country-id
                                (format nil "~a&filters[country_id]=~a" base-url country-id)
                                base-url))))
    (shasht:read-json* :stream response)))

;; (defun json-to-lisp (string)
;;   (shasht:read-json* :stream string))

;; (defun lisp-to-json (alist)
;;   (shasht:write-json* alist :stream nil :alist-as-object t))
