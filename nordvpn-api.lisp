;;;; nordvpn-api.lisp
;;; Everything related to consuming the API lives here

(in-package #:nordvpn-api)

(defvar *nord-api-url* "https://api.nordvpn.com/v1/servers" "Base URL of the NordVPN API.")

(defvar *open-vpn-udp-name* "OpenVPN UDP"
  "Technology \"name\" for OpenVPN UDP in the response for /recommendations")
(defvar *api-call-limit* 5000
  "Number of items to return per API call. Currently used in when fetching by country.")

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

(defun filter-recommended-servers-openvpn-udp (recommended-server-list)
  "Helper to filter a server list to only the ones that support OpenVPN UDP."
  ;; keeping this for reference, but it seems all servers support OpenVPN, so :shrug:
  (loop for a-server across recommended-server-list
        for has-open-vpn-udp = (loop for tech across (gethash "technologies" a-server)
                                     when (string= (gethash "name" tech) *open-vpn-udp-name*)
                                       do (return t))
        when has-open-vpn-udp
          collect recommended-server-list))

(defun get-best-server-current-location ()
  "Fetch and parse the recommended servers. If provided, COUNTRY-ID is passed in the API call."
  (let ((ten-best-servers (shasht:read-json* :stream (dex:get (format nil
                                                                      "~a/recommendations?limit=10"
                                                                      *nord-api-url*)))))
    ;; Servers are returned sorted load. Return the first match:
    (elt ten-best-servers 0)))

(defun get-recommended-servers-for-country (country-id)
  "Fetch and parse the recommended servers for a given COUNTRY-ID."
  ;; I couldn't find online, nor figure out, how to filter by city in the API call, so I am taking
  ;; the same approach I've seen in a few other tools of getting all the servers for a country and
  ;; then filter the lowest-load one that matches the desired city
  (shasht:read-json* :stream (dex:get (format nil
                                              "~a/recommendations?limit=~a&filters[country_id]=~a"
                                              *nord-api-url*
                                              *api-call-limit*
                                              country-id))))

(defun get-best-server-for-city (country-id city-name)
  "Call `get-recommended-servers-for-country' with COUNTRY-ID, and filter the output by CITY-NAME."
  ;; Experimentation shows that the sever list is sorted by city name and then load,
  ;; if that stops being the case, this function would need to check the value for "load" too
  (flet ((server-city (s)
           (gethash "name" (gethash "city" (gethash "country" (elt (gethash "locations" s) 0))))))
  (let* ((country-servers (get-recommended-servers-for-country country-id)))
    (loop for a-server across country-servers
          when (string-equal city-name (server-city a-server))
            do (return a-server)))))

;; (defun json-to-lisp (string)
;;   (shasht:read-json* :stream string))

;; (defun lisp-to-json (alist)
;;   (shasht:write-json* alist :stream nil :alist-as-object t))
