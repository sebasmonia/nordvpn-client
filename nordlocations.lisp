;;;; nordlocations.lisp

(in-package #:nordlocations)

(defvar *nord-servers-url* "https://api.nordvpn.com/server" "URL to retrieve the list of VPN servers.")
(defvar *reverse-geocode-url* "https://geocode.arcgis.com/arcgis/rest/services/World/GeocodeServer/reverseGeocode?location=~a,~a&f=json" "URL template to replace the lat/long values and retrieve the location data.")
(defvar *servers-data* nil "List with server information, once it is retrieved from the location specified in `*nord-servers-url*'.")

;; Controls bound to global variables because they are accessed from multiple functions.
(defvar *servers-listbox* nil "Holds a reference to the listbox that has the server list.")
(defvar *location-label* nil "Holds a reference to the label that needs to be updated with location data.")

(defvar *help-text* "Execute \"nordlocations\" to display a window to retrieve the list of NordVPN servers. When you select a server name, on the side you will see its location info.")

(defun init ()
  "Start the UI, show the help text if needed"
  (let ((arguments (uiop:command-line-arguments)))
    (when (string= "-h" (first arguments))
      (show-help-and-exit)))
  ;; This would be a good place to set *nord-servers-url* and
  ;; *reverse-geocode-url* from a config file. Or something.
  (start-ui))

(defun show-help-and-exit ()
  (format t "~a~%" *help-text*)
  (uiop:quit 0))

(defun start-ui ()
  "Launches the UI for nordlocations."
  (with-nodgui (:title "NordVPN - Server Locations")
    (font-configure "TkDefaultFont" :size 12)
    (font-configure "TkTextFont" :size 12)
    (let* ((server-label (make-instance 'label
                                        :text "Server:"))
           (server-list (make-instance 'scrolled-listbox))
           (location-title-label (make-instance 'label
                                          :text "Location info:"))
           (location-info-label (make-instance 'label
                                               :text (make-string 40 :initial-element #\Space)))
           (fetch-servers-button (make-instance 'button
                                                :text "Get server list"
                                                :command #'populate-server-list-start)))
      (setf *servers-listbox* server-list)
      (setf *location-label* location-info-label)
      ;; focus on the combo to select the container
      (focus fetch-servers-button)
      ;; Make fetch-servers-button the default action...need testing to determine
      ;; if this is convenient or annoying
      (bind *tk* "<Return>" (lambda (evt)
                              (declare (ignore evt))
                              (funcall (command fetch-servers-button))))
      ;; when the listbox selection changes, we have to fetch the data
      (bind (listbox server-list) "<<ListboxSelect>>" #'handle-server-selected)

      (grid server-label 0 0 :padx 10 :pady 10 :sticky "w")
      (grid fetch-servers-button 0 1 :padx 10 :pady 10 :sticky "w")
      (grid server-list 1 0 :padx 10 :pady 10 :sticky "w" :columnspan 2)
      (grid location-title-label 0 2 :padx 10 :pady 10 :sticky "w")
      (grid location-info-label 1 2 :padx 10 :pady 10 :sticky "w")
      (grid-columnconfigure *tk* :all :weight 1)
      (grid-rowconfigure    *tk* :all :weight 1))))

(defun populate-server-list-start ()
  "Fetch the list of servers from `*nord-servers-url*' and fill the server listbox."
  (listbox-delete *servers-listbox*)
  (listbox-append *servers-listbox* "Retrieving the servers list...")
  ;; not so subtle attempt to delay a bit the next step...
  (nodgui:after 100 #'populate-server-list-complete))

(defun populate-server-list-complete ()
  (let ((parsed-server-data (jonathan:parse (dex:get "https://api.nordvpn.com/server")
                                            :as :hash-table)))
    (setf *servers-data* (sort parsed-server-data
                               #'string-lessp
                               :key (lambda (server) (gethash "name" server))))
    (listbox-delete *servers-listbox*)
    (loop for server in *servers-data*
          do (listbox-append *servers-listbox* (gethash "name" server)))))

(defun handle-server-selected (evt)
  (let ((item-index (first (first (listbox-get-selection *servers-listbox*)))))
    (format t "raw: ~a index: ~a" (listbox-get-selection *servers-listbox*) item-index)
    (format t "Get the info for ~a ~%" (gethash "domain" (nth item-index *servers-data*)))))

(defun run-program-and-exit ()
  "Run the program according to the parameters in the UI and exit."
  (uiop:launch-program
   (format nil "toolbox run -c ~a ~a" *selected-toolbox* *command-text*))
   ;; got this from reading the source, maybe there is a better way
  (setf *exit-mainloop* t)
  (uiop:quit 0))
