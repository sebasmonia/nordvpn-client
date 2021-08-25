;;;; nordlocations.lisp

(in-package #:nordlocations)

(defvar *nord-servers-url* "https://api.nordvpn.com/server" "URL to retrieve the list of VPN servers.")
(defvar *reverse-geocode-url* "https://geocode.arcgis.com/arcgis/rest/services/World/GeocodeServer/reverseGeocode?location=~$,~$&f=json" "URL template to replace the lat/long values and retrieve the location data.")
(defvar *servers-data* nil "List with server information, once it is retrieved from the location specified in `*nord-servers-url*'.")

(defvar *selected-server* nil "Holds a reference to the currently selected server in `*servers-listbox*'.")

(defvar *location-info-template*
  "Region:~%~a~%~%Subregion:~%~a~%~%MetroArea:~%~a~%~%City:~%~a~%"
  "Template to show the location information in `*location-label*'.")


;; Controls bound to global variables because they are accessed from multiple functions.
(defvar *servers-listbox* nil "Holds a reference to the listbox that has the server list.")
(defvar *location-label* nil "Holds a reference to the label that needs to be updated with location data.")
(defvar *operation-label* nil "Holds a reference to the label that reflects the current operation.")

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

(defun searchable-listbox-match-ignore-case (entry-text item-text)
  "Return non-nil if ENTRY-TEXT is contained in ITEM-TEXT.
Unlike the default match function in searchable-listbox, this one is case insensitive."
  (search entry-text item-text :test #'char-equal))

(defun start-ui ()
  "Launches the UI for nordlocations."
  (with-nodgui (:title "NordVPN - Server Locations")
    (font-configure "TkDefaultFont" :size 12)
    (font-configure "TkTextFont" :size 12)
    (let* ((server-label (make-instance 'label
                                        :text "Server:"))
           (server-list (make-instance 'nodgui.mw:searchable-listbox
                                       :matching-fn #'searchable-listbox-match-ignore-case
                                       :remove-non-matching-p t))
           (location-title-label (make-instance 'label
                                          :text "Location info:"))
           (location-info-label (make-instance 'label
                                               :text (make-string 80 :initial-element #\Space)))
           (fetch-servers-button (make-instance 'button
                                                :text "Get server list"
                                                :command #'populate-server-list-start))
           (operation-frame (make-instance 'labelframe
                                               :text "Current operation:"))
           (operation-label (make-instance 'label
                                        :master operation-frame
                                        :text "-")))
      (setf *servers-listbox* server-list)
      (setf *location-label* location-info-label)
      (setf *operation-label* operation-label)
      ;; focus on the combo to select the container
      (focus fetch-servers-button)
      ;; Make fetch-servers-button the default action...need testing to determine
      ;; if this is convenient or annoying
      (bind *tk* "<Return>" (lambda (evt)
                              (declare (ignore evt))
                              (funcall (command fetch-servers-button))))
      ;; when the listbox selection changes, we have to fetch the data
      (bind (listbox server-list) "<<ListboxSelect>>" #'handle-server-selected-start)

      (grid server-label 0 0 :padx 10 :pady 10 :sticky "w")
      (grid fetch-servers-button 0 1 :padx 10 :pady 10 :sticky "w")
      (grid server-list 1 0 :padx 10 :pady 10 :sticky "w" :columnspan 2)
      (grid location-title-label 0 2 :padx 10 :pady 10 :sticky "w")
      (grid location-info-label 1 2 :padx 10 :pady 10 :sticky "w")

      (grid operation-frame 2 0 :padx 10 :pady 10 :sticky "nswe" :columnspan 3)
      (grid operation-label 0 0 :padx 10 :pady 10 :sticky "w")

      (grid-columnconfigure *tk* :all :weight 1)
      (grid-rowconfigure    *tk* :all :weight 1))))

(defun populate-server-list-start ()
  "Setup the UI and then call `populate-server-list-end'."
  (listbox-delete *servers-listbox*)
  (setf (text *operation-label*) "Retrieving the server list"
        (text *location-label*) "")
  ;; noto so subtle attempt to delay a bit the next step...
  (nodgui:after 100 #'populate-server-list-end))

(defun get-server-name (item)
  (gethash "name" item))

(defun populate-server-list-end ()
  "Fetch the list of servers from `*nord-servers-url*' and fill the server listbox."
  (let ((parsed-server-data (jonathan:parse (dex:get "https://api.nordvpn.com/server")
                                            :as :hash-table)))
    (setf *servers-data* (sort parsed-server-data
                               #'string-lessp
                               :key #'get-server-name))
    (listbox-delete *servers-listbox*)
    (listbox-append *servers-listbox* (loop for server in *servers-data*
                                            collect (get-server-name server)))
  (setf (text *operation-label*) "-")))

(defun handle-server-selected-start (evt)
  "Setup the UI and then call `handle-server-selected-end'."
  (let* ((server-name (first (listbox-get-selection-value *servers-listbox*)))
         (server-item (find server-name *servers-data* :key #'get-server-name :test #'string=)))
    (setf *selected-server* server-item
          (text *location-label*) ""
          (text *operation-label*) (format nil
                                           "Getting location data for ~a"
                                           server-name))
    ;; not so subtle attempt to delay a bit the next step...
    (nodgui:after 100 #'handle-server-selected-end)))

(defun handle-server-selected-end ()
  "Call the API at `*reverse-geocode-url*' for the selected server, and display the information."
  (let* ((location-info (gethash "location" *selected-server*))
         (geo-data (gethash "address"
                            (jonathan:parse (dex:get (format nil *reverse-geocode-url*
                                                             (gethash "long" location-info)
                                                             (gethash "lat" location-info)))
                                            :as :hash-table))))
    (setf (text *location-label*) (format nil *location-info-template*
                                          (value-or-dash "Region" geo-data)
                                          (value-or-dash "Subregion" geo-data)
                                          (value-or-dash "MetroArea" geo-data)
                                          (value-or-dash "City" geo-data))))
  (setf (text *operation-label*) "-"))

(defun run-program-and-exit ()
  "Run the program according to the parameters in the UI and exit."
  ;; (uiop:launch-program
   ;;(format nil "toolbox run -c ~a ~a" *selected-toolbox* *command-text*))
   ;; got this from reading the source, maybe there is a better way
  (setf *exit-mainloop* t)
  (uiop:quit 0))

(defun value-or-dash (key ht)
  (let ((value (gethash key ht)))
    (if (= (length value) 0)
        "-"
        value)))
