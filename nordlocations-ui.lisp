;;;; nordlocations.lisp
;;; This file contains the  main UI code

(in-package #:nordlocations-ui)

(defvar *nord-servers-url* "https://api.nordvpn.com/server"
  "URL to retrieve the list of VPN servers.")

(defvar *reverse-geocode-url*
  "https://geocode.arcgis.com/arcgis/rest/services/World/GeocodeServer/reverseGeocode?location=~$,~$&f=json"
  "URL template to replace the lat/long values and retrieve the location data.")

(defvar *servers-data* nil
  "List with server information, once it is retrieved from the location specified in `*nord-servers-url*'.")

(defvar *selected-server* nil
  "Holds a reference to the currently selected server in `*servers-listbox*'.")

(defvar *location-info-template* "Region:~%~a~3%Subregion:~%~a~3%MetroArea:~%~a~3%City:~%~a~%"
  "Template to show the location information in `*location-label*'.")

;; Controls bound to global variables because they are accessed from multiple functions.
(defvar *servers-listbox* nil "Reference to the listbox that has the server list.")
(defvar *location-label* nil "Reference to the label that needs to be updated with location data.")
(defvar *status-label* nil "Reference to the label that reflects the current status.")

(defvar *help-text*
  "Execute \"nordlocations\" to display a window to retrieve the list of NordVPN servers.
When you select a server name, on the side you will see its location info.")

(defun init ()
  "Start the UI, show the help text if needed"
  (let ((arguments (uiop:command-line-arguments)))
    (when (string= "-h" (first arguments))
      (show-help-and-exit)))
  ;; This would be a good place to set *nord-servers-url* and
  ;; *reverse-geocode-url* from a config file. Or something.
  (start-ui))

(defun show-help-and-exit ()
  "Display `*help-text' and return a success exit code."
  (format t *help-text*)
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
                                       :fill :both
                                       :expand t
                                       :matching-fn #'searchable-listbox-match-ignore-case
                                       :remove-non-matching-p t))
           (location-title-label (make-instance 'label
                                         :width 30
                                         :text "Location info:"))
           (location-info-label (make-instance 'label
                                               :text ""))
           (fetch-servers-button (make-instance 'button
                                                :text "Get server list"
                                                :command #'fetch-servers-click-start))
           (status-frame (make-instance 'labelframe
                                        :text "Status:"))
           (status-label (make-instance 'label
                                        :master status-frame
                                        :text "Press [space] or click the button to get the server list.")))
      (setf *servers-listbox* server-list)
      (setf *location-label* location-info-label)
      (setf *status-label* status-label)
      ;; make the listbox in the seachable-listbox wider and taller than the default
      (configure (listbox *servers-listbox*) :height  20)
      (configure (listbox *servers-listbox*) :width  30)
      ;; start by setting focus  on the button to fetch the servers, so Enter triggers the action
      (focus fetch-servers-button)
      ;; After fetching the servers, the focus is moved to the searchable listbox entry
      ;; make it so that pressing Enter in the entry moves focus to the list
      (bind (entry server-list) "<Return>" #'server-list-entry-enter-key)
      ;; when the listbox selection changes, we have to fetch the data
      (bind (listbox server-list) "<<ListboxSelect>>" #'server-list-selected-start)

      (grid server-label 0 0 :padx 10 :pady 10 :sticky "w")
      (grid fetch-servers-button 0 1 :padx 10 :pady 10 :sticky "w")
      (grid server-list 1 0 :padx 10 :pady 10 :sticky "w" :columnspan 2)
      (grid location-title-label 0 2 :padx 10 :pady 10 :sticky "w")
      (grid location-info-label 1 2 :padx 10 :pady 10 :sticky "w")

      (grid status-frame 2 0 :padx 10 :pady 10 :sticky "nswe" :columnspan 3)
      (grid status-label 0 0 :padx 10 :pady 10 :sticky "w")

      (grid-columnconfigure *tk* :all :weight 1)
      (grid-rowconfigure    *tk* :all :weight 1))))

(defun fetch-servers-click-start ()
  "Setup the UI and then call `fetch-servers-click-end'."
  (listbox-delete *servers-listbox*)
  (setf (text *status-label*) "Retrieving the server list..."
        (text *location-label*) "")
  ;; noto so subtle attempt to delay a bit the next step...
  (nodgui:after 100 #'fetch-servers-click-end))

(defun get-server-name (item)
  (gethash "name" item))

(defun fetch-servers-click-end ()
  "Fill the server listbox with the list of servers obtained using the API package."
  (setf *servers-data* (sort (nordapi:get-nord-servers)
                             #'string-lessp
                             :key #'get-server-name))
  (listbox-delete *servers-listbox*)
  (listbox-append *servers-listbox* (loop for server in *servers-data*
                                          collect (get-server-name server)))
  (setf (text *status-label*) "-")
  (focus (entry *servers-listbox*)))

(defun server-list-selected-start (evt)
  "Setup the UI and then call `server-list-selected-end'."
  (declare (ignore evt))
  (let* ((server-name (first (listbox-get-selection-value *servers-listbox*)))
         (server-item (find server-name *servers-data* :key #'get-server-name :test #'string=)))
    (setf *selected-server* server-item
          (text *location-label*) ""
          (text *status-label*) (format nil
                                           "Getting location data for ~a..."
                                           server-name))
    ;; not so subtle attempt to delay a bit the next step...
    (nodgui:after 100 #'server-list-selected-end)))

(defun server-list-selected-end ()
  "Use the coordinates of the selected server to resolve the location, and display the information."
  (let* ((location-info (gethash "location" *selected-server*))
         (geo-data (nordapi:get-coordinates-location (gethash "long" location-info)
                                                     (gethash "lat" location-info))))
    (setf (text *location-label*) (format nil *location-info-template*
                                          (value-or-dash "Region" geo-data)
                                          (value-or-dash "Subregion" geo-data)
                                          (value-or-dash "MetroArea" geo-data)
                                          (value-or-dash "City" geo-data))))
  (setf (text *status-label*) "-"))

(defun server-list-entry-enter-key (evt)
  "Event handle for pressing Enter focused on the search box.
Selects the first element in the listbox and changes focus to it."
  (declare (ignore evt))
  (let ((the-listbox (listbox *servers-listbox*)))
    (listbox-select the-listbox 0)
    (focus the-listbox)
    (server-list-selected-start nil)
    ))

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
