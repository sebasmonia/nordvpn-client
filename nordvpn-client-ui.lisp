;;;; nordvpn-client-ui.lisp
;;; This file contains the  main UI code

(in-package #:nordvpn-client-ui)

(defvar *countries-cities* nil
  "List of countries and their cities, as reported from the NordVPN API, formatted in an alist.")

(defvar *selected-country-city* "" "The text selected in `*cities-listbox*' after clicking on it.")
(defvar *recommended-server-data "" "Data of the currently recommended server.")

(defvar *recommended-info-template* "Id:~%~a~3%Hostname:~%~a~3%Name:~%~a~3%Load:~%~a~%"
  "Template to show the location information in `*recommended-label*'.")

;; Controls bound to global variables because they are accessed from multiple functions.
(defvar *cities-listbox* nil "Listbox that displays countries and cities.")
(defvar *recommended-label* nil "Label that will display the recommended server data.")
(defvar *status-label* nil "Label that reflects the current status.")
(defvar *connect-button* nil "Button to connect to the server displayed in `*recommended-label*'.")

(defvar *auto-connect* nil "Whether the flag --auto was provided when starting the app.")

(defvar *help-text*
  "Execute \"nordvpn-client\" to display a window that allows finding a city to
connect to, or the best \"local\" server, and then create & open the VPN
connection in Network Manager.

If you use the flag \"--auto\", the client to try to connect on startup without
user intervention.

For this application to work, you need to setup in your keyring proper values
for \"nordvpn-client username\" and \"nordvpn-client password\".

This tool was created an an exercise to practice Common Lisp, and also as a
convenience in Fedora Silverblue, where installing the official NordVPN rpm
needs \"layering\".
Visit https://github.com/sebasmonia/nordvpn-client for more information.

")

(defun init ()
  "Start the UI, or show the help text if needed."
  (let ((arguments (uiop:command-line-arguments)))
    (when (member "-h" arguments :test #'string=)
      (format t *help-text*)
      (uiop:quit 0))
    (when (member "--auto" arguments :test #'string=)
      (setf *auto-connect* t))
    (start-ui)))

(defun searchable-listbox-match-ignore-case (entry-text item-text)
  "Return non-nil if ENTRY-TEXT is contained in ITEM-TEXT.
Unlike the default match function in searchable-listbox, this one is case insensitive."
  (search entry-text item-text :test #'char-equal))

(defun start-ui ()
  "Launches the UI for nordvpn-client, and sets up the call to `populate-cities-listbox'."
  (with-nodgui (:title "NordVPN (unofficial) Client")
    (font-configure "TkDefaultFont" :size 12)
    (font-configure "TkTextFont" :size 12)
    (let* ((cities-label (make-instance 'label
                                           :text "Countries & cities:"))
           (cities-listbox (make-instance 'nodgui.mw:searchable-listbox
                                          :fill :both
                                          :expand t
                                          :matching-fn #'searchable-listbox-match-ignore-case
                                          :remove-non-matching-p t))
           (get-recommended-local-button (make-instance 'button
                                                        :text "Detect best local server"
                                                        :command #'get-recommended-local-start))
           (recommended-title-label (make-instance 'label
                                                   :width 30
                                                   :text "Recommended server:"))
           (recommended-info-label (make-instance 'label
                                                  :text ""))
           (connect-to-server-button (make-instance 'button
                                                    :state :disabled
                                                    :text "!!! CONNECT !!!"
                                                    :command #'create-and-open-connection))
           (status-frame (make-instance 'labelframe
                                        :text "Status:"))
           (status-label (make-instance 'label
                                        :master status-frame
                                        :text "")))
      ;; make the listbox in the seachable-listbox wider and taller than the default
      (configure (listbox cities-listbox) :height  20)
      (configure (listbox cities-listbox) :width  30)
      ;; start by setting focus on the button to get the local server, so hitting the
      ;; spacebar triggers it
      (focus get-recommended-local-button)
      ;; make it so that pressing Enter in the entry is equivalent to clicking
      ;; the first item on the list
      (bind (entry cities-listbox) "<Return>" #'cities-listbox-entry-enter-key)
      ;; when the listbox selection changes, we need to update the recommended server information
      (bind (listbox cities-listbox) "<<ListboxSelect>>" #'cities-listbox-selected-start)

      (grid cities-label 0 0 :padx 10 :pady 10 :sticky "w")
      (grid cities-listbox 1 0 :padx 10 :pady 10 :sticky "w" :columnspan 2)

      (grid recommended-title-label 0 2 :padx 10 :pady 10 :sticky "w")
      (grid recommended-info-label 1 2 :padx 10 :pady 10 :sticky "w")

      (grid get-recommended-local-button 2 0 :padx 10 :pady 10 :sticky "we")
      (grid connect-to-server-button 2 2 :padx 10 :pady 10 :sticky "we")

      (grid status-frame 3 0 :padx 10 :pady 10 :sticky "nswe" :columnspan 3)
      ;; The status label is on 0,0 but _inside the frame_
      (grid status-label 0 0 :padx 10 :pady 10 :sticky "w")

      (grid-columnconfigure *tk* :all :weight 1)
      (grid-rowconfigure    *tk* :all :weight 1)

      ;; Keep a reference to some of the controls, as we will need to access them globally
      (setf *cities-listbox* cities-listbox)
      (setf *recommended-label* recommended-info-label)
      (setf *status-label* status-label)
      (setf *connect-button* connect-to-server-button)

      (setf (text status-label) "Retrieving countries and cities...")
      (nodgui:after 50 #'populate-cities-listbox))))

(defun populate-cities-listbox ()
  "Shows the list of countries. This function is called right after initializing the UI.
Being the last step of UI initialization, this function also checks if it needs to start the
auto-connect process."
  (flet ((format-countries-cities ()
           (loop for country in *countries-cities*
                 for name = (alexandria:assoc-value country :name)
                 nconc (loop for city in (alexandria:assoc-value country :cities)
                             collect (format nil "~a  -  ~a" name city)))))
    (setf *countries-cities* (nordvpn-api:get-countries-cities))
    (listbox-append *cities-listbox* (format-countries-cities))
    (setf (text *status-label*) ""))
  (when *auto-connect*
    (get-recommended-local-start)))

(defun cities-listbox-selected-start (evt)
  "Setup the UI and then call `cities-listbox-selected-end'."
  (declare (ignore evt))
  (let ((selected-text (first (listbox-get-selection-value *cities-listbox*))))
    (setf (text *recommended-label*) ""
          (text *status-label*) (format nil
                                        "Retrieving best server for \"~a\"..."
                                        selected-text))
    ;; Delay a bit the next step so the status label updates
    (nodgui:after 50 (lambda () (cities-listbox-selected-end selected-text)))))

(defun cities-listbox-selected-end (selected-text)
  "Use the information in SELECTED-TEXT to get the recommended server."
  (flet ((find-country-node-by-name (name)
           ;; Unless something is very wrong, `find-if' will always return an item, so:
           (find-if (lambda (item)
                      (string= (alexandria:assoc-value item :name) name))
                    *countries-cities*)))
  ;; this logic is fickle, I tried to account for country names with "-" in ther name
  ;; by using two spaces before/after the - char
  (let* ((country-name (subseq selected-text 0 (search "  -  " selected-text)))
         (city-name (subseq selected-text (+ 5 (search "  -  " selected-text))))
         (country-id (alexandria:assoc-value (find-country-node-by-name country-name) :id)))
    (prepare-to-connect (nordvpn-api:get-best-server-for-city country-id city-name)))))

(defun get-recommended-local-start ()
  "Setup the UI and then call `get-recommended-local-end'."
  (setf (text *recommended-label*) ""
        (text *status-label*) "Retrieving best server for the current location...")
  ;; Delay a bit the next step so the status label updates
  (nodgui:after 50 #'get-recommended-local-end))

(defun get-recommended-local-end ()
  "Use the API to retrieve the recommended server for the current location, and display it."
  (prepare-to-connect (nordvpn-api:get-best-server-current-location)))

(defun prepare-to-connect (server-data)
  "Store SERVER-DATA in `*recommended-server-data', and also display it in `*recommended-label*'.
If the auto-connect flag was set on start up, then try to connect."
  (setf *recommended-server-data* server-data)
  (setf (text *recommended-label*) (format nil *recommended-info-template*
                                           (gethash "id" server-data)
                                           (gethash "hostname" server-data)
                                           (gethash "name" server-data)
                                           (gethash "load" server-data)))
  (setf (text *status-label*) "")
  (configure *connect-button* :state :active)
  (focus *connect-button*)
  (when *auto-connect*
    (setf *auto-connect* nil)
    (create-and-open-connection)))

(defun cities-listbox-entry-enter-key (evt)
  "Event handler for pressing Enter focused on the search box.
Selects the first element in the listbox and act as if it was clicked."
  (declare (ignore evt))
  (let ((the-listbox (listbox *cities-listbox*)))
    (listbox-select the-listbox 0)
    (focus the-listbox)
    (cities-listbox-selected-start nil)))

(defun create-and-open-connection ()
  "Start the process of connecting to the server in `*recommended-server-data*'.
In order to keep updating the UI, this function kicks off a 3 step chain."
  (let ((hostname (gethash "hostname" *recommended-server-data*)))
    (setf (text *status-label*) (format nil "Downloading configuration file for ~a" hostname))
    (configure *connect-button* :state :disabled)
    (nodgui:after 50 (lambda () (step-1-download-ovpn-file hostname)))))

(defun step-1-download-ovpn-file (hostname)
  "Download the ovpn file for HOSTNAME and call step 2 (create the connection)"
  (let ((config-filename (nordvpn-api:download-openvpn-config-file hostname)))
    (setf (text *status-label*) "Creating connection in Network Manager...")
    (nodgui:after 50 (lambda () (step-2-create-connection config-filename)))))

(defun step-2-create-connection (config-filename)
  "Import in NM the connection in CONFIG-FILENAME, setup the \"connect\" step."
  (multiple-value-bind (message nm-connection-name) (nmcli-wrapper:create-connection
                                                     config-filename)
    (if message
        (progn
          (setf (text *status-label*) message)
          (configure *connect-button* :state :active))
        (progn
          (setf (text *status-label*) (format nil "Connecting to server ~a..." nm-connection-name))
          (nodgui:after 50 (lambda () (step-3-create-connection nm-connection-name)))))))

(defun step-3-create-connection (nm-connection-name)
  "Open NM-CONNECTION-NAME and get the UI back to its original state."
  (let ((message (nmcli-wrapper:open-connection nm-connection-name)))
    (setf (text *status-label*) (or message (format nil "Connected to ~a!" nm-connection-name)))
    (configure *connect-button* :state :disabled)
    ;; clear the message after 5 seconds - unless it was an error
    (unless message
      (nodgui:after 5000 (lambda () (setf (text *status-label*) ""))))))
