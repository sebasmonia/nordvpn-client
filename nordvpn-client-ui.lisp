;;;; nordvpn-client-ui.lisp
;;; This file contains the  main UI code

(in-package #:nordvpn-client-ui)

(defvar *countries-cities* nil
  "List of countries and their cities, as reported from the NordVPN API, formatted in an alist.")

(defvar *selected-country-city* "" "The text selected in `*cities-listbox*' after clicking on it.")

(defvar *recommended-info-template* "Id:~%~a~3%Name:~%~a~3%Load:~%~a~%"
  "Template to show the location information in `*recommended-label*'.")

;; Controls bound to global variables because they are accessed from multiple functions.
(defvar *cities-listbox* nil "Listbox that displays countries and cities.")
(defvar *recommended-label* nil "Label that will display the recommended server data.")
(defvar *status-label* nil "Label that reflects the current status.")

;; TODO: update this text
(defvar *help-text*
  "Execute \"nordlocations\" to display a window to retrieve the list of NordVPN servers.
When you select a server name, on the side you will see its location info.")

(defun init ()
  "Start the UI, show the help text if needed"
  (let ((arguments (uiop:command-line-arguments)))
    (when (string= "-h" (first arguments))
      (show-help-and-exit)))
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
  (with-nodgui (:title "NordVPN Client")
    (font-configure "TkDefaultFont" :size 12)
    (font-configure "TkTextFont" :size 12)
    (let* ((cities-label (make-instance 'label
                                           :text "Countries & cities:"))
           (cities-list (make-instance 'nodgui.mw:searchable-listbox
                                       :fill :both
                                       :expand t
                                       :matching-fn #'searchable-listbox-match-ignore-case
                                       :remove-non-matching-p t))
           (get-recommended-local-button (make-instance 'button
                                                        :text "Detect best local server"))
                                                        ;;:command #'get-recommended-local-start))
           (recommended-title-label (make-instance 'label
                                                   :width 30
                                                   :text "Recommended server:"))
           (recommended-info-label (make-instance 'label
                                                  :text ""))
           (connect-to-server-button (make-instance 'button
                                                    :state :disabled
                                                    :text "Connect to server"))
                                                    ;;:command #'get-recommended-local-start))
           (status-frame (make-instance 'labelframe
                                        :text "Status:"))
           (status-label (make-instance 'label
                                        :master status-frame
                                        :text "Press [space] or click the button to get the server list.")))
      (setf *cities-listbox* cities-list)
      (setf *recommended-label* recommended-info-label)
      (setf *status-label* status-label)
      ;; make the listbox in the seachable-listbox wider and taller than the default
      (configure (listbox *cities-listbox*) :height  20)
      (configure (listbox *cities-listbox*) :width  30)
      ;; start by setting focus on the button to get the local server, so Enter triggers the action
      (focus get-recommended-local-button)
      ;; ;; After fetching the servers, the focus is moved to the searchable listbox entry
      ;; ;; make it so that pressing Enter in the entry moves focus to the list
      ;; (bind (entry server-list) "<Return>" #'server-list-entry-enter-key)
      ;; when the listbox selection changes, we need to update the recommended server information
      (bind (listbox cities-list) "<<ListboxSelect>>" #'cities-list-selected-start)

      (grid cities-label 0 0 :padx 10 :pady 10 :sticky "w")
      (grid cities-list 1 0 :padx 10 :pady 10 :sticky "w" :columnspan 2)

      (grid recommended-title-label 0 2 :padx 10 :pady 10 :sticky "w")
      (grid recommended-info-label 1 2 :padx 10 :pady 10 :sticky "w")

      (grid get-recommended-local-button 2 0 :padx 10 :pady 10 :sticky "we")
      (grid connect-to-server-button 2 2 :padx 10 :pady 10 :sticky "we")

      (grid status-frame 3 0 :padx 10 :pady 10 :sticky "nswe" :columnspan 3)
      (grid status-label 0 0 :padx 10 :pady 10 :sticky "w")

      (grid-columnconfigure *tk* :all :weight 1)
      (grid-rowconfigure    *tk* :all :weight 1)
      (populate-cities-listbox)
      )))

(defun populate-cities-listbox ()
  "Shows the list of countries. This function is called on startup."
  (flet ((format-countries-cities ()
           (loop for country in *countries-cities*
                 for name = (alexandria:assoc-value country :name)
                 nconc (loop for city in (alexandria:assoc-value country :cities)
                             collect (format nil "~a  -  ~a" name city)))))
    (setf (text *status-label*) "Retrieving countries and cities...")
    (setf *countries-cities* (nordapi:get-countries-cities))
    (listbox-append *cities-listbox* (format-countries-cities))
    (setf (text *status-label*) "")))

(defun get-id-and-city-from-selected-text ()
  "Extract from `*selected-country-city*' the country id and city name."
  (let* ((text *selected-country-city*)
         ;; this logic is fickle, I tried to account for country names with "-" in ther name
         ;; by using two spaces before/after the - char
         (country-name (subseq text 0 (search "  -  " text)))
         (city-name (subseq text (+ 5 (search "  -  " text)))))
    (values
     ;; Unless something is wrong, `find-if' will always return an item, so:
     (alexandria:assoc-value
      (find-if (lambda (item) (string= (alexandria:assoc-value item :name) country-name))
               *countries-cities*)
      :name)
     city-name)))

(defun cities-list-selected-start (evt)
  "Setup the UI and then call `cities-list-selected-end'."
  (declare (ignore evt))
  (let ((selected-text (first (listbox-get-selection-value *cities-listbox*))))
    (setf *selected-country-city* selected-text
          (text *recommended-label*) ""
          (text *status-label*) (format nil
                                        "Retrieving best server for \"~a\"..."
                                        selected-text))
    ;; Delay a bit the next step so the status label updates
    (nodgui:after 50 #'cities-list-selected-end)))

(defun cities-list-selected-end ()
  "Use the information in `*selected-country-city*' to get the recommended server."
  (multiple-value-bind (country-id city-name) (get-id-and-city-from-selected-text)))

  ;;      (location-info (gethash "location" *selected-server*))
  ;;        (geo-data (nordapi:get-coordinates-location (gethash "long" location-info)
  ;;                                                    (gethash "lat" location-info))))
  ;;   (setf (text *recommnded-label*) (format nil *recommnded-info-template*
  ;;                                         (value-or-dash "Region" geo-data)
  ;;                                         (value-or-dash "Subregion" geo-data)
  ;;                                         (value-or-dash "MetroArea" geo-data)
  ;;                                         (value-or-dash "City" geo-data))))
  ;; (setf (text *status-label*) "-"))



(defun server-list-entry-enter-key (evt)
  "Event handle for pressing Enter focused on the search box.
Selects the first element in the listbox and changes focus to it."
  (declare (ignore evt))
  (let ((the-listbox (listbox *servers-listbox*)))
    (listbox-select the-listbox 0)
    (focus the-listbox)
    (server-list-selected-start nil)))

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
