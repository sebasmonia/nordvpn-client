;;;; nordlocations.lisp

(in-package #:nordlocations)

(defvar *selected-toolbox* "" "The name of the selected the toolbox container name.")
(defvar *command-text* "emacs" "The command to execute.")
(defvar *available-containers* nil "List of containers available.")

(defvar *help-text* "Usage: toolbox-launcher [container-name] [command -and -arguments]

Call \"toolbox run\" with the parameters specified in the UI.
The optional command line arguments allow pre-filling the values of the window.")

(defun init ()
  "Initialize the application based on the CLI arguments (if any) and start the UI."
  (let ((arguments (uiop:command-line-arguments)))
    (if (string= "-h" (first arguments))
        (show-help-and-exit)
        (progn
          (setf *selected-toolbox* (first arguments))
          (setf *command-text* (format nil "~{~a ~}" (or (rest arguments) '("emacs"))))
          (get-available-toolboxes))))
  (start-ui))

(defun show-help-and-exit ()
  (format t "~a~%" *help-text*)
  (uiop:quit 0))

(defun start-ui ()
  "Launches the UI for toolbox-launcher."
  (with-nodgui (:title "toolbox launcher")
    (font-configure "TkDefaultFont" :size 12)
    (font-configure "TkTextFont" :size 12)
    (let* ((toolbox-label (make-instance 'label
                                         :text "Toolbox:"))
           (toolbox-combo (make-instance 'combobox
                                         ;; hardcoded...
                                         :text *selected-toolbox*
                                         :values *available-containers*))
           (command-label (make-instance 'label
                                         :text "Command:"))
           (command-entry (make-instance 'entry
                                         :text *command-text*))
           (exec-button (make-instance 'button
                                       :text "Execute"
                                       :command (lambda ()
                                                  (setf *selected-toolbox* (text toolbox-combo)
                                                        *command-text* (text command-entry))
                                                  (run-program-and-exit)))))
      ;; focus on the combo to select the container
      (focus toolbox-combo)
      ;; Make exec-button the default action
      (bind *tk* "<Return>" (lambda (evt)
                              (declare (ignore evt))
                              (funcall (command exec-button))))
      (grid toolbox-label 0 0 :padx 10 :pady 10 :sticky "w")
      (grid toolbox-combo 0 1 :padx 10 :pady 10 :sticky "w")
      (grid command-label 1 0 :padx 10 :pady 10 :sticky "w")
      (grid command-entry 1 1 :padx 10 :pady 10 :sticky "w")
      (grid exec-button 2 0 :padx 10 :pady 10 :columnspan 2)
      (grid-columnconfigure *tk* :all :weight 1)
      (grid-rowconfigure    *tk* :all :weight 1))))

(defun get-available-toolboxes ()
  "Call toolbox to list the available containers, and parse its output."
  (let* ((list-output (uiop:run-program '("toolbox" "list" "-c")
                                        :output '(:string :stripped t)))
         (containers (subseq (uiop:split-string list-output :separator '(#\Newline)) 1)))
    (setf *available-containers* (loop for line in containers
                                       collect (third (uiop:split-string line))))
    (unless *selected-toolbox*
      (setf *selected-toolbox* (first *available-containers*)))))

(defun run-program-and-exit ()
  "Run the program according to the parameters in the UI and exit."
  (uiop:launch-program
   (format nil "toolbox run -c ~a ~a" *selected-toolbox* *command-text*))
   ;; got this from reading the source, maybe there is a better way
  (setf *exit-mainloop* t)
  (uiop:quit 0))
