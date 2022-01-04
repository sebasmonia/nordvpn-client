;;;; nmcli-wrapper.lisp
;;; Access nmcli to do the stuff we need: list/create/edit connections

(in-package #:nmcli-wrapper)

;; This should be "nmcli", except when developing inside a toolbx container, then
;; it is "flatpak-spawn --host nmcli"
(defvar *nmcli-executable* "flatpak-spawn --host nmcli" "How to invoke nmcli.")

(defun connection-name-for-file (path)
  "Determines the Network Manager connection name for the file at PATH."
  ;; I used to have a prefix & concat here...not anymore...but leaving the funcion anyway.
  (pathname-name path))

(defun delete-connections-by-name (target-name)
  "Delete any other connections using TARGET-NAME."
  (let ((nmcli-output (uiop:run-program (format nil "~a ~a"
                                                *nmcli-executable*
                                                "-f name,uuid conn")
                                        :output :string)))
    (loop for output-line in (uiop:split-string nmcli-output :separator '(#\Newline))
          for line-pieces = (uiop:split-string (string-trim " " output-line))
          for conn-name = (first line-pieces)
          for conn-uuid = (alexandria:lastcar line-pieces)
          when (string-equal conn-name target-name)
            do (delete-connection-by-uuid conn-uuid))))

(defun delete-connection-by-uuid (connection-uuid)
  "Delete the connection with CONNECTION-UUID, return the operation's exit code."
  (multiple-value-bind (stdout stderr exit-code)
      (uiop:run-program (format nil "~a ~a ~a" *nmcli-executable* "conn delete" connection-uuid)
                        :output :string)
    (declare (ignore stdout stderr))
    exit-code))

(defun create-connection (config-file)
  "Use nmcli to import CONFIG-FILE, and sets its credentials, using secret-tool.
Return two values: Message to display (if any) and new connection name."
  (let ((nm-conn-name (connection-name-for-file config-file)))
    (delete-connections-by-name nm-conn-name)
    (handler-case
        (import-connection config-file)
      (t (condition) ;; on any error, return the message
        (return-from create-connection
          (values (format nil "ERROR - importing connection: ~a~%" condition)
                  nm-conn-name))))
    (handler-case
        (set-username nm-conn-name)
      (t (condition) ;; on any error, return the message
        (return-from create-connection
          (values (format nil "ERROR - setting username: ~a~%" condition)
                  nm-conn-name))))
    (handler-case
        (set-password nm-conn-name)
      (t (condition) ;; on any error, return the message
        (return-from create-connection
          (values (format nil "ERROR - setting password: ~a~%" condition)
                  nm-conn-name))))
    (values nil nm-conn-name)))

(defun import-connection (config-file)
  "Import CONFIG-FILE into Network Manager."
  (multiple-value-bind (stdout stderr exit-code)
      ;; would it be better to use a list of strings instead?
      (uiop:run-program (format nil "~a ~a \"~a\"" *nmcli-executable*
                                "connection import --temporary type openvpn file"
                                config-file)
                        :output :string)
    (declare (ignore stdout))
    (unless (equal 0 exit-code)
      (error stderr))))

(defun set-username (connection-name)
  "Set the username in CONNECTION-NAME using secret-tool."
  (multiple-value-bind (stdout stderr exit-code)
      ;; would it be better to use a list of strings instead?
      (uiop:run-program (format nil "~a ~a ~a ~a \"~a\"" *nmcli-executable*
                                "con mod"
                                connection-name
                                "+vpn.data"
                                "username=$(secret-tool lookup nordvpn-client username)")
                        :output :string)
    (declare (ignore stdout))
    (unless (equal 0 exit-code)
      (error stderr))))

(defun set-password (connection-name)
  "Set the password in CONNECTION-NAME using secret-tool."
  (multiple-value-bind (stdout stderr exit-code)
      ;; would it be better to use a list of strings instead?
      (uiop:run-program (format nil "~a ~a ~a ~a \"~a\"" *nmcli-executable*
                                "con mod"
                                connection-name
                                "vpn.secrets"
                                "password=$(secret-tool lookup nordvpn-client password)")
                        :output :string)
    (declare (ignore stdout))
    (unless (equal 0 exit-code)
      (error stderr))))

(defun open-connection (connection-name)
  "Open CONNECTION-NAME, return an error message in case of failure."
  (multiple-value-bind (stdout stderr exit-code)
      ;; would it be better to use a list of strings instead?
      (uiop:run-program (format nil "~a ~a ~a"
                                *nmcli-executable*
                                "connection up"
                                connection-name)
                        :output :string)
    (declare (ignore stdout))
    (unless (equal 0 exit-code)
      stderr)))
