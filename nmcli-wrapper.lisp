;;;; nmcli-wrapper.lisp
;;; Access nmcli to do the stuff we need: list/create/edit connections

(in-package #:nmcli-wrapper)

;; This should be "nmcli", except when developing inside a toolbx container, then
;; it is "flatpak-spawn --host nmcli"
(defvar *nmcli-executable* "flatpak-spawn --host nmcli" "How to invoke nmcli.")

(defvar *vpn-name-prefix* "nordvpn-client-"
  "Prefix of the connection names managed by this application.")

(defun delete-connections-by-name (name)
  "Delete any other connections named \"`*vpn-connection-prefix*' + NAME\"."
  (let ((nmcli-output (uiop:run-program (format nil "~a ~a"
                                                *nmcli-executable*
                                                "-f name,uuid conn")
                                        :output :string))
        (target-name (concatenate 'string *vpn-connection-name* name)))
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

(defun setup-connection (connection-name config-file)
  "Use nmcli to create CONNECTION-NAME, by importing CONFIG-FILE and setting the user and pass."
  (delete-connections-by-name connection-name)
  ;; (import-connection )
  ;; ;; nmcli connection import --temporary type openvpn file "/tmp/nordvpn-openvpn-udp.ovpn"
  ;; ;; $ nmcli con mod nordvpn-openvpn-udp vpn.secrets "password=$(secret-tool lookup nordvpn-client password)"
  ;; ;; $ nmcli con mod nordvpn-openvpn-udp +vpn.data "username=$(secret-tool lookup nordvpn-client username)"
  ;; ;; $ nmcli con up nordvpn-openvpn-udp

  )

(defun edit-connection ()
  )
