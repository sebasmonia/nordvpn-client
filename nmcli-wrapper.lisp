;;;; nmcli-wrapper.lisp
;;; Access nmcli to do the stuff we need: list/create/edit connections

(in-package #:nmcli-wrapper)

;; This should be "nmcli", except when developing inside a toolbx container, then
;; it is "flatpak-spawn --host nmcli"
(defvar *nmcli-executable* "flatpak-spawn --host nmcli" "How to invoke nmcli.")

(defvar *vpn-connection-name* "nordvpn-openvpn-udp" "Name of the connection to use for all tasks.")

(defun delete-connections-created-by-this-program ()
  "Delete any other connections with the name `*vpn-connection-name*'.
In theory, except when coding...there should be one, or none, of these connections."
  (let ((nmcli-output (uiop:run-program (format nil "~a ~a"
                                                *nmcli-executable*
                                                "-f name,uuid conn")
                                        :output :string)))
    (loop for output-line in (uiop:split-string nmcli-output :separator '(#\Newline))
          for line-pieces = (uiop:split-string (string-trim " " output-line))
          for conn-name = (first line-pieces)
          for conn-uuid = (alexandria:lastcar line-pieces)
          when (string= conn-name *vpn-connection-name*)
            do (delete-connection-by-uuid conn-uuid))))

(defun delete-connection-by-uuid (connection-uuid)
  "Delete the connection with CONNECTION-UUID, return the operation's exit code."
  (multiple-value-bind (stdout stderr exit-code)
      (uiop:run-program (format nil "~a ~a ~a" *nmcli-executable* "conn delete" connection-uuid)
                        :output :string)
    (declare (ignore stdout stderr))
    exit-code))

(defun setup-connection (path)
  "Creates a connection, using the file in PATH."
  ;; If the connection is up, it is delete anyway, so:
  (delete-connections-created-by-this-program)
  ;; nmcli connection import --temporary type openvpn file "/tmp/nordvpn-openvpn-udp.ovpn"
  )

(defun edit-connection ()
  )
