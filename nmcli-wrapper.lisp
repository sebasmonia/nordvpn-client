;;;; nmcli-wrapper.lisp
;;; Access nmcli to do the stuff we need: list/create/edit connections

(in-package #:nmcli-wrapper)

;; This should be "nmcli", except when developing inside a toolbx container, then
;; it is "flatpak-spawn --host nmcli"
(defvar *nmcli-executable* "flatpak-spawn --host nmcli" "How to invoke nmcli.")

(defvar *vpn-connection-name* "nordvpn-openvpn-udp" "Name of the connection to use for all tasks.")

(defun connection-exists ()
  "Check if a VPN connection `*vpn-connection-name*' exists."
  (let ((nmcli-output (uiop:run-program (format nil "~a ~a"
                                                *nmcli-executable*
                                                "-f name -m multiline conn")
                                        :output :string)))
    (loop for line in (uiop:split-string NMCLI-OUTPUT :separator '(#\Newline))
          when (uiop:string-suffix-p line *vpn-connection-name*)
            do (return t))))

(defun create-connection (path)
  "Creates a connection named `*vpn-connection-name*'."

  )

(defun edit-connection ()
  )
