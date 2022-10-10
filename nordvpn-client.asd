;;;; nordvpn-client.asd

;; See https://lispcookbook.github.io/cl-cookbook/scripting.html#building-a-smaller-binary-with-sbcls-core-compression
#+sb-core-compression
(defmethod asdf:perform ((o asdf:image-op) (c asdf:system))
  (uiop:dump-image (asdf:output-file o c) :executable t :compression t))

(asdf:defsystem #:nordvpn-client
  :description "GUI tool to manage NordVPN connections using nmcli"
  :author "Sebastián Monía <smonia@outlook.com>"
  :license  "MIT"
  :version "0.0.2"
  :serial t
  :build-operation "program-op"
  :build-pathname "nordvpn-client"
  :entry-point "nordvpn-client-ui:init"
  :depends-on (#:alexandria
               #:uiop
               #:nodgui
               #:drakma
               #:shasht)
  :components ((:file "packages")
               (:file "nordvpn-api")
               (:file "nmcli-wrapper")
               (:file "nordvpn-client-ui")))
