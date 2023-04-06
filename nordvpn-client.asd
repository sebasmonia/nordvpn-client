;;;; nordvpn-client.asd

(asdf:defsystem #:nordvpn-client
  :description "GUI tool to manage NordVPN connections using nmcli"
  :author "Sebastián Monía <code@sebasmonia.com>"
  :license  "MIT"
  :version "0.0.3"
  :serial t
  :defsystem-depends-on (:deploy)
  :build-operation "deploy-op"
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
