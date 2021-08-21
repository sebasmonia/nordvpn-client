;;;; nordlocations.asd

;; See https://lispcookbook.github.io/cl-cookbook/scripting.html#building-a-smaller-binary-with-sbcls-core-compression
#+sb-core-compression
(defmethod asdf:perform ((o asdf:image-op) (c asdf:system))
  (uiop:dump-image (asdf:output-file o c) :executable t :compression t))

(asdf:defsystem #:nordlocations
  :description "GUI tool to lookup NordVPN server locations"
  :author "Sebastián Monía <smonia@outlook.com>"
  :license  "MIT"
  :version "0.0.1"
  :serial t
  :build-operation "program-op"
  :build-pathname "nordlocations"
  :entry-point "nordlocations   :init"
  :depends-on (#:alexandria
               #:uiop
               #:nodgui
               #:dexador
               #:jonathan)
  :components ((:file "package")
               (:file "nordlocations")))
