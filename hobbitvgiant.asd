;;;; hobbitvgiant.asd

(asdf:defsystem #:hobbitvgiant
  :serial t
  :description "A web-based textish gamey thing"
  :author "Daniel Higginbotham <daniel@flyingmachinestudios.com>"
  :license "MIT"
  :depends-on (#:hunchentoot #:iterate #:alexandria #:talcl #:buildnode #:cl-json #:clws)
  :components ((:file "package")
               (:file "hobbitvgiant")
               (:file "utilities")
               (:file "base-classes")
               (:file "observer")
               (:file "player")
               (:file "damage")
               (:file "body-part-prototype")
               (:file "body-layer")
               (:file "body-part")
               (:file "body")
               (:file "body-creation")
               (:file "body-data")
               (:file "describe-body")
               (:file "items")
               (:file "game-room")
               (:file "combat")
               (:file "engagement")
               (:file "web-server")
               (:file "clws-server-interface")
               (:file "clws-server")
               (:file "tests")))
