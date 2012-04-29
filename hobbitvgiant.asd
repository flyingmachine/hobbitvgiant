;;;; hobbitvgiant.asd

(asdf:defsystem #:hobbitvgiant
  :serial t
  :description "A web-based textish gamey thing"
  :author "Daniel Higginbotham <daniel@flyingmachinestudios.com>"
  :license "MIT"
  :depends-on (#:hunchentoot)
  :components ((:file "package")
               (:file "hobbitvgiant")
               (:file "utilities")
               (:file "damage")
               (:file "body-classes")
               (:file "body-data")
               (:file "describe-body")
               (:file "items")
               (:file "combat")
               (:file "engagement")
               (:file "tests")))
