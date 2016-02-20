;;;; teleregilo.asd

(asdf:defsystem #:teleregilo
  :description "LIRC frontend"
  :author "CoselTech"
  :license "GPL"
  :depends-on (#:cl-async
               #:bordeaux-threads
               #:vom)
  :serial t
  :components ((:file "package")
               (:file "teleregilo")
               (:file "automation")))

