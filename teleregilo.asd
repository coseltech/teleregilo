;;;; teleregilo.asd

(asdf:defsystem #:teleregilo
  :description "LIRC frontend"
  :author "CoselTech"
  :license "GPL"
  :depends-on (#:cl-async
               #:bordeaux-threads
               #:vom
               #:carrier
               #:cl-one-time-passwords)
  :serial t
  :components ((:file "package")
               (:file "teleregilo")
               (:file "speak")
               (:file "automation")))

