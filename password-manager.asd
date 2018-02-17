(defsystem "password-manager"
  :depends-on ("cl-store")
  :serial t
  :components ((:file "random-password")
               (:file "account")
               (:file "password-manager")))
