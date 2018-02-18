(in-package :cl-user)

(let ((quicklisp-init (merge-pathnames "quicklisp/setup.lisp" (user-homedir-pathname))))
  (when (probe-file quicklisp-init)
    (load quicklisp-init)))

(ql:quickload :password-manager)

(defun main ()
  (password-manager:password-manager))

(pushnew '("Password Manager" (:priority 60000000 :restart-action :continue) main)
         mp:*initial-processes*)

(hcl:save-image "password-manager"
                :multiprocessing t
                :environment nil)
