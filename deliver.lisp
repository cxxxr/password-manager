(in-package "CL-USER")

(lw:set-default-character-element-type 'cl:character)

(let ((quicklisp-init (merge-pathnames "quicklisp/setup.lisp" (user-homedir-pathname))))
  (when (probe-file quicklisp-init)
    (load quicklisp-init)))

(load-all-patches)
(uiop:symbol-call :ql :quickload :password-manager)

(deliver (find-symbol "PASSWORD-MANAGER" :password-manager)
         "./password-manager"
         0 
         :interface :capi
         :keep-pretty-printer T)
