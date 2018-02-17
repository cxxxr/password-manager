(defpackage :password-manager.account
  (:add-use-defaults t)
  (:export
   :wrong-password
   :account
   :sitename
   :username
   :password
   :created-date
   :comment
   :load-accounts
   :save-accounts
   :all-accounts
   :account-count
   :account-empty-p
   :set-account
   :add-account
   :make-account-matcher
   :find-account
   :find-nth-account
   :nth-account
   :delete-account))
(in-package :password-manager.account)

(defparameter *account-pathname*
  (merge-pathnames ".accounts" (user-homedir-pathname)))

(defvar *master-password*)
(defvar *accounts* '())

(define-condition wrong-password (simple-error)
  ())

(defclass account ()
  ((sitename
    :initarg :sitename
    :accessor sitename)
   (username
    :initarg :username
    :accessor username)
   (password
    :initarg :password
    :accessor password)
   (created-date
    :initarg :created-date
    :accessor created-date)
   (comment
    :initarg :comment
    :accessor comment
    :initform "")))

(defun encrypt (raw-file encrypted-file password)
  (uiop:with-temporary-file (:stream password-stream :pathname password-file)
    (write-line password password-stream)
    (force-output password-stream)
    :close-stream
    (uiop:run-program (format nil
                              "openssl aes-256-cbc -e -in '~A' -out '~A' -pass file:'~A'"
                              raw-file encrypted-file password-file)
                      :standard-output t
                      :error-output t)))

(defun decrypt (encrypted-file password raw-file)
  (uiop:with-temporary-file (:stream password-stream :pathname password-file)
    (write-line password password-stream)
    (force-output password-stream)
    :close-stream
    (handler-case (uiop:run-program (format nil "openssl aes-256-cbc -d -in '~A' -out '~A' -pass file:'~A'"
                                            encrypted-file raw-file password-file)
                                    :standard-output t
                                    :error-output t)
      (error () (error 'wrong-password)))))

(defun load-accounts (master-password)
  (setf *master-password* master-password)
  (when (probe-file *account-pathname*)
    (uiop/stream:with-temporary-file (:pathname raw-file)
      :closed-stream
      (decrypt *account-pathname* master-password raw-file)
      (setf *accounts* (cl-store:restore raw-file)))))

(defun save-accounts ()
  (uiop:with-temporary-file (:pathname raw-file)
    :close-stream
    (cl-store:store *accounts* raw-file)
    (encrypt raw-file *account-pathname* *master-password*)))

(defun all-accounts ()
  *accounts*)

(defun account-count ()
  (length *accounts*))

(defun account-empty-p ()
  (null *accounts*))

(defun set-account (old-account new-account)
  (setf (sitename old-account) (sitename new-account)
        (username old-account) (username new-account)
        (password old-account) (password new-account)
        (created-date old-account) (created-date new-account)
        (comment old-account) (comment new-account))
  (save-accounts))

(defun add-account (account)
  (push account *accounts*)
  (save-accounts))

(defun delete-account (account)
  (setf *accounts* (delete account *accounts*))
  (save-accounts))

(defun make-account-matcher (sitename username)
  (lambda (account)
    (and (equal sitename (sitename account))
         (equal username (username account)))))

(defun find-account (matcher)
  (find-if matcher *accounts*))

(defun find-nth-account (matcher)
  (position-if matcher *accounts*))

(defun nth-account (n)
  (elt *accounts* n))
