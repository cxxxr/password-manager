(defpackage :password-manager
  (:add-use-defaults t)
  (:use
   :password-manager.account
   :password-manager.random-password)
  (:export :password-manager))
(in-package :password-manager)

(defvar *password-manager*)

(capi:define-interface password-manager ()
  ()
  (:panes
   (new-button
    capi:push-button
    :text "作成"
    :callback-type :none
    :callback 'new-account-window)
   (account-list-panel
    capi:multi-column-list-panel
    :reader account-list-panel
    :columns '((:title "サイト名" :width 100)
               (:title "ユーザー名" :width 100)
               (:title "作成日" :width 120)
               (:title "コメント" :width 300))
    :items '()
    :action-callback (lambda (data interface)
                       (edit-account interface (get-account-from-selection data)))
    :pane-menu 'account-menu
    :interaction :single-selection
    :right-click-selection-behavior :no-change))
  (:layouts
   (column-layout
    capi:column-layout
    '(account-list-panel new-button)))
  (:default-initargs
   :best-height 400
   :best-width 622
   :layout 'column-layout
   :title ""))

(defgeneric slot-to-item-string (slot value)
  (:method ((slot (eql 'created-date)) value)
   (multiple-value-bind (second minute hour day month year)
       (decode-universal-time value)
     (format nil "~D/~D/~D ~D:~D:~D" year month day hour minute second)))
  (:method (slot value)
   (princ-to-string value)))

(defun make-column-item (account)
  (mapcar (lambda (slot-name)
            (slot-to-item-string slot-name (slot-value account slot-name)))
          '(sitename username created-date comment)))

(defun make-column-items (accounts)
  (mapcar #'make-column-item accounts))

(defun update-account-list ()
  (setf (capi:collection-items (account-list-panel *password-manager*))
        (make-column-items (all-accounts))))

(defun get-account-from-selection (data &optional (offset 0))
  (unless (account-empty-p)
    (let* ((sitename (first data))
           (username (second data))
           (matcher (make-account-matcher sitename username))
           (n (or (find-nth-account matcher)
                  (account-count))))
      (nth-account (- n offset)))))

(defstruct menu-item
  name
  action)

(defun edit-account (pane account)
  (declare (ignore pane))
  (overwrite-account-window account))

(defun copy-password (pane account)
  (capi:set-clipboard pane (password account)))

(defun delete-account* (pane account)
  (declare (ignore pane))
  (delete-account account)
  (update-account-list))

(defun account-menu (pane data x y)
  (declare (ignore x y))
  (let ((account (get-account-from-selection data 1)))
    (when account
      (make-instance 'capi:menu
                     :items (list (make-menu-item :name "編集" :action 'edit-account)
                                  (make-menu-item :name "パスワードをコピー" :action 'copy-password)
                                  (make-menu-item :name "削除" :action 'delete-account*))
                     :print-function 'menu-item-name
                     :callback-type :data
                     :callback (lambda (data)
                                 (funcall (menu-item-action data) pane account))))))

(defparameter *enable-character-items* '(:small-alpha :large-alpha :digit :sign))
(defparameter *default-enable-character-items* '(:small-alpha :large-alpha :digit))
(defparameter *enable-character-titles* '("小文字" "大文字" "数字" "記号"))

(capi:define-interface input-account-window ()
  ()
  (:panes
   (sitename-pane
    capi:text-input-pane
    :title "サイト名"
    :title-args '(:external-min-width 70)
    :reader sitename-pane)
   (username-pane
    capi:text-input-pane
    :title "ユーザー名"
    :title-args '(:external-min-width 70)
    :reader username-pane)
   (comment-pane
    capi:text-input-pane
    :title "コメント"
    :title-args '(:external-min-width 70)
    :reader comment-pane)
   (enable-characters-panel
    capi:check-button-panel
    :items *enable-character-items*
    :selected-items *default-enable-character-items*
    :print-function (lambda (item)
                      (elt *enable-character-titles*
                           (position item *enable-character-items*)))
    :reader enable-characters-panel
    :max-height t
    :max-width t)
   (sign-characters
    capi:text-input-pane
    :text (coerce *sign-characters* 'string)
    :title "記号"
    :reader sign-characters)
   (password-length
    capi:text-input-range
    :title "パスワードの長さ"
    :title-position :left
    :start 1 :end 200 :value *default-length*
    :reader password-length)
   (password-input-pane
    capi:text-input-pane
    :title "パスワード"
    :reader password-input-pane)
   (generate-button
    capi:push-button
    :text "生成"
    :callback-type :interface
    :callback 'generate-password)
   (create-account-button
    capi:push-button
    :text "決定"
    :callback-type :interface
    :callback 'create-account))
  (:layouts
   (layout
    capi:column-layout
    '(sitename-pane username-pane comment-pane enable-characters-layout
                    password-input-layout create-account-button))
   (enable-characters-layout
    capi:column-layout
    '(enable-characters-panel sign-characters password-length)
    :title "パスワード生成オプション"
    :title-position :frame)
   (password-input-layout
    capi:row-layout
    '(password-input-pane generate-button)))
  (:default-initargs
   :best-height nil
   :best-width 300
   :internal-border 5
   :layout 'layout
   :title ""))

(defclass new-account-window (input-account-window)
  ())

(defun new-account-window ()
  (capi:display (make-instance 'new-account-window)))

(defclass overwrite-account-window (input-account-window)
  ((old-account :initarg :old-account :reader old-account)))

(defun overwrite-account-window (account)
  (let ((window (make-instance 'overwrite-account-window :old-account account)))
    (setf (capi:text-input-pane-text (sitename-pane window)) (sitename account))
    (setf (capi:text-input-pane-text (username-pane window)) (username account))
    (setf (capi:text-input-pane-text (comment-pane window)) (comment account))
    (setf (capi:text-input-pane-text (password-input-pane window)) (password account))
    (capi:display window)))

(defgeneric confirm-account (self account))

(defmethod confirm-account ((self new-account-window) account)
  (let ((old-account (find-account (make-account-matcher (sitename account)
                                                         (username account)))))
    (cond ((null old-account)
           (add-account account)
           t)
          ((capi:confirm-yes-or-no "そのアカウントはすでに作られています、上書きしますか？")
           (set-account old-account account)
           t)
          (t
           nil))))

(defmethod confirm-account ((self overwrite-account-window) account)
  (set-account (old-account self) account)
  t)

(defun generate-password (input-account-window)
  (let ((items (capi:choice-selected-items (enable-characters-panel input-account-window)))
        (chars (capi:text-input-pane-text (sign-characters input-account-window)))
        (length (capi:text-input-range-value (password-length input-account-window))))
    (let ((*sign-characters* (delete nil (map 'list (lambda (c) (if (char= c #\space) nil c)) chars))))
      (let ((password (random-password :length length
                                       :use-small-alpha (find :small-alpha items)
                                       :use-large-alpha (find :large-alpha items)
                                       :use-digit (find :digit items)
                                       :use-sign (find :sign items)))
            (input-pane (password-input-pane input-account-window)))
        (setf (capi:text-input-pane-text input-pane) password)))))

(defun create-account (input-account-window)
  (flet ((trim-space (string) (string-trim " " string)))
    (let ((sitename (trim-space (capi:text-input-pane-text (sitename-pane input-account-window))))
          (username (trim-space (capi:text-input-pane-text (username-pane input-account-window))))
          (comment (trim-space (capi:text-input-pane-text (comment-pane input-account-window))))
          (password (trim-space (capi:text-input-pane-text (password-input-pane input-account-window)))))
      (cond ((zerop (length sitename))
             (capi:display-message "サイト名が入力されていません"))
            ((zerop (length username))
             (capi:display-message "ユーザー名が入力されていません"))
            ((zerop (length password))
             (capi:display-message "パスワードが空です"))
            (t
             (when (confirm-account input-account-window
                                    (make-instance 'account
                                                   :sitename sitename
                                                   :username username
                                                   :comment comment
                                                   :password password
                                                   :created-date (get-universal-time)))
               (capi:apply-in-pane-process *password-manager*
                                           'update-account-list)
               (capi:quit-interface input-account-window)))))))

(defun prompt-for-password (message)
  (let* ((text-input-pane (make-instance 'capi:password-pane
                                         :callback-type :data
                                         :callback (lambda (password)
                                                     (return-from prompt-for-password password)))))
    (and (capi:popup-confirmer (make-instance 'capi:column-layout
                                              :description (list text-input-pane))
                               message)
         (capi:text-input-pane-text text-input-pane))))

(defun input-password ()
  (tagbody
   :start
   (let ((password (prompt-for-password "マスターパスワード")))
     (unless password (return-from input-password))
     (handler-case (load-accounts password)
       (wrong-password ()
         (capi:display-message "パスワードが違います")
         (go :start)))))
  t)

(defun password-manager ()
  (when (input-password)
    (setf *password-manager* (make-instance 'password-manager))
    (update-account-list)
    (capi:display *password-manager*)))
