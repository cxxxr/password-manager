(defpackage :password-manager.random-password
  (:add-use-defaults t)
  (:export
   :*default-length*
   :*sign-characters*
   :random-password))
(in-package :password-manager.random-password)

(defparameter *default-length* 15)

(defparameter *sign-characters*
  '(#\# #\$ #\% #\& #\( #\) #\* #\+ #\, #\- #\. #\/ #\: #\; #\< #\= #\> #\? #\@
        #\[ #\] #\^ #\_ #\{ #\| #\} #\~ #\!))

(defun choices-characters (small-alpha large-alpha digit others)
  (let ((characters (make-array 0 :adjustable t :fill-pointer 0)))
    (when small-alpha
      (loop :for code :from (char-code #\a) :to (char-code #\z)
            :do (vector-push-extend (code-char code) characters)))
    (when large-alpha
      (loop :for code :from (char-code #\A) :to (char-code #\Z)
            :do (vector-push-extend (code-char code) characters)))
    (when digit
      (dotimes (i 10)
        (vector-push-extend (digit-char i) characters)))
    (dolist (char others)
      (vector-push-extend char characters))
    characters))

(defun random-elt (seq)
  (elt seq (random (length seq))))

(defun random-password (&key (length *default-length*)
                             (use-small-alpha t)
                             (use-large-alpha t)
                             (use-digit t)
                             (use-sign nil))
  (let ((*random-state* (make-random-state t)))
    (with-output-to-string (out)
      (loop :with characters := (choices-characters use-small-alpha
                                                    use-large-alpha
                                                    use-digit
                                                    (if use-sign *sign-characters*))
            :repeat length
            :do (write-char (random-elt characters) out)))))
