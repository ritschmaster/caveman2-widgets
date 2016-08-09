;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; This file is a part of the caveman2-widgets project.
;;
;; Copyright (c) 2016 Richard Paul Bäck (richard.baeck@free-your-pc.com)
;; LICENSE: LLGPLv3
;;
;; Purpose:
;; --------
;; This package provides an accessor to a session flag which decides
;; if a requester's session is logged in or not. Further it implements
;; a full login procedure.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package :cl-user)
(defpackage caveman2-widgets.login
  (:use :cl
        :caveman2
        :caveman2-widgets.util
        :caveman2-widgets.widget
        :caveman2-widgets.widgets
        :caveman2-widgets.callback-widget)
  (:export
   :*login-authentication-keyword*
   :logged-in

   :<login-widget>
   :login-authenticator
   :signout-hook
   :login-form
   :logout-button))
(in-package :caveman2-widgets.login)

(defvar *login-authentication-keyword*
  :login
  "This variable holds the keyword which is used within the session to
indicated that a session holder is logged in (or not).")

(defgeneric logged-in (session))
(defmethod logged-in ((session hash-table))
  (find *login-authentication-keyword*
        (gethash *protection-circles-session-key* *session*)))

(defgeneric (setf logged-in) (value session))

(defmethod (setf logged-in) (value (session hash-table))
  (if value
      (add-authorization *login-authentication-keyword* *session*)
      (remove-authorization *login-authentication-keyword* *session*)))

(defclass <login-widget> (<composite-widget>)
  ((login-authenticator
    :initarg :authenticator
    :reader login-authenticator
    :initform #'(lambda (user password) nil)
    :documentation "Must be a function that takes two parameters. The
first is the username and the second is the password.")
   (signout-hook
    :initform nil
    :initarg :signout-hook
    :accessor signout-hook
    :documentation "A functions which will be called after signing out.")
   (login-failed
    :initform nil
    :accessor login-failed
    :documentation "For internal use only. This slot is used to
indicate that the login procedure did not work.")
   (login-form
    :initform nil
    :reader login-form)
   (logout-button
    :initform nil
    :reader logout-button)))

(defmethod initialize-instance :after ((this <login-widget>) &key)
  (setf (slot-value this 'logout-button)
        (make-widget
         :global '<button-widget>
         :label "Sign out"
         :callback
         #'(lambda (args)
             (setf (logged-in *session*)
                   nil)
             (when (signout-hook this)
               (funcall (signout-hook this)))
             (mark-dirty this)))))

(defmethod render-widget ((this <login-widget>))
  (when (null (login-form this))
    (setf (slot-value this 'login-form)
          (let ((text-field (make-instance '<input-field>
                                           :label "Username"
                                           :input-type "text"
                                           :name "username"
                                           :value ""))
                (password-field (make-instance '<input-field>
                                               :label "Password"
                                               :input-type "password"
                                               :name "password"
                                               :value "")))
            (make-widget :global '<form-widget>
                         :input-fields (list
                                        text-field
                                        password-field)
                         :label "Sign in"
                         :callback
                         #'(lambda (args)
                             (if (funcall (login-authenticator this)
                                          (cdr
                                           (assoc 'username args
                                                  :test #'string-case-insensitive=))
                                          (cdr
                                           (assoc 'password args
                                                  :test #'string-case-insensitive=)))
                                 (setf (logged-in *session*)
                                       t)
                                 (setf (login-failed this)
                                       t))
                             (mark-dirty this))))))
  (with-output-to-string (ret-val)
    (if (logged-in *session*)
        (progn
          (format ret-val (call-next-method this))
          (format ret-val (render-widget
                           (logout-button this))))
        (progn
          (format
           ret-val
           (render-widget
            (login-form this)))
          (format
           ret-val "<div class=\"failed-login\">~a</div>"
           (if (login-failed this)
               (progn
                 (setf (login-failed this) nil)
                 (funcall +translate+ "Your sign in attempt has failed!"))
               ""))))))
