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
   :authenticator
   :logout-button

   :protect-widget))
(in-package :caveman2-widgets.login)

(defvar *login-authentication-keyword*
  :logged-in-flag
  "This variable holds the keyword which is used within the session to
indicated that a session holder is logged in (or not).")

(defgeneric logged-in (session))
(defmethod logged-in ((session hash-table))
  (gethash *login-authentication-keyword* *session*))

(defgeneric (setf logged-in) (value session))

(defmethod (setf logged-in) (value (session hash-table))
  (setf (gethash *login-authentication-keyword* *session*)
        value))

(defclass <login-widget> (<composite-widget>)
  ((authenticator
    :initarg :authenticator
    :reader authenticator
    :initform #'(lambda (user password) nil)
    :documentation "Must be a function that takes two parameters. The
first is the username and the second is the password.")
   (login-failed
    :initform nil
    :accessor login-failed
    :documentation "For internal use only. This slot is used to
indicate that the login procedure did not work.")
   (logout-button
    :initform
    (make-widget
     :session '<button-widget>
     :label "Logout"
     :callback
     #'(lambda ()
         (setf (logged-in *session*)
               nil)))
    :reader logout-button)))

(defmethod render-widget ((this <login-widget>))
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
            (make-widget
             :session '<button-widget>
             :label "Login"
             :callback
             #'(lambda ()
                 (if (funcall (authenticator this)
                              "user"
                              "password")
                     (setf (logged-in *session*)
                           t)
                     (setf (login-failed this)
                           t))
                 (mark-dirty this)))))
          (format
           ret-val "<div class=\"failed-login\">~a</div>"
           (if (login-failed this)
               (progn
                 (setf (login-failed this) nil)
                 (funcall +translate+ "Your login attempt has failed!"))
               ""))))))

(defgeneric protect-widget (widget for)
  (:documentation "@return The WIDGET object."))

(defmethod protect-widget ((widget <widget>) (for (eql :login)))
  (setf (protected widget)
        *login-authentication-keyword*)
  widget)
