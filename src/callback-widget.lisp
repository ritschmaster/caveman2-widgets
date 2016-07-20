;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; This file is a part of the caveman2-widgets project.
;;
;; Copyright (c) 2016 Richard Paul Bäck (richard.baeck@free-your-pc.com)
;; LICENSE: LLGPLv3
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package :cl-user)
(defpackage caveman2-widgets.callback-widget
  (:use :cl
        :caveman2
        :caveman2-widgets.util
        :caveman2-widgets.widget)
  (:export
   :<callback-widget>
   :init-callback-widget
   :label
   :uri-path
   :http-method
   :classes

   :<button-widget>
   :*button-call-path*

   :<link-widget>
   :*link-call-path*))
(in-package :caveman2-widgets.callback-widget)

(defclass <callback-widget> (<widget>)
  ((label
    :initform nil
    :initarg :label
    :reader label)
   (callback
    :initform #'(lambda () "")
    :initarg :callback
    :documentation "")
   (uri-path
    :initform (error "Must supply an uri-path to access the widget.")
    :initarg :uri-path
    :reader uri-path
    :documentation "This slot should give the exact path to access this widget.")
   (http-method
    :initform  (error "Must supply a method to access the HTTP URL.")
    :initarg :http-method
    :reader http-method
    :documentation "This slot should be one of the HTTP methods as
keyword (e.g. :post or :get")
   (classes
    :initform nil
    :initarg :classes
    :accessor classes)))

(defun test-widget-if-session (scope widget-id &optional (session *session*))
  (declare (keyword scope)
           (string widget-id))
  (when (eql scope :session)
    (let* ((session-widget-holder
            (gethash :widget-holder session))
           (sessioned-widget (if session-widget-holder
                                 (find-item session-widget-holder
                                            widget-id)
                                 nil)))
      (when (null sessioned-widget)
        (throw-code 404)))))

(defvar *button-call-path* "buttons")
(defvar *input-field-for-old-uri* "oldUri")

(defclass <button-widget> (<callback-widget>)
  ()
  (:default-initargs
   :uri-path ""
    :http-method :post)
  (:documentation "The callback function will be called when the user
presses the button."))

(defmethod initialize-instance :after ((this <button-widget>) &key)
  (setf (slot-value this 'uri-path)
        (concatenate 'string
                     "/"
                     *button-call-path*
                     "/"
                     (id this)))
  (setf (ningle:route *web*
                      (uri-path this)
                      :method (http-method this))
        #'(lambda (params)
            (test-widget-if-session (widget-scope this)
                                    (id this))
            (funcall (slot-value this 'callback))
            (let ((oldUrl (get-value-for-cons-list
                           params
                           *input-field-for-old-uri*)))
              (when oldUrl
                (redirect oldUrl))))))

(defmethod render-widget ((this <button-widget>))
  (with-output-to-string (ret-val)
    (format ret-val "<form method=\"post\" action=\"~a\">"
            (uri-path this))
    (format ret-val "<button type=\"submit\" class=\"~a\">~a</button>"
            (or (classes this) "")
            (funcall +translate+ (label this)))
    (format ret-val "<input type=\"hidden\" name=\"~a\" value=\"~a\" />
</form>"
            *input-field-for-old-uri*
            (getf (request-env *request*) :request-uri))))

(defvar *link-call-path* "links")

(defclass <link-widget> (<callback-widget>)
  ((target-foreign-p
    :initform nil
    :initarg :target-foreign-p
    :documentation "When the given link redirects absolute (like http://...)."))
  (:default-initargs
   :uri-path ""
    :http-method :get)
  (:documentation "The callback function will be called when the user
clickes the link. The function must return a string. The returned
string should be an URL to which the server should redirect."))

(defmethod initialize-instance :after ((this <link-widget>) &key)
  (setf (slot-value this 'uri-path)
        (concatenate 'string
                     "/"
                     *link-call-path*
                     "/"
                     (id this)))
  (setf (ningle:route *web*
                      (uri-path this)
                      :method (http-method this))
        #'(lambda (params)
            (test-widget-if-session (widget-scope this)
                                    (id this))
            (redirect (concatenate 'string
                                   (if (slot-value this 'target-foreign-p)
                                       ""
                                       "/")
                                   (funcall (slot-value this
                                                        'callback))))))
  (setf (ningle:route *web*
                      (uri-path this)
                      :method :post)
        #'(lambda (params)
            (test-widget-if-session (widget-scope this)
                                    (id this))
            (funcall (slot-value this
                                 'callback)))))

(defmethod render-widget ((this <link-widget>))
  (concatenate 'string
               "<a href=\"" (uri-path this) "\">"
               (funcall +translate+ (label this))
               "</a>")) 
