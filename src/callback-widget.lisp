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

   :<button-widget>
   :make-button
   :*button-call-path*

   :<link-widget>
   :make-link
   :*link-call-path*))
(in-package :caveman2-widgets.callback-widget)

(defclass <callback-widget> (<widget>)
  ((label
    :initform nil
    :reader label)
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
keyword (e.g. :post or :get")))

(defun test-widget-if-session (scope widget-id &optional (session *session*))
  (declare (keyword scope)
           (string widget-id))
  (when (eql scope :session)
    (let* ((session-widget-holder
            (gethash :widget-holder session))
           (sessioned-widget (if session-widget-holder
                                 (find-widget session-widget-holder
                                              widget-id)
                                 nil)))
      (when (null sessioned-widget)
        (throw-code 404)))))

;; (defmethod initialize-instance :around ((this <callback-widget>) &key)
;;   (store-callback-for-widget (callback this)
;;                              (uri-path this)))

(defun init-callback-widget (widget label callback)
  "
@param callback The callback function for the route. This function
will be called when the route is accessed.

@param uri-path The path where to access the callback function."
  (declare (<callback-widget> widget)
           (string label)
           (function callback))
  (setf (slot-value widget 'label) label)
  (setf (ningle:route *web*
                      (uri-path widget)
                      :method (http-method widget))
        callback))

(defvar *button-call-path* "buttons")
(defvar *input-field-for-old-uri* "oldUri")

(defclass <button-widget> (<callback-widget>)
  ()
  (:default-initargs
   :uri-path ""
    :http-method :post)
  (:documentation ""))

(defmethod initialize-instance :after ((this <button-widget>) &key)
  (setf (slot-value this 'uri-path)
        (concatenate 'string
                     "/"
                     *button-call-path*
                     "/"
                     (id this))))

(defmethod render-widget ((this <button-widget>))
  (concatenate 'string
               "<form method=\"post\" action=\"" (uri-path this) "\">"
               "<input type=\"submit\" value=\"" (label this) "\"/>"
               "<input type=\"hidden\" name=\"" *input-field-for-old-uri* "\" value=\"" (getf (request-env *request*) :request-uri) "\" /></form>"))

(defun make-button (scope label callback)
  "@param callback The callback function for the button. This function
   will be called when the user presses the button."
  (declare (keyword scope)
           (string label)
           (function callback))
  (let ((ret-val (make-widget scope '<button-widget>)))
    (init-callback-widget ret-val
                          label
                          #'(lambda (params)
                              (test-widget-if-session scope (id ret-val))
                              (funcall callback)
                              (let ((oldUrl (get-value-for-ningle-request-parameter
                                             params
                                             *input-field-for-old-uri*)))
                                (when oldUrl
                                  (redirect oldUrl)))))
    ret-val))

(defvar *link-call-path* "links")

(defclass <link-widget> (<callback-widget>)
  ()
  (:default-initargs
   :uri-path ""
    :http-method :get)
  (:documentation ""))

(defmethod initialize-instance :after ((this <link-widget>) &key)
  (setf (slot-value this 'uri-path)
        (concatenate 'string
                     "/"
                     *link-call-path*
                     "/"
                     (id this))))

(defmethod render-widget ((this <link-widget>))
  (concatenate 'string
               "<a href=\"" (uri-path this) "\">"
               (label this)
               "</a>"))

(defun make-link (scope label callback)
  "@param callback The callback function for the button. This function
   will be called when the user clickes the link. The function must
   return a string. The returned string should be an URL to which the
   server should redirect."
  (declare (keyword scope)
           (string label)
           (function callback))
  (let ((ret-val (make-widget scope '<link-widget>)))
    (init-callback-widget ret-val
                          label
                          #'(lambda (params)
                              (test-widget-if-session scope (id ret-val))
                              (redirect (funcall callback))))
    ret-val))

