;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; This file is a part of caveman2-widgets project.
;;
;; Copyright (c) 2016 Richard Paul Bäck (richard.baeck@free-your-pc.com)
;; LICENSE: LLGPLv3
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package :cl-user)
(defpackage caveman2-widgets.widget
  (:use :cl
        :caveman2
        :caveman2-widgets.util)
  (:export :<widget>
           :render-widget
           :render-widget-rest
           :init-widgets
           :make-widget
           :set-widget-for-session
           :get-widget-for-session
           :remove-widget-for-session
           :id
           :*rest-path*
           :*web*))
(in-package :caveman2-widgets.widget)

(defvar *rest-path* "rest")
(defvar *rest-methods* '(:get :post :put :patch :delete :head :options))
(defvar *web* nil
  "An <app>-instance")

(defclass <widget-holder> ()
  ((widgets
    :initform '()
    :documentation "

Holds all widgets and derived widgets of a specific session. If a
widget is finalized it will be removed from this list
automatically. This list is neccessary for the REST API to get exactly
the given widget.")))

(defgeneric append-widget (this widget))

(defgeneric remove-widget (this widget))

(defgeneric find-widget (this to-find))

(defun init-widgets (webapp)
  (declare (<app> webapp))
  (setf *web* webapp))

(defclass <widget> ()
  ((id
    :initform (symbol-name (gensym))
    :reader id)

   (api-generated-p
    :initform (make-hash-table)
    :accessor api-generated-p
    :allocation :class
    :documentation "
To know if the REST API has been generated yet. This is a hash-table which
stores a boolean for each derived class. E.g. to check if the API for <widget>
has been created you can call (gethash '<widget> (api-generated-p this))"))
  (:documentation ""))

(defmethod append-widget ((this <widget-holder>) (widget <widget>))
  (setf
   (slot-value this 'widgets)
   (append (slot-value this 'widgets)
           (list
            (trivial-garbage:make-weak-pointer widget)))))

(defmethod remove-widget ((this <widget-holder>) (widget <widget>))
  (setf (slot-value this 'widgets)
        (remove-if #'(lambda (item)
                       (string= (slot-value (trivial-garbage:weak-pointer-value item)
                                            'id)
                                (slot-value widget 'id)))
                   (slot-value this 'widgets))))



(defmethod find-widget ((this <widget-holder>) (to-find string))
  (trivial-garbage:gc :full t)
  (setf (slot-value this 'widgets)
        (clean-list-of-broken-links (slot-value this 'widgets)))
  (let ((ret-val (find-if  #'(lambda (item)
                               (string= (id (trivial-garbage:weak-pointer-value item))
                                        to-find))
                           (slot-value this 'widgets))))
    (if ret-val
        (trivial-garbage:weak-pointer-value ret-val)
        nil)))

(defmethod initialize-instance :after ((this <widget>) &key)
  "
Generates a REST for the widget. It will automatically generate
accessable URIs for the HTTP methods stored in *rest-methods*.
The REST can be accessed by the URI /*rest-path*/widget-name"
  (declare (special *web*))

  (when (not (gethash (type-of this) (api-generated-p this)))
    (let ((rest-path
           (string-downcase
            (concatenate 'string
                         "/"
                         *rest-path*
                         "/"
                         (get-trimmed-class-name this)))))

      (dolist (cur-method *rest-methods*)
        (setf (ningle:route *web*
                            rest-path
                            :method cur-method)
              #'(lambda (params)
                  (let* ((session-widget-holder
                          (gethash :widget-holder *session*))
                         (requested-id (get-value-for-ningle-request-parameter
                                        params
                                        "id"))
                         (found-widget
                          (or
                           (find-widget *global-widget-holder*
                                        requested-id)
                           (if session-widget-holder
                               (find-widget session-widget-holder
                                            requested-id)
                               nil))))
                    (if found-widget
                        (render-widget-rest
                         found-widget
                         cur-method
                         params)
                        (throw-code 404)))))))
    (setf (gethash (type-of this) (api-generated-p this)) t)))

(defgeneric render-widget (this)
  (:documentation "@return Returns the HTML representation of the
widget as string. It is intended to use this within a simple HTML
transfer or embedded in another page."))

(defmethod render-widget :around ((this <widget>))
  (concatenate 'string
               "<div class=\"widget "
               (get-trimmed-class-name this)
               "\">"
               (call-next-method this)
               "</div>"))

(defgeneric render-widget-rest (this method args)
  (:documentation "
@return Returns the HTML representation of the
widget as string. To generate a method for a specific HTTP method you
can do the following:

(defmethod render-widget-rest ((this <widget>) (method (eql :get)))
  \"HTML output for the REST when GET.\")"))

(defvar *global-widget-holder*
  (make-instance '<widget-holder>))

(defgeneric make-widget (scope class)
  (:documentation ""))

(defmethod make-widget ((scope (eql :global)) (class symbol))
  (let ((ret-val (make-instance class)))
    (append-widget *global-widget-holder* ret-val)
    ret-val))

(defmethod make-widget ((scope (eql :session)) (class symbol))
  (let ((holder (gethash :widget-holder *session*))
        (ret-val (make-instance class)))
    (when (null holder)
      (setf holder (make-instance '<widget-holder>))
      (setf (gethash :widget-holder *session*)
            holder))
    (append-widget holder ret-val)
    ret-val))

(defun set-widget-for-session (session-tag widget &optional (session *session*))
  (declare (keyword session-tag)
           (<widget> widget)
           (hash-table session))
  (when (null (gethash session-tag session))
    (setf (gethash session-tag session)
          widget)))

(defun get-widget-for-session (session-tag  &optional (session *session*))
  (declare (keyword session-tag)
           (hash-table session))
  (gethash session-tag session))

(defun remove-widget-for-session (session-tag  &optional (session *session*))
  (declare (keyword session-tag)
           (hash-table session))
  (setf (gethash session-tag session) nil))
