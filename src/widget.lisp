;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; This file is a part of the caveman2-widgets project.
;;
;; Copyright (c) 2016 Richard Paul Bäck (richard.baeck@free-your-pc.com)
;; LICENSE: LLGPLv3
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package :cl-user)
(defpackage caveman2-widgets.widget
  (:use :cl
        :caveman2
        :caveman2-widgets.util)
  (:export
   :<widget>
   :render-widget-header
   :render-widget-body
   :render-widget
   :render-widget-rest
   :make-widget
   :id
   :widget-scope

   :find-item

   :set-widget-for-session
   :get-widget-for-session
   :remove-widget-for-session

   :mark-dirty
   :*dirty-objects-uri-path*
   :init-mark-dirty

   :*rest-path*
   :*javascript-checker-path*
   :*web*
   :*javascript-path*
   :*widgets-js-filename*
   :*css-path*
   :*css-route*))
(in-package :caveman2-widgets.widget)

(defvar *rest-path* "rest")
(defvar *javascript-checker-path* "javascript-checker")
(defvar *rest-methods* '(:get :post :put :patch :delete :head :options))
(defvar *web* nil
  "An <app>-instance")

(defvar *javascript-path*
  "/widgets/js"
  "An absolute path where caveman2-widgets' JavaScript files can be accessed.")
(defvar *css-path*
  "/widgets/css"
  "An absolute route where caveman2-widgets' CSS files can be accessed.")

(defvar *widgets-js-filename* "widgets.js"
  "The filename of the JavaScript file which manages all standard widget
functionality.")


(defclass <widget-holder> ()
  ((widgets
    :initform '()
    :documentation "

Holds all widgets and derived widgets of a specific session. If a
widget is finalized it will be removed from this list
automatically. This list is neccessary for the REST API to get exactly
the given widget.")))

(defclass <widget> ()
  ((id
    :initform (symbol-name (gensym))
    :reader id)
   (scope
    :reader widget-scope))
  (:documentation ""))

(defmethod append-item ((this <widget-holder>) (item <widget>))
  (setf
   (slot-value this 'widgets)
   (append (slot-value this 'widgets)
           (list
            (trivial-garbage:make-weak-pointer item)))))

(defmethod delete-item ((this <widget-holder>) (item <widget>))
  (setf (slot-value this 'widgets)
        (remove-if #'(lambda (item)
                       (string= (slot-value (trivial-garbage:weak-pointer-value item)
                                            'id)
                                (slot-value item 'id)))
                   (slot-value this 'widgets))))



(defmethod find-item ((this <widget-holder>) (to-find string))
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

  (let ((rest-path
         (string-downcase
          (concatenate 'string
                       "/"
                       *rest-path*
                       "/"
                       (get-trimmed-class-name this)))))

    (dolist (cur-method *rest-methods*)
      (when (null (ningle:route *web*
                                rest-path
                                :method cur-method))
        (setf (ningle:route *web*
                            rest-path
                            :method cur-method)
              #'(lambda (params)
                  (let* ((session-widget-holder
                          (gethash :widget-holder *session*))
                         (requested-id (get-value-for-cons-list
                                        params
                                        "id"))
                         (found-widget
                          (or
                           (find-item *global-widget-holder*
                                      requested-id)
                           (if session-widget-holder
                               (find-item session-widget-holder
                                          requested-id)
                               nil))))
                    (if found-widget
                        (render-widget-rest
                         found-widget
                         cur-method
                         params)
                        (throw-code 404)))))))))

(defgeneric render-widget-header (this))
(defgeneric render-widget-body (this))

(defgeneric render-widget (this)
  (:documentation "@return Returns the HTML representation of the
widget as string. It is intended to use this within a simple HTML
transfer or embedded in another page."))

(defmethod render-widget :around ((this <widget>))
  (demark-dirty this)
  (with-output-to-string (ret-val)
    (format ret-val
            "<div id=\"~a\" class=\"~a"
            (id this)
            (get-trimmed-class-name this
                                    :get-all t))
    (format ret-val "\">")
    (format ret-val (call-next-method this))
    (format ret-val "</div>")))

(defgeneric render-widget-rest (this method args)
  (:documentation "
@return Returns the HTML representation of the
widget as string. To generate a method for a specific HTTP method you
can do the following:

(defmethod render-widget-rest ((this <widget>) (method (eql :get)) (args t))
  \"HTML output for the REST when GET.\")"))

(defmethod render-widget-rest ((this <widget>)
                               method
                               args)
  (render-widget this))

(defmethod render-widget-rest :around ((this <widget>)
                                       method
                                       args)
  (demark-dirty this)
  (call-next-method this method args))

(defvar *global-widget-holder*
  (make-instance '<widget-holder>))

(defmacro make-widget (scope class
                       &rest obj-args)
  "@param obj-args The parameter which are passed to the constructor for the
new widget."
  `(cond
     ((eql ,scope :global)
      (let ((ret-val (make-instance ,class
                                    ,@obj-args)))
        (append-item caveman2-widgets.widget::*global-widget-holder* ret-val)
        (setf (slot-value ret-val 'scope) ,scope)
        ret-val))
     ((eql ,scope :session)
      (let ((holder (gethash :widget-holder *session*))
            (ret-val (make-instance ,class
                                    ,@obj-args)))
        (when (null holder)
          (setf holder (make-instance '<widget-holder>))
          (setf (gethash :widget-holder *session*)
                holder))
        (append-item holder ret-val)
        (setf (slot-value ret-val 'scope) ,scope)
        ret-val))
     (t
      (error "unsupported scope"))))

(defun set-widget-for-session (session-tag widget &optional (session *session*))
  "Saves a widget in the session variable. This should be considered ONLY for
session scoped widgets. Only adds WIDGET if there is no widget at SESSION-TAG
already.

@return The current value in the SESSION at position SESSION-TAG."
  (declare (keyword session-tag)
           (<widget> widget)
           (hash-table session))
  (let ((ret-val (gethash session-tag session)))
    (when (null ret-val)
      (setf (gethash session-tag session)
            widget))
    ret-val))

(defun get-widget-for-session (session-tag  &optional (session *session*))
  "Gets a previously saved widget from the session variable (e.g. to render
it)."
  (declare (keyword session-tag)
           (hash-table session))
  (gethash session-tag session))

(defun remove-widget-for-session (session-tag  &optional (session *session*))
  "Removes a saved widget from the session variable."
  (declare (keyword session-tag)
           (hash-table session))
  (setf (gethash session-tag session) nil))

(defvar *dirty-objects-session-key* :dirty-object-ids)

(defclass <dirty-widget-holder> ()
  ((widgets
    :initform '()
    :documentation "Holds the ids of all widgets that are marked as dirty.")))

(defmethod append-item ((this <dirty-widget-holder>) (item <widget>))
  (let ((found nil)
        (widget-id (id item)))
    (dolist (dirty-widget (slot-value this 'widgets))
      (when (string= dirty-widget
                     widget-id)
        (setf found t)))
    (when (null found)
      (setf
       (slot-value this 'widgets)
       (append (slot-value this 'widgets)
               (list
                widget-id))))))

(defmethod delete-item ((this <dirty-widget-holder>) (item <widget>))
  (setf
   (slot-value this 'widgets)
   (remove (id item)
           (slot-value this 'widgets))))

(defun mark-dirty (widget &optional (session *session*))
  "Marks a widget that it should be rendered ASAP."
  (declare (<widget> widget)
           (hash-table session))

  (when (null (gethash *dirty-objects-session-key* session))
    (setf (gethash *dirty-objects-session-key* session)
          (make-instance '<dirty-widget-holder>)))
  (append-item (get-widget-for-session *dirty-objects-session-key*)
               widget))

(defun demark-dirty (widget &optional (session *session*))
  "Marks a widget as rendered."
  (declare (<widget> widget)
           (hash-table session))
  (when (and
         session
         (get-widget-for-session *dirty-objects-session-key*))
    (delete-item (get-widget-for-session *dirty-objects-session-key*)
                 widget)))

(defvar *dirty-objects-uri-path* "/widgets/dirty")

(defun init-mark-dirty (web &optional (uri-path *dirty-objects-uri-path*))
  (declare (string uri-path)
           (<app> web))
  (setf (ningle:route web
                      uri-path
                      :method :get)
        #'(lambda (params)
            (declare (ignore params))

            (setf (getf (response-headers *response*) :content-type)
                  "application/json")

            (let ((ret-val "{\"dirtyObjects\":["))
              (when (gethash *dirty-objects-session-key* *session*)
                (let* ((dirty-widgets (slot-value (gethash *dirty-objects-session-key*
                                                           *session*)
                                                  'widgets))
                       (len (length dirty-widgets)))
                  (loop for i from 0 below len do
                       (setf ret-val
                             (concatenate 'string
                                          ret-val
                                          "\""
                                          (elt dirty-widgets i)
                                          "\""
                                          (if (= i (- len 1))
                                              ""
                                              ","))))))
              (setf ret-val
                    (concatenate 'string
                                 ret-val
                                 "]}"))
              ret-val))))
