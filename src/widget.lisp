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
   :render-widget
   :render-widget-rest
   :make-widget
   :id

   :find-item

   :set-widget-for-session
   :get-widget-for-session
   :remove-widget-for-session

   :mark-dirty
   :*dirty-objects-uri-path*
   :init-mark-dirty

   :*rest-path*
   :*web*
   :*javascript-path*
   :*widgets-js-filename*
   :*css-path*
   :*css-route*))
(in-package :caveman2-widgets.widget)

(defvar *rest-path* "rest")
(defvar *rest-methods* '(:get :post :put :patch :delete :head :options))
(defvar *web* nil
  "An <app>-instance")

(defvar *javascript-path*
  "/widgets/js"
  "An absolute path where caveman2-widgets' JavaScript files can be accessed.")
(defvar *css-path*
  "/widgets/css"
  "An absolute route where caveman2-widgets' CSS files can be accessed.")

(defvar *widgets-js-filename* "/widgets.js"
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

   (api-generated-p
    :initform (make-hash-table)
    :accessor api-generated-p
    :allocation :class
    :documentation "
To know if the REST API has been generated yet. This is a hash-table which
stores a boolean for each derived class. E.g. to check if the API for <widget>
has been created you can call (gethash '<widget> (api-generated-p this))"))
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
                        (throw-code 404)))))))
    (setf (gethash (type-of this) (api-generated-p this)) t)))

(defgeneric render-widget (this)
  (:documentation "@return Returns the HTML representation of the
widget as string. It is intended to use this within a simple HTML
transfer or embedded in another page."))

(defmethod render-widget :around ((this <widget>))
  (demark-dirty this)
  (with-output-to-string (ret-val)
    (format ret-val "<div id=\"" (id this) "\" class=\"widget ")
    (format ret-val (get-trimmed-class-name this))
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

(defgeneric make-widget (scope class)
  (:documentation ""))

(defmethod make-widget ((scope (eql :global)) (class symbol))
  (let ((ret-val (make-instance class)))
    (append-item *global-widget-holder* ret-val)
    ret-val))

(defmethod make-widget ((scope (eql :session)) (class symbol))
  (let ((holder (gethash :widget-holder *session*))
        (ret-val (make-instance class)))
    (when (null holder)
      (setf holder (make-instance '<widget-holder>))
      (setf (gethash :widget-holder *session*)
            holder))
    (append-item holder ret-val)
    ret-val))

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
