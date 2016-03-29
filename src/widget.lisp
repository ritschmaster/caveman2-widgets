(in-package :cl-user)
(defpackage caveman2-widget
  (:use :cl)
  (:export :<widget>
           :render-widget
           :render-widget-rest
           :init-widgets))
(in-package :caveman2-widget)

(defvar *rest-path* "rest")
(defvar *rest-methods* '(:get :post :put :patch :delete :head :options))
(defvar *web* nil
  "An <app>-instance")

(defun init-widgets (webapp)
  (declare (<app> webapp))
  (setf *web* webapp))

(defclass <widget> ()
  ((id
    :initform (symbol-name (gensym))
    :accessor id)

   (api-generated-p
    :initform nil
    :accessor api-generated-p
    :allocation :class
    :documentation "To know if the REST API has been generated yet.")
   (all-widgets
    :initform '()
    :accessor all-widgets
    :allocation :class
    :documentation "

Holds all widgets and derived widgets of a specific session. If a
widget is finalized it will be removed from this list
automatically. This list is neccessary for the REST API to get exactly
the given widget."))
  (:documentation ""))

(defgeneric find-widget (class to-find))

(defmethod find-widget ((class <widget>) (to-find string))
  (find-if  #'(lambda (item)
                (declare (<widget> item))
                (string= (id item)
                         to-find))
            (all-widgets class)))

(defmethod initialize-instance ((this <widget>) &key)
  "
Generates a REST for the widget. It will automatically generate
accessable URIs for the HTTP methods stored in *rest-methods*.
The REST can be accessed by the URI /*rest-path*/widget-name"
  (declare (special *web*))

  (when (not (api-generated-p this))
    (let* ((class-name (symbol-name (type-of this)))
           (rest-path (string-downcase
                       (concatenate 'string
                                    "/"
                                    *rest-path*
                                    "/"
                                    (subseq class-name
                                            1
                                            (- (length class-name) 1))))))
      (dolist (cur-method *rest-methods*)

        (setf (ningle:route *web*
                            rest-path
                            :method cur-method)
              #'(lambda (params)
                  (let ((found-widget
                         (find-widget this
                                      (cdr
                                       (assoc "id"
                                              params
                                              :test #'string=)))))
                    (if found-widget
                        (render-widget-rest
                         found-widget
                         cur-method
                         params)
                        "404 Not found"))))))
    (setf (api-generated-p this) t))
  (setf
   (all-widgets this)
   (append (all-widgets this)
           (list this)))
  (trivial-garbage:finalize
   this
   (let ((id (slot-value this 'id)))
     (setf (all-widgets this)
           (remove-if #'(lambda (item)
                          (declare (<widget> item))
                          (string= id (slot-value item 'id)))
                      (all-widgets this)))))
  (print             (all-widgets this)))

(defgeneric render-widget (this)
  (:documentation "@return Returns the HTML representation of the
widget as string. It is intended to use this within a simple HTML
transfer or embedded in another page."))

(defgeneric render-widget-rest (this method args)
  (:documentation "
@return Returns the HTML representation of the
widget as string. To generate a method for a specific HTTP method you
can do the following:

(defmethod render-widget-rest ((this <widget>) (method (eql :get)))
  \"HTML output for the REST when GET.\")"))
