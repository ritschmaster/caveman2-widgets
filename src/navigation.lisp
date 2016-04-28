;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; This file is a part of the caveman2-widgets project.
;;
;; Copyright (c) 2016 Richard Paul Bäck (richard.baeck@free-your-pc.com)
;; LICENSE: LLGPLv3
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package :cl-user)
(defpackage caveman2-widgets.navigation
  (:use :cl
        :caveman2-widgets.util
        :caveman2-widgets.widget
        :caveman2-widgets.widgets
        :caveman2-widgets.callback-widget
        :caveman2-widgets.document)
  (:export
   :<navigation-widget>
   :pages
   :current-page))
(in-package :caveman2-widgets.navigation)

(defclass <navigation-widget> (<html-document-widget> <widget>)
  ((created-paths
    :initform '()
    :allocation :class)
   (pages
    :initform '()
    :initarg :pages
    :reader pages
    :documentation "A list of cons. This slot holds all possible pages
and it should look like: (list (list \"pagetitle\" \"uri-path\" <widget>))")
   (current-page
    :initform nil
    :reader current-page
    :type 'string
    :documentation "The name for the current page to display.")
   (composite
    :initform (make-widget :session '<composite-widget>)
    :reader composite)))

(defgeneric (setf current-page) (value this))

(defmethod (setf current-page) (value (this <navigation-widget>))
  "@param value Must be an uri path string"
  (setf (slot-value this 'current-page) value)
  (dolist (page (pages this))
    (when (string= value
                   (second page))
      (setf (slot-value (composite this) 'widgets)
            (list (third page)))))
  (mark-dirty (composite this)))


(defmethod render-widget ((this <navigation-widget>))
  (setf (body this)
        (let ((str-widget (make-widget :session '<string-widget>))
              (current-widget nil))
          (setf (text str-widget)
                (with-output-to-string (ret-val)
                  (format ret-val "<ul>")
                  (dolist (page (pages this))
                    (format ret-val "<li>")
                    (format ret-val (render-widget
                                     (make-link :global (first page)
                                                #'(lambda ()
                                                    (setf (current-page this) (second page))
                                                    (second page)))))
                    (format ret-val "</li>")
                    (when (string= (second page)
                                   (current-page this))
                      (setf current-widget (third page))))
                  (when (null current-widget)
                    (setf current-widget (third (first (pages this)))))
                  (setf (slot-value (composite this) 'widgets)
                        (list current-widget))
                  (format ret-val "</ul>")
                  (format ret-val (render-widget (composite this)))))
          str-widget))
  (call-next-method this))

(defmethod find-item ((this <navigation-widget>) (item string))
  "@param item The URI path as string."
  (find-if #'(lambda (find-item)
               (if (string= (second find-item)
                            item)
                   t
                   nil))
           (pages this)))

(defmethod append-item ((this <navigation-widget>) (item list))
  "@param item This should be a list which should looke like
that: (list \"pagetitle\" \"uri-path\" <widget-for-pagetitle>)."
  (let ((found-widget (find-item this (second item))))
    (when (null (current-page this))
      (setf (current-page this)
            (second (first (pages this)))))
    (if (null found-widget)
        (progn
          (setf (slot-value this 'pages)
                (append (slot-value this 'pages)
                        (list item)))
          (when (null (find (second item)
                            (slot-value this 'created-paths)
                            :test #'equal))
            (setf (slot-value this 'created-paths)
                  (append (slot-value this 'created-paths)
                          (list (second item))))
            (setf (ningle:route *web*
                                (concatenate 'string
                                             "/"
                                             (second item))
                                :method :get)
                  #'(lambda (params)
                      (declare (ignore params))
                      (setf (current-page this) (second item))
                      (render-widget this)))
            (nconc found-widget (list t))))
        nil)))
