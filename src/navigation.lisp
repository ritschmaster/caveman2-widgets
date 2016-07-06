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
   :<menu-navigation-widget>
   :<blank-navigation-widget>
   :pages
   :current-page
   :base-path
   :session-tag

   :with-navigation-widget))
(in-package :caveman2-widgets.navigation)

(defclass <navigation-widget> (<html-document-widget> <widget>)
  ((created-paths
    :initform '()
    :accessor created-paths
    :allocation :class)
   (pages
    :initform '()
    :initarg :pages
    :reader pages
    :documentation "A list of cons. This slot holds all possible pages
and it should look like: (list (list \"pagetitle\" \"uri-path\" <widget>))")
   (current-page
    :initform nil
    :accessor current-page
    :type 'string
    :documentation "The name for the current page to display.")
   (composite
    :initform (make-widget :session '<composite-widget>)
    :reader composite)
   (base-path
    :initform ""
    :initarg :base-path
    :accessor base-path
    :documentation "Determines the path for this navigation. It does
not need an initial or trailing forward slash.")
   (session-tag
    :initarg :session-tag
    :accessor session-tag))
  (:documentation "This is an abstract widget which implements all
  interactions with a navigation but not the RENDER-WIDGET. Please
  subclass this class as you want (default implementations are
<MENU-NAVIGATION-WIDGET> and <BLANK-NAVIGATION-WIDGET>."))

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
          t)
        nil)))

(defclass <menu-navigation-widget> (<navigation-widget>)
  ())

(defmethod render-widget ((this <menu-navigation-widget>))
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
                                                    (concatenate 'string
                                                                 (base-path this)
                                                                 "/"
                                                                 (second page))))))
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

(defclass <blank-navigation-widget> (<navigation-widget>)
  ())

(defmethod render-widget ((this <blank-navigation-widget>))
  (setf (body this)
        (let ((str-widget (make-widget :session '<string-widget>))
              (current-widget nil))
          (setf (text str-widget)
                (with-output-to-string (ret-val)
                  (format ret-val (render-widget (composite this)))))
          str-widget))
  (call-next-method this))

(defmacro with-navigation-widget ((session-key
                                   navigation-widget-symbol
                                   header-widget
                                   &key
                                   (base-path "/")
                                   (kind '<menu-navigation-widget>))
                                  &rest body)
  "Macro to use a menu navigation widget very easily. For every different
SESSION-KEY there will be created a new navigation. Therefore you can
call this macro everytime you want to modify your navigation widget
without any headache.

@param navigation-widget-symbol You can access the navigation widget
inside the macro by giving a symbol and using that symbol afterwards.

@return The RENDER-WIDGET of the navigation-widget"
  `(progn
     (make-widget :session '<widget>) ;; init session
     (flet ((create-navigation ()
              (set-widget-for-session ,session-key (make-widget :session
                                                                ',kind))
              (let ((,navigation-widget-symbol (get-widget-for-session ,session-key)))
                (setf (session-tag ,navigation-widget-symbol) ,session-key)
                (setf (base-path ,navigation-widget-symbol) ,base-path)
                (when (null (header ,navigation-widget-symbol))
                  (setf (header ,navigation-widget-symbol)
                        ,header-widget))
                ,@body

                (render-widget ,navigation-widget-symbol))))
       (create-navigation)
       (dolist (page (pages (get-widget-for-session ,session-key)))
         (when (null (find (second page)
                           (created-paths (get-widget-for-session ,session-key))
                           :test #'equal))
           (setf (created-paths (get-widget-for-session ,session-key))
                 (append (created-paths (get-widget-for-session ,session-key))
                         (list (second page)))))
         (when (null (ningle:route *web*
                                   (concatenate 'string
                                                "/"
                                                ,base-path
                                                "/"
                                                (second page))
                                   :method :get))
           (setf (ningle:route *web*
                               (concatenate 'string
                                            "/"
                                            ,base-path
                                            "/"
                                            (second page))
                               :method :get)
                 #'(lambda (params)
                     (declare (ignore params))
                     (let ((nav-widget (get-widget-for-session ,session-key)))
                       (when (null nav-widget)
                         (create-navigation)
                         (setf nav-widget (get-widget-for-session ,session-key)))
                       (setf (current-page nav-widget) (second page))
                       (render-widget nav-widget)))))))
     (render-widget (get-widget-for-session ,session-key))))
