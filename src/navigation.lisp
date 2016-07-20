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
   :*port*

   :<menu-navigation-widget>
   :<blank-navigation-widget>
   :pages
   :current-page
   :base-path
   :session-tag

   :defnav
   :with-navigation-widget))
(in-package :caveman2-widgets.navigation)

(defvar *port* nil)
(defvar *navigation-widgets*
  (make-array 1 :fill-pointer 0 :adjustable t))

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
    :initform nil
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
                  (format ret-val "<ul class=\"navigation-widget-links\">")
                  (dolist (page (pages this))
                    (format ret-val "<li>")
                    (format ret-val (render-widget
                                     (make-widget
                                      :session '<link-widget>
                                      :label (first page)
                                      :callback #'(lambda ()
                                                    (setf (current-page this) (second page))
                                                    (concatenate 'string
                                                                 (subseq
                                                                  (base-path this) 1)
                                                                 (if (= (length
                                                                         (base-path this))
                                                                        1)
                                                                     ""
                                                                     "/")
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

(defmacro defnav (base-path
                  (header-widget
                   pages
                   &key
                   (kind ''<menu-navigation-widget>)
                   (bottom-widget nil)
                   (session-key :nav-widget)))
  "@param base-path The path for the navigation. Should have a starting \"/\".
@param header-widget A <HEADER-WIDGET> for the navigation and it's children.
@param pages A list of lists."
  `(progn
     (when (null (ningle:route *web*
                               ,base-path
                               :method :get))
       (setf (ningle:route *web*
                           (concatenate 'string
                                        ,base-path)
                           :method :get)
             #'(lambda (params)
                 (with-html-document (doc
                                      ,header-widget)
                   (setf (body doc)
                         (progn
                           (when (null (get-widget-for-session ,session-key))
                             (set-widget-for-session ,session-key (make-widget :session
                                                                               ,kind))
                             (let ((navigation-widget (get-widget-for-session ,session-key)))
                               (setf (slot-value navigation-widget 'composite)
                                     (make-widget :session '<composite-widget>))
                               (dolist (page ,pages)
                                 (append-item navigation-widget
                                              page))
                               (setf (session-tag navigation-widget) ,session-key)
                               (setf (base-path navigation-widget) ,base-path)
                               (when (null (header navigation-widget))
                                 (setf (header navigation-widget)
                                       ,header-widget))))
                           (get-widget-for-session ,session-key)))
                   (setf (bottom doc)
                         ,bottom-widget)))))
     (dolist (page ,pages)
       (when (null (ningle:route *web*
                                 (concatenate 'string
                                              ,base-path
                                              (if (= (length ,base-path)
                                                     1)
                                                  ""
                                                  "/")
                                              (second page))
                                 :method :get))
         (setf (ningle:route *web*
                             (concatenate 'string
                                          ,base-path
                                          (if (= (length ,base-path)
                                                 1)
                                              ""
                                              "/")
                                          (second page))
                             :method :get)
               #'(lambda (params)
                   (with-html-document (doc
                                        ,header-widget)
                     (setf (body doc)
                           (progn
                             (when (null (get-widget-for-session ,session-key))
                               (set-widget-for-session ,session-key (make-widget :session
                                                                                 ,kind))
                               (let ((navigation-widget (get-widget-for-session ,session-key)))
                                 (setf (slot-value navigation-widget 'composite)
                                       (make-widget :session '<composite-widget>))
                                 (dolist (page ,pages)
                                   (append-item navigation-widget
                                                page))
                                 (setf (session-tag navigation-widget) ,session-key)
                                 (setf (base-path navigation-widget) ,base-path)
                                 (when (null (header navigation-widget))
                                   (setf (header navigation-widget)
                                         ,header-widget))))

                             (let ((navigation-widget (get-widget-for-session ,session-key)))
                               (setf (current-page navigation-widget)
                                     (second page))
                               navigation-widget)))
                     (setf (bottom doc)
                           ,bottom-widget))))))))
