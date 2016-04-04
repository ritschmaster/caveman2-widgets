;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; This file is a part of the caveman2-widgets project.
;;
;; Copyright (c) 2016 Richard Paul Bäck (richard.baeck@free-your-pc.com)
;; LICENSE: LLGPLv3
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package :cl-user)
(defpackage caveman2-widgets
  (:use :cl
        :caveman2
        :caveman2-widgets.util
        :caveman2-widgets.widget
        :caveman2-widgets.widgets
        :caveman2-widgets.callback-widget
        :caveman2-widgets.document
        :caveman2-widgets.navigation)
  (:export
   ;; from this package
   :init-widgets

   ;; from caveman2-widgets.util
   :append-item
   :delete-item
   :find-item

   ;; from caveman2-widgets.widget
   :<widget>
   :make-widget
   :render-widget
   :render-widget-rest

   :set-widget-for-session
   :get-widget-for-session
   :remove-widget-for-session

   :mark-dirty

   ;; from caveman2-widgets.callback-widget
   :<callback-widget>
   :init-callback-widget
   :label
   :uri-path

   :*button-call-path*
   :<button-widget>
   :make-button

   :*link-call-path*
   :<link-widget>
   :make-link

   ;; from caveman2-widgets.widgets
   :<string-widget>
   :text

   :<composite-widget>
   :widgets

   ;; from caveman2-widgets.document
   :*jquery-cdn-link*

   :<js-file>
   :<css-file>
   :<header-widget>

   :<body-widget>

   :<html-document-widget>
   :header
   :body

   ;; from caveman2-widgets.navigation-widget
   :<navigation-widget>
   :current-page))
(in-package :caveman2-widgets)

(defun init-widgets (webapp &key
                              (javascript-path *javascript-path*)
                              (css-path *css-path*)
                              (rest-path *rest-path*)
                              (button-call-path *button-call-path*)
                              (link-call-path *link-call-path*)
                              (dirty-objects-uri-path *dirty-objects-uri-path*))
  (declare (<app> webapp)
           (string javascript-path)
           (string css-path)
           (string rest-path)
           (string button-call-path)
           (string link-call-path))
  (setf *web* webapp)
  (setf *javascript-path* javascript-path)
  (setf *css-path* css-path)
  (setf *rest-path* rest-path)
  (setf *button-call-path* button-call-path)
  (setf *link-call-path* link-call-path)
  (setf *dirty-objects-uri-path* dirty-objects-uri-path)

  (init-mark-dirty *web* *dirty-objects-uri-path*)

  (defroute-static
      (concatenate 'string
                   *javascript-path*
                   *widgets-js-filename*)
      (merge-pathnames #P"widgets.js" *js-directory*)
    *web*
    "text/javascript; charset=utf-8")
  ;; (defroute-static
  ;;     (concatenate 'string
  ;;                  *css-path*
  ;;                  "/widgets.css")
  ;;     (merge-pathnames #P"widgets.css" *js-directory*)
  ;;   *web*)
  )
