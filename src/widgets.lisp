;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; This file is a part of the caveman2-widgets project.
;;
;; Copyright (c) 2016 Richard Paul Bäck (richard.baeck@free-your-pc.com)
;; LICENSE: LLGPLv3
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package :cl-user)
(defpackage caveman2-widgets
  (:use :cl
        :caveman2-widgets.widget
        :caveman2-widgets.callback-widget)
  (:export
   ;; from caveman2-widgets.widget
   :<widget>
   :render-widget
   :render-widget-rest
   :init-widgets
   :make-widget
   :set-widget-for-session
   :get-widget-for-session
   :remove-widget-for-session
   :<widget-holder>
   :*rest-path*
   :*button-call-path*

   ;; from caveman2-widgets.callback-widget
   :<callback-widget>
   :init-callback-widget
   :label
   :uri-path

   :<button-widget>
   :make-button

   :<link-widget>
   :make-link
   :*link-call-path*))
(in-package :caveman2-widgets)
