;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; This file is a part of caveman2-widgets project.
;;
;; Copyright (c) 2016 Richard Paul Bäck (richard.baeck@free-your-pc.com)
;; LICENSE: LLGPLv3
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package :cl-user)
(defpackage caveman2-widgets
  (:use :cl
        :caveman2-widgets.widget
        :caveman2-widgets.button)
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

   ;; from caveman2-widgets.button
   :<button-widget>
   :make-button
   :label))
(in-package :caveman2-widgets)
