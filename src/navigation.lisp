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
        :caveman2-widgets.document))
  (:export
   :*jquery-cdn-link*

   :<js-file>
   :<css-file>
   :<header-widget>
   :title
   :charset

   :<body-widget>

   :<html-document-widget>))
(in-package :caveman2-widgets.navigation)
