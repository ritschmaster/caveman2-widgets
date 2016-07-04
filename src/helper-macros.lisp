;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; This file is a part of the caveman2-widgets project.
;;
;; Copyright (c) 2016 Richard Paul Bäck (richard.baeck@free-your-pc.com)
;; LICENSE: LLGPLv3
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package :cl-user)
(defpackage caveman2-widgets.helper-macros
  (:use :cl
        :caveman2
        :caveman2-widgets.widget
        :caveman2-widgets.document
        :caveman2-widgets.navigation)
  (:export
   :with-navigation-widget))
(in-package :caveman2-widgets.helper-macros)

(defmacro with-navigation-widget ((session-key
                                   navigation-widget-symbol
                                   header-widget)
                                  &rest body)
  "Macro to use a menu navigation widget very easily. For every different
SESSION-KEY there will be created a new navigation. Therefore you can
call this macro everytime you want to modify your navigation widget
without any headache.

@param navigation-widget-symbol You can access the navigation widget
inside the macro by giving a symbol and using that symbol afterwards.

@return The RENDER-WIDGET of the navigation-widget"
  `(progn
     (make-widget :session '<widget>)
     (set-widget-for-session ,session-key (make-widget :session '<menu-navigation-widget>))
     (let ((,navigation-widget-symbol (get-widget-for-session ,session-key)))
       (when (null (header ,navigation-widget-symbol))
         (setf (header ,navigation-widget-symbol)
               ,header-widget))
       ,@body
       (render-widget ,navigation-widget-symbol))))
