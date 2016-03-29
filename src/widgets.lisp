(in-package :cl-user)
(defpackage caveman2-widgets
  (:use :cl
        :caveman2-widget)
  (:export :<widget>
           :render-widget
           :render-widget-rest
           :init-widgets
           :make-widget
           :<widget-holder>))
(in-package :caveman2-widgets)
