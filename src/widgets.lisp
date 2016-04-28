;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; This file is a part of the caveman2-widgets project.
;;
;; Copyright (c) 2016 Richard Paul Bäck (richard.baeck@free-your-pc.com)
;; LICENSE: LLGPLv3
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package :cl-user)
(defpackage caveman2-widgets.widgets
  (:use :cl
        :caveman2
        :caveman2-widgets.util
        :caveman2-widgets.widget
        :caveman2-widgets.callback-widget)
  (:export
   :<string-widget>
   :text

   :<composite-widget>
   :widgets))
(in-package :caveman2-widgets.widgets)

(defclass <string-widget> (<widget>)
  ((text
    :initform "" ;(error "Must supply a text for a <string-widget>.")
    :initarg :text
    :accessor text)))

(defmethod render-widget ((this <string-widget>))
  (text this))

(defclass <composite-widget> (<widget>)
  ((widgets
    :initform '()
    :initarg :widgets
    :reader widgets)))

(defmethod append-item ((this <composite-widget>) (item <widget>))
  (setf
   (slot-value this 'widgets)
   (append (slot-value this 'widgets)
           (list
            item))))

(defmethod render-widget ((this <composite-widget>))
  (with-output-to-string (ret-val)
    (dolist (widget (slot-value this 'widgets))
      (format ret-val (render-widget widget)))))
