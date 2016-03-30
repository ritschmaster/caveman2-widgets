;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; This file is a part of caveman2-widgets project.
;;
;; Copyright (c) 2016 Richard Paul Bäck (richard.baeck@free-your-pc.com)
;; LICENSE: LLGPLv3
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package :cl-user)
(defpackage caveman2-widgets.button
  (:use :cl
        :caveman2
        :caveman2-widgets.widget)
  (:export :<button-widget>
           :make-button
           :label))
(in-package :caveman2-widgets.button)

(defclass <button-widget> (<widget>)
  ((label
    :initform nil
    :reader label)
   (callback
    :initform nil
    :documentation "The callback function for the button. The return-value of the function
will be the response text."))
  (:documentation ""))

(defmethod render-widget ((this <button-widget>))
  (concatenate 'string
               "<form id=\"" (id this) "\" method=\"post\">"
               "<input type=\"submit\" value=\"" (label this) "\" />"
               "<input type=\"hidden\" value=\"" (getf (request-env *request*) :request-uri) "\" />"
               "</form>"))

(defmethod render-widget-rest ((this <button-widget>)
                               (method (eql :post))
                               (args t))
  (print args)
  (funcall (slot-value this 'callback) this))


(defun make-button (scope label callback)
  (declare (keyword scope)
           (string label)
           (function callback))
  (let ((ret-val (make-widget scope '<button-widget>)))
    (setf (slot-value ret-val 'label) label)
    (setf (slot-value ret-val 'callback) callback)))
