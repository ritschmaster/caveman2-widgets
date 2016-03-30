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
        :caveman2-widgets.util
        :caveman2-widgets.widget)
  (:export :<button-widget>
           :make-button
           :label
           :*button-call-path*))
(in-package :caveman2-widgets.button)

(defvar *button-call-path* "buttons")
(defvar *input-field-for-old-uri* "oldUri")

(defclass <button-widget> (<widget>)
  ((label
    :initform nil
    :reader label))
  (:documentation "This "))

(defmethod render-widget ((this <button-widget>))
  (concatenate 'string
               "<form method=\"post\" action=\"/"
               *button-call-path*
               "/"
               ;; (get-trimmed-class-name this) "?id="
               (id this)
               "\">"
               "<input type=\"submit\" value=\"" (label this) "\" />"
               "<input type=\"hidden\" name=\"" *input-field-for-old-uri* "\" value=\""
               (getf (request-env *request*) :request-uri) "\" />"
               "</form>"))


(defun make-button (scope label callback)
  "@param callback The callback function for the button. This function
   will be called when the user presses the button."
  (declare (keyword scope)
           (string label)
           (function callback))
  (let ((ret-val (make-widget scope '<button-widget>)))
    (setf (slot-value ret-val 'label) label)
    ;; (setf (slot-value ret-val 'callback) callback)
    (setf (ningle:route *web*
                        (concatenate 'string
                                     "/"
                                     *button-call-path*
                                     "/"
                                     (id ret-val))
                        :method :post)
          #'(lambda (params)
              (funcall callback)
              (let ((oldUrl (get-value-for-ningle-request-parameter
                             params
                             *input-field-for-old-uri*)))
                (when oldUrl
                  (redirect oldUrl)))))
    ret-val))
