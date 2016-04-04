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
   :<navigation-widget>
   :pages
   :current-page))
(in-package :caveman2-widgets.navigation)

(defclass <navigation-widget> (<body-widget>)
  ((pages
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
    :initform (make-widget :session '<composite-widget>)
    :reader composite)))

(defmethod render-widget ((this <navigation-widget>))
  (let ((ret-val "<ul>")
        (current-widget nil))
    (dolist (page (pages this))
      (setf ret-val
            (concatenate 'string
                         ret-val
                         "<li>"
                         (render-widget
                          (make-link :global (first page)
                                     #'(lambda ()
                                         (setf (current-page this) (second page))
                                         (dolist (thispage (pages this))
                                           (when (string= (second thispage)
                                                          (current-page this))
                                             (setf (slot-value (composite this) 'widgets)
                                                   (list (third thispage)))))
                                         (mark-dirty (composite this))
                                         (second page))))
                         "</li>"))
      (when (string= (second page)
                     (current-page this))
        (setf current-widget (third page))))
    (when (null current-widget)
      (setf current-widget (third (first pages))))
    (setf (slot-value (composite this) 'widgets)
          (list current-widget))
    (setf ret-val
          (concatenate 'string
                       ret-val
                       "</ul>"
                       (render-widget (composite this))))
    ret-val))

(defmethod append-item ((this <navigation-widget>) (item list))
  "@param item This should be a list which should looke like
that: (list \"pagetitle\" \"uri-path\" <widget-for-pagetitle>)."
  (let ((found-widget (find-if #'(lambda (find-item)
                                   (if (string= (second find-item)
                                                (second item))
                                       t
                                       nil))
                               (pages this))))
    (when (null (current-page this))
      (setf (slot-value this 'current-page)
            (second (first (pages this)))))
    (if (null found-widget)
        (progn
          (setf (slot-value this 'pages)
                (append (slot-value this 'pages)
                        (list item)))
          (setf (ningle:route *web*
                              (concatenate 'string
                                           "/"
                                           (second item))
                              :method :get)
                #'(lambda (params)
                    (declare (ignore params))
                    (setf (slot-value this 'current-page)
                          (second item))
                    (render-widget this))))
        nil)))
