;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; This file is a part of the caveman2-widgets project.
;;
;; Copyright (c) 2016 Richard Paul Bäck (richard.baeck@free-your-pc.com)
;; LICENSE: LLGPLv3
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package :cl-user)
(defpackage caveman2-widgets.document
  (:use :cl
        :caveman2-widgets.util
        :caveman2-widgets.widget)
  (:export
   :<js-file>
   :<css-file>

   :<body-widget>

   :<html-document-widget>))
(in-package :caveman2-widgets.document)

(defclass <file> ()
  ((path
    :initform (error "Must supply a path to access the file.")
    :initarg :path
    :reader path)))

(defclass <js-file> (<file>)
  ())

(defmethod render-widget ((this <js-file>))
  (concatenate 'string
               "<script src=\"" (path this) "\" type=\"text/javascript\">"))

(defclass <css-file> (<file>)
  ())

(defmethod render-widget ((this <css-file>))
  (concatenate 'string
               "<link rel=\"stylesheet\" href=\"" (path this) "\">")  )

(defclass <header-widget> ()
  ((css-files
    :initform '()
    :reader css-files)
   (js-files
    :initform '()
    :reader js-files)
   (title
    :initform nil
    :initarg :title
    :reader title)))

(defmethod render-widget ((this <header-widget>))
  (let ((ret-val "<head>"))
    (dolist (css-file (css-files this))
      (setf ret-val
            (concatenate 'string
                         ret-val
                         (render-widget css-file))))
    (dolist (js-file (js-files this))
      (setf ret-val
            (concatenate 'string
                         ret-val
                         (render-widget js-file))))
    (setf ret-val
          (concatenate 'string
                       ret-val
                       (or (title this)
                           "")
                       "</head>"))

    ret-val))

(defmethod append-item ((this <header-widget>) (item <js-file>))
  (setf (slot-value this 'js-files)
        (append (slot-value this 'js-files)
                (list item))))

(defmethod append-item ((this <header-widget>) (item <css-file>))
  (setf (slot-value this 'css-files)
        (append (slot-value this 'css-files)
                (list item))))

(defclass <body-widget> (<widget>)
  ())


(defmethod render-widget-rest ((this <body-widget>)
                               (method (eql :get))
                               (args t))
  (render-widget this))

(defclass <html-document-widget> ()
  ((header
    :initform (make-instance '<header-widget>)
    :type '<header-widget>
    :reader header)
   (body
    :initform (error "Must supply a <body-widget>.")
    :initarg :body
    :type '<body-widget>
    :reader body))
  (:documentation "The body-widget will be wrapped in a div with the id \"body\" automatically."))

(defmethod append-item ((this <html-document-widget>) (item <file>))
  (append-item (header this) item))

(defmethod render-widget ((this <html-document-widget>))
  (let* ((rendered-body (render-widget (body this)))
         ;; (body-first-part (subseq ret-val 0 4))
         ;; (body-second-part (subseq ret-val 4 (length ret-val)))
         )
    (concatenate 'string
                 "<html>"
                 (render-widget (header this))

                 "<body><div id=\"body\">"
                 ;; body-first-part
                 ;; body-second-part
                 rendered-body
                 "</div></body>"
                 "</html>")))
