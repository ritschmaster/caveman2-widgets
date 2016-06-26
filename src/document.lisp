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
   :*jquery-cdn-link*

   :<js-file>
   :<css-file>
   :<header-widget>
   :title
   :icon-path
   :charset

   :<body-widget>

   :<html-document-widget>
   :header
   :body))
(in-package :caveman2-widgets.document)

(defvar *jquery-cdn-link* "https://code.jquery.com/jquery-2.2.2.min.js"
  "The URL to access jQuery.")

(defclass <file> ()
  ((path
    :initform (error "Must supply a path to access the file.")
    :initarg :path
    :reader path)))

(defclass <js-file> (<file>)
  ())

(defmethod render-widget ((this <js-file>))
  (concatenate 'string
               "<script src=\"" (path this) "\" type=\"text/javascript\"></script>"))

(defclass <css-file> (<file>)
  ())

(defmethod render-widget ((this <css-file>))
  (concatenate 'string
               "<link rel=\"stylesheet\" href=\"" (path this) "\"></link>"))

(defclass <header-widget> ()
  ((css-files
    :initform '()
    :reader css-files)
   (js-files
    :initform
    (list
     (make-instance '<js-file>
                    :path *jquery-cdn-link*)
     (make-instance '<js-file>
                    :path
                    (concatenate 'string
                                 *javascript-path*
                                 *widgets-js-filename*)))
    :reader js-files)
   (title
    :initform nil
    :initarg :title
    :reader title)
   (icon-path
    :initform nil
    :initarg :icon-path
    :accessor icon-path
    :documentation "The path to a specific image to use as icon for page.")
   (charset
    :initform "utf-8"
    :initarg :charset
    :reader charset)))

(defmethod render-widget ((this <header-widget>))
  (with-output-to-string (ret-val)
    (format ret-val "<head>")

    (format ret-val "<title>~a</title>" (or (title this) ""))
    (format ret-val "<meta charset=\"~a\">" (charset this))
    (when (icon-path this)
      (format ret-val "<link href=\"~a\" type=\"image/~a\" rel=\"icon\">"
              (icon-path this)
              (subseq (icon-path this)
                      (- (length (icon-path this)) 3)
                      (length (icon-path this)))))

    (dolist (css-file (css-files this))
      (format ret-val (render-widget css-file)))

    (dolist (js-file (js-files this))
      (format ret-val (render-widget js-file)))

    (format ret-val "</head>")))

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

(defclass <html-document-widget> ()
  ((header
    :initform nil ;; (error "Must supply a <header-widget>.")
    :initarg :header
    :type '<header-widget>
    :accessor header)
   (body
    :initform nil ;; (error "Must supply a <body-widget>.")
    :initarg :body
    :type '<body-widget>
    :accessor body))
  (:documentation "The body-widget will be wrapped in a div with the id \"body\" automatically."))

(defmethod render-widget ((this <html-document-widget>))
  (let* ((rendered-body (render-widget (body this))))
    (concatenate 'string
                 "<html>"
                 (render-widget (header this))
                 "<body><div id=\"body\">"
                 rendered-body
                 "</div></body>"
                 "</html>")))
