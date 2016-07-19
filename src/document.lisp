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
   :path
   :initegrity
   :crossorigin

   :<css-file>

   :<header-widget>
   :title
   :icon-path
   :charset
   :other-header-content

   :<body-widget>

   :<html-document-widget>
   :header
   :body

   :with-html-document))
(in-package :caveman2-widgets.document)

(defvar *jquery-cdn-link* "https://code.jquery.com/jquery-2.2.2.min.js"
  "The URL to access jQuery.")

(defclass <file> ()
  ((path
    :initform (error "Must supply a path to access the file.")
    :initarg :path
    :reader path)
   (integrity
    :initform nil
    :initarg :integrity
    :accessor integrity)
   (crossorigin
    :initform nil
    :initarg :crossorigin
    :accessor crossorigin)))

(defclass <js-file> (<file>)
  ())

(defmethod render-widget ((this <js-file>))
  (with-output-to-string (ret-val)
    (format ret-val
            "<script src=\"~a\" type=\"text/javascript\""
            (path this))
    (when (integrity this)
      (format ret-val " integrity=\"~a\"" (integrity this)))
    (when (crossorigin this)
      (format ret-val " crossorigin=\"~a\"" (crossorigin this)))
    (format ret-val "></script>")    ))

(defclass <css-file> (<file>)
  ())

(defmethod render-widget ((this <css-file>))
  (with-output-to-string (ret-val)
    (format ret-val
            "<link rel=\"stylesheet\" href=\"~a\""
            (path this))
    (when (integrity this)
      (format ret-val " integrity=\"~a\"" (integrity this)))
    (when (crossorigin this)
      (format ret-val " crossorigin=\"~a\"" (crossorigin this)))
    (format ret-val "></link>")))

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
    :accessor title)
   (icon-path
    :initform nil
    :initarg :icon-path
    :accessor icon-path
    :documentation "The path to a specific image to use as icon for page.")
   (charset
    :initform "utf-8"
    :initarg :charset
    :accessor charset)
   (other-header-content
    :initform '()
    :initarg :other-header-content
    :reader other-header-content)))

(defmethod render-widget ((this <header-widget>))
  (with-output-to-string (ret-val)
    (format ret-val "<head>")

    (format ret-val "<title>~a</title>"
            (if (title this)
                (funcall +translate+ (title this))))
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
                (list item)))  )

(defmethod append-item ((this <header-widget>) (item string))
  "The given item will be added to the <head> tag as given."
  (setf (slot-value this 'other-header-content)
        (append (slot-value this 'other-header-content)
                (list item))))

(defclass <html-document-widget> ()
  ((header
    :initform nil ;; (error "Must supply a <header-widget>.")
    :initarg :header
    :accessor header)
   (body
    :initform nil ;; (error "Must supply a <body-widget>.")
    :initarg :body
    :accessor body))
  (:documentation "The body-widget will be wrapped in a div with the id \"body\" automatically."))

(defmethod render-widget ((this <html-document-widget>))
  (concatenate 'string
               "<html>"
               (render-widget (header this))
               "<body><div id=\"body\">"
               (render-widget (body this))
               "</div></body>"
               "</html>"))

(defmacro with-html-document ((document-symbol
                               header
                               &key
                               (doc-kind '<html-document-widget>))
                              &rest body)
  "@param document-symbol The symbol name to access the document.
@param doc-kind The class which is used as HTML document.
@param header A <HEADER-WIDGET> for this document. "
  `(let ((,document-symbol (make-instance ',doc-kind
                                          :header ,header)))
     ,@body
     (render-widget ,document-symbol)))
