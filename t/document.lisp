(in-package :cl-user)
(defpackage caveman2-widgets-test.document
  (:use :cl
        :caveman2-widgets
        :prove))
(in-package :caveman2-widgets-test.document)

(defmethod initialize-instance :after ((this <widget>) &key)
  "Disable the route creation for testing purposes.")

(defmethod render-widget ((this <body-widget>))
  "Sample output."
  "")

(defparameter *js-file-expected-output*
  "<script src=\"/test.js\" type=\"text/javascript\">")
(defvar *js-file* (make-instance '<js-file>
                                 :path "/test.js"))

(defparameter *css-file-expected-output*
  "<link rel=\"stylesheet\" href=\"/test.css\">")
(defvar *css-file*
  (make-instance '<css-file>
                 :path "/test.css"))

(defvar *header-expected-output*
  (concatenate 'string
               "<head>"
               *css-file-expected-output*
               *js-file-expected-output*
               "</head>"))
(defvar *header-widget*
  (let ((header (make-instance 'caveman2-widgets.document::<header-widget>)))
    (append-item header *js-file*)
    (append-item header *css-file*)
    header))

(defparameter *body-expected-output*
  "<div class=\"widget body-widget\"></div>")
(defvar *body-widget*
  (let ((body (make-instance '<body-widget>))
        ;; (js-file (make-instance '<js-file>
        ;;                         :path "/test.js"))
        ;; (css-file (make-instance '<css-file>
        ;;                          :path "/test.css"))
        )
    ;; (append-item header-widget js-file)
    ;; (append-item header-widget css-file)
    body))

(defvar *document-expected-output*
  (concatenate 'string
               "<html>"
               *header-expected-output*
               "<body><div id=\"body\">"
               *body-expected-output*
               "</div></body>"
               "</html>"))
(defvar *document-widget*
  (let ((document (make-instance '<html-document-widget>
                                 :body *body-widget*)))
    (append-item document *js-file*)
    (append-item document *css-file*)
    document))

(plan 5)

(subtest "Testing <js-file>"
  (is (render-widget *js-file*)
      *js-file-expected-output*))

(subtest "Testing <css-file>"
  (is (render-widget *css-file*)
      *css-file-expected-output*))

(subtest "Testing <header-widget>"
  (is (render-widget *header-widget*)
      *header-expected-output*))

(subtest "Testing <body-widget>"
  (is (render-widget *body-widget*)
      *body-expected-output*))

(subtest "Testing <html-document-widget>"
  (is (render-widget *document-widget*)
      *document-expected-output*))

(setf prove:*default-reporter* :fiveAM)
(finalize)
