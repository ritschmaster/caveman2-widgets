(in-package :cl-user)
(defpackage caveman2-widgets-test.document
  (:use :cl
        :caveman2-widgets
        :prove))
(in-package :caveman2-widgets-test.document)

(defmethod initialize-instance :after ((this <widget>) &key)
  "Disable the route creation for testing purposes.")

(in-package :caveman2-widgets.widget)
(setf *session* (make-hash-table :test 'equal))
(in-package :caveman2-widgets-test.document)

(defmethod render-widget ((this <body-widget>))
  "Sample output."
  "")

(defparameter *js-file-expected-output*
  "<script src=\"/test.js\" type=\"text/javascript\"></script>")
(defvar *js-file* (make-instance '<js-file>
                                 :path "/test.js"))

(defparameter *css-file-expected-output*
  "<link rel=\"stylesheet\" href=\"/test.css\"></link>")
(defvar *css-file*
  (make-instance '<css-file>
                 :path "/test.css"))

(defvar *header-expected-output*
  (concatenate 'string
               "<head>"
               *css-file-expected-output*
               "<script src=\"https://code.jquery.com/jquery-2.2.2.min.js\" type=\"text/javascript\"></script>"
               "<script src=\"/widgets/js/widgets.js\" type=\"text/javascript\"></script>"
               *js-file-expected-output*
               "<meta charset=\"utf-8\">"
               "<title>Testpage</title>"
               "</head>"))
(defvar *header-widget*
  (let ((header (make-instance '<header-widget>
                               :title "Testpage")))
    (append-item header *js-file*)
    (append-item header *css-file*)
    header))

(defparameter *body-expected-output*
  "\<div id=\"[A-Za-z0-9]+\" class=\"widget body-widget\"\>\</div>")
(defvar *body-widget*
  (let ((body (make-instance '<body-widget>)))
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
                                 :header *header-widget*
                                 :body *body-widget*)))
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
         (like (render-widget *body-widget*)
             *body-expected-output*))

(subtest "Testing <html-document-widget>"
         (like (render-widget *document-widget*)
               *document-expected-output*))

(setf prove:*default-reporter* :fiveAM)
(finalize)
