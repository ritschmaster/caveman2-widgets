;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; This file is a part of the caveman2-widgets project.
;;
;; Copyright (c) 2016 Richard Paul Bäck (richard.baeck@free-your-pc.com)
;; LICENSE: LLGPLv3
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package :cl-user)
(defpackage caveman2-widgets-asd
  (:use :cl :asdf))
(in-package :caveman2-widgets-asd)

(defsystem caveman2-widgets
  :version "0.4"
  :author "Richard Paul Bäck"
  :license "LLGPL"
  :depends-on (:trivial-garbage
               :bordeaux-threads
               :drakma
               :caveman2)
  :components ((:module "src"
                        :components
                        ((:file "util")
                         (:file "widget")
                         (:file "callback-widget")
                         (:file "widgets")
                         (:file "document")
                         (:file "navigation")
                         (:file "helper-macros")
                         (:file "caveman2-widgets"))))
  :description "Weblocks like widgets for caveman2."
  :long-description
  #.(with-open-file (stream (merge-pathnames
                             #p"README"
                             (or *load-pathname* *compile-file-pathname*))
                            :if-does-not-exist nil
                            :direction :input)
      (when stream
        (let ((seq (make-array (file-length stream)
                               :element-type 'character
                               :fill-pointer t)))
          (setf (fill-pointer seq) (read-sequence seq stream))
          seq)))
  :in-order-to ((test-op (test-op caveman2-widgets-test))))
