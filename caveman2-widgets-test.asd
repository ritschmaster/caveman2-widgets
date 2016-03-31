;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; This file is a part of the caveman2-widgets project.
;;
;; Copyright (c) 2016 Richard Paul Bäck (richard.baeck@free-your-pc.com)
;; LICENSE: LLGPLv3
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package :cl-user)
(defpackage caveman2-widgets-test-asd
  (:use :cl :asdf))
(in-package :caveman2-widgets-test-asd)

(defsystem caveman2-widgets-test
  :author "Richard Paul Bäck"
  :license "LLGPL"
  :depends-on (:caveman2-widgets
               :prove)
  :components ((:module "t"
                :components
                ((:test-file "caveman2-widgets"))))
  :description "Test system for caveman2-widgets"

  :defsystem-depends-on (:prove-asdf)
  :perform (test-op :after (op c)
                    (funcall (intern #.(string :run-test-system) :prove-asdf) c)
                    (asdf:clear-system c)))
