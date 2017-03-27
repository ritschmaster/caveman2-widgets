;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; This file is a part of the caveman2-widgets project.
;;
;; Copyright (c) 2016 Richard Paul Bäck (richard.baeck@free-your-pc.com)
;; LICENSE: LLGPLv3
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defsystem "caveman2-widgets-test"
  :author "Richard Paul Bäck"
  :license "LLGPL"
  :depends-on ("caveman2-widgets" "prove")
  :components ((:module "t"
                        :components
                        ((:test-file "document")
                         (:test-file "widgets"))))
  :description "Test system for caveman2-widgets"
  :defsystem-depends-on ("prove-asdf")
  :perform (test-op (o c) (symbol-call :prove-asdf :run-test-system c)))

;; NOTE: To run the tests, execute `(asdf:test-system :caveman2-widgets)' in your Lisp.

