;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; This file is a part of the caveman2-widgets project.
;;
;; Copyright (c) 2016 Richard Paul Bäck (richard.baeck@free-your-pc.com)
;; LICENSE: LLGPLv3
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defsystem "caveman2-widgets"
    :version "0.5"
    :author "Richard Paul Bäck"
    :license "LLGPL"
    :depends-on ("trivial-garbage"
                 "moptilities"
                 "caveman2")
    :components ((:module "src"
                          :components
                          ((:file "util")
                           (:file "widget")
                           (:file "callback-widget")
                           (:file "default-widgets")
                           (:file "login")
                           (:file "document")
                           (:file "navigation")
                           (:file "caveman2-widgets"))))
    :description "Weblocks like widgets for caveman2."
    :long-description #.(read-file-string (subpathname *load-pathname* "README"))
    :in-order-to ((test-op (test-op "caveman2-widgets-test"))))
