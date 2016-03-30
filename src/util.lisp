;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; This file is a part of caveman2-widgets project.
;;
;; Copyright (c) 2016 Richard Paul Bäck (richard.baeck@free-your-pc.com)
;; LICENSE: LLGPLv3
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package :cl-user)
(defpackage caveman2-widgets.util
  (:use :cl)
  (:export :get-trimmed-class-name
           :clean-list-of-broken-links))
(in-package :caveman2-widgets.util)

(defun get-trimmed-class-name (obj)
  (let ((class-name (symbol-name (type-of obj))))
    (string-downcase
     (subseq class-name
             1
             (- (length class-name) 1)))))

(defun clean-list-of-broken-links (some-list)
  (declare (list some-list))
  (remove-if #'(lambda (item)
                 (null (trivial-garbage:weak-pointer-value item)))
             some-list))
