;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; This file is a part of the caveman2-widgets project.
;;
;; Copyright (c) 2016 Richard Paul Bäck (richard.baeck@free-your-pc.com)
;; LICENSE: LLGPLv3
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package :cl-user)
(defpackage caveman2-widgets.util
  (:use :cl
        :moptilities
        :caveman2)
  (:export
   :+translate+
   :*automatically-set-languages*
   :*language-key-in-session*
   :*application-root*
   :*static-directory*
   :*js-directory*
   :*css-directory*

   :append-item
   :delete-item
   :find-item

   :defroute-static
   :get-trimmed-class-name
   :clean-list-of-broken-links
   :get-value-for-cons-list
   :has-trailing-slash
   :string-case-insensitive=
   :javascript-available
   :check-and-set-language
   :accepted-languages))
(in-package :caveman2-widgets.util)

(defparameter *application-root* (asdf:system-source-directory :caveman2-widgets))
(defparameter *static-directory* (merge-pathnames #P"static/" *application-root*))
(defparameter *js-directory* (merge-pathnames #P"js/" *static-directory*))
(defparameter *css-directory* (merge-pathnames #P"css/" *static-directory*))

(defvar *automatically-set-languages* t)
(defvar *language-key-in-session* :accept-language)
(defvar +translate+ #'(lambda (text
                          &key
                            plural-p
                            genitive-form-p
                            items-count
                            accusative-form-p
                            language
                            &allow-other-keys)
                        text)
  "This should be a function which should translate a given text. You can modify
 it to your needs. By default this function does nothing.

@param plural-p
@param genitive-form-p
@param items-count
@param accusative-form-p
@param language")

(defun get-trimmed-class-name (obj
                               &key
                                 (get-all nil))
  "@param obj The object of which the name is required
@param get-all-self If non-nil returns the entire hierarchy."
  (if get-all
      (let* ((super-classes
              (superclasses obj
                            :proper? nil))
             (to
              (- (length super-classes)
                 3))
             (class-names ""))
        ;; deleting the standard classes:
        (setf super-classes
              (subseq super-classes
                      0
                      (if (< to 0)
                          0
                          to)))
        (dolist (super-class
                  ;; let the first class be the first:
                  (reverse super-classes))
          (let ((class-name (string-downcase
                             (symbol-name
                              (class-name-of super-class)))))
            (setf class-names
                  (concatenate 'string
                               class-names
                               (subseq class-name
                                       1
                                       (- (length class-name) 1))
                               " "))))
        class-names)
      (let ((class-name (symbol-name (type-of obj))))
        (string-downcase
         (subseq class-name
                 1
                 (- (length class-name) 1))))))

(defun clean-list-of-broken-links (some-list)
  (declare (list some-list))
  (remove-if #'(lambda (item)
                 (null (trivial-garbage:weak-pointer-value item)))
             some-list))

(defun get-value-for-cons-list (some-list key)
  (declare (string key)
           (list some-list))
  (cdr
   (assoc key
          some-list
          :test #'equal)))

(defun defroute-static (uri-path path app content-type)
  (declare (string uri-path)
           (pathname path)
           (string content-type))
  (setf (ningle:route app
                      uri-path
                      :method :get)
        #'(lambda (params)
            (declare (ignore params))

            (setf (getf (response-headers *response*) :content-type)
                  content-type)

            (let ((ret-val ""))
              (with-open-file (input path :direction :input)
                (loop
                   for line = (read-line input nil 'eof)
                   until (eq line 'eof) do
                     (setf ret-val
                           (format nil "~a~%~a"
                                   ret-val
                                   line))))
              ret-val))))

(defgeneric append-item (this item))

(defmethod append-item ((this t) (item t))
  (error "Not supported yet!"))

(defgeneric delete-item (this item))

(defmethod delete-item ((this t) (item t))
  (error "Not supported yet!"))

(defgeneric find-item (this to-find))

(defmethod find-item ((this t) (item t))
  (error "Not supported yet!"))

(defun has-trailing-slash (str)
  (declare (string str))
  (let ((len (length str)))
    (string= (subseq str
                     (1- len)
                     len)
             "/")))

(defun string-case-insensitive= (str1 str2)
  (string= (string-downcase str1)
           (string-downcase str2)))

(defgeneric (setf javascript-available) (value session))
(defmethod (setf javascript-available) (value (session hash-table))
  (setf (gethash :javascript-available session) value))

(defgeneric javascript-available (session))
(defmethod javascript-available ((session hash-table))
  (gethash :javascript-available session))

(defun check-and-set-language (request session)
  (when (and
         *automatically-set-languages*
         (null (accepted-languages session)))
    (setf
     (accepted-languages session)
     (gethash "accept-language" (request-headers request)))))


(defgeneric accepted-languages (session))
(defmethod accepted-languages ((session hash-table))
  (gethash *language-key-in-session* session))

(defgeneric (setf accepted-languages) (value session))
(defmethod (setf accepted-languages) (value (session hash-table))
  (setf (gethash *language-key-in-session* session)
        value))
