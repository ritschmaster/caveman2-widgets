;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; This file is a part of the caveman2-widgets project.
;;
;; Copyright (c) 2016 Richard Paul Bäck (richard.baeck@free-your-pc.com)
;; LICENSE: LLGPLv3
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package :cl-user)
(defpackage caveman2-widgets.callback-widget
  (:use :cl
        :caveman2
        :caveman2-widgets.util
        :caveman2-widgets.widget)
  (:export
   :get-from-callback-args
   
   :<callback-widget>
   :label
   :callback
   :uri-path
   :http-method
   :classes

   :<button-widget>
   :*button-call-path*

   :<link-widget>
   :*link-call-path*

   :<form-field>
   :name
   :required
   :supplied
   :error-happened
   :error-message
   :check-function
   
   :<input-field>
   :input-type 
   :value

   :<option-field>
   :display-type
   :<select-field>
   :options
   :multiple

   :<radio-field>
   :checked-option

   :<form-widget>
   :input-fields))
(in-package :caveman2-widgets.callback-widget)

(defun get-from-callback-args (key args)
  "@param key A string which might be in the args
@param args The value passed by a <CALLBACK-WIDGET> in its callback
function."
  (declare (string key))
  (cdr
   (assoc-if #'(lambda (item)
                 (string= (string-downcase item)
                          (string-downcase key)))
             args)))

(defclass <callback-widget> (<widget>)
  ((label
    :initform nil
    :initarg :label
    :reader label)
   (callback
    :initform #'(lambda (args) "")
    :initarg :callback
    :reader callback
    :documentation "")
   (uri-path
    :initform (error "Must supply an uri-path to access the widget.")
    :initarg :uri-path
    :reader uri-path
    :documentation "This slot should give the exact path to access this widget.")
   (http-method
    :initform  (error "Must supply a method to access the HTTP URL.")
    :initarg :http-method
    :reader http-method
    :documentation "This slot should be one of the HTTP methods as
keyword (e.g. :post or :get")
   (classes
    :initform nil
    :initarg :classes
    :accessor classes)))

(defun test-widget-if-session (scope widget-id &optional (session *session*))
  (declare (keyword scope)
           (string widget-id))
  (when (eql scope :session)
    (let* ((session-widget-holder
            (gethash :widget-holder session))
           (sessioned-widget (if session-widget-holder
                                 (find-item session-widget-holder
                                            widget-id)
                                 nil)))
      (when (null sessioned-widget)
        (throw-code 404)))))

(defvar *button-call-path* "buttons")
(defvar *input-field-for-old-uri* "oldUri")

(defclass <button-widget> (<callback-widget>)
  ()
  (:default-initargs
   :uri-path ""
    :http-method :post)
  (:documentation "The callback function will be called when the user
presses the button."))

(defmethod initialize-instance :after ((this <button-widget>) &key)
  (setf (slot-value this 'uri-path)
        (concatenate 'string
                     "/"
                     *button-call-path*
                     "/"
                     (id this)))
  (setf (ningle:route *web*
                      (uri-path this)
                      :method (http-method this))
        #'(lambda (params)
            (test-widget-if-session (widget-scope this)
                                    (id this))
            (funcall (slot-value this 'callback)
                     params)
            (let ((oldUrl (get-value-for-cons-list
                           params
                           *input-field-for-old-uri*)))
              (when oldUrl
                (redirect oldUrl))))))

(defmethod render-widget ((this <button-widget>))
  (with-output-to-string (ret-val)
    (format ret-val "<form method=\"post\" action=\"~a\">"
            (uri-path this))
    (format ret-val "<button type=\"submit\" class=\"~a\">~a</button>"
            (or (classes this) "")
            (funcall +translate+ (label this)))
    (format ret-val "<input type=\"hidden\" name=\"~a\" value=\"~a\" />
</form>"
            *input-field-for-old-uri*
            (getf (request-env *request*) :request-uri))))

(defvar *link-call-path* "links")

(defclass <link-widget> (<callback-widget>)
  ((target-foreign-p
    :initform nil
    :initarg :target-foreign-p
    :documentation "When the given link redirects absolute (like http://...)."))
  (:default-initargs
   :uri-path ""
    :http-method :get)
  (:documentation "The callback function will be called when the user
clickes the link. The function must return a string. The returned
string should be an URL to which the server should redirect."))

(defmethod initialize-instance :after ((this <link-widget>) &key)
  (setf (slot-value this 'uri-path)
        (concatenate 'string
                     "/"
                     *link-call-path*
                     "/"
                     (id this)))
  (setf (ningle:route *web*
                      (uri-path this)
                      :method (http-method this))
        #'(lambda (params)
            (test-widget-if-session (widget-scope this)
                                    (id this))
            (redirect (concatenate 'string
                                   (if (slot-value this 'target-foreign-p)
                                       ""
                                       "/")
                                   (funcall (slot-value this
                                                        'callback)
                                            params)))))
  (setf (ningle:route *web*
                      (uri-path this)
                      :method :post)
        #'(lambda (params)
            (test-widget-if-session (widget-scope this)
                                    (id this))
            (funcall (slot-value this
                                 'callback)
                     params))))

(defmethod render-widget ((this <link-widget>))
  (concatenate 'string
               "<a href=\"" (uri-path this) "\">"
               (funcall +translate+ (label this))
               "</a>"))

(defclass <form-field> ()
  ((name
    :initarg :name
    :initform (error "Must specify a name for the form field.")
    :reader name)
   (label
    :initarg :label
    :initform ""
    :accessor label
    :documentation "The label which will be placed before the <input> tag.")
   (supplied
    :initform t
    :accessor supplied
    :documentation "A highly frequented slot. It tells if the form
field was filled by the client.") 
   (required
    :initarg :required
    :initform nil
    :accessor required)
   (check-function
    :initarg :check-function
    :initform #'(lambda (str) t)
    :accessor check-function
    :documentation "Checks the user input for flaws. Takes one
argument - the string passed by the user. Should return non-nil if
everything is correct.")
   (error-happened
    :initform nil
    :accessor error-happened
    :documentation "A highly frequented slot. Non-nil indicates that
an error occurred.")
   (error-message
    :initarg :error-message
    :initform ""
    :accessor error-message
    :documentation "The error message that will be displayed if
ERROR-HAPPENED is non-nil. The error message will be translated
before rendered.")))

(defmethod render-widget ((this <form-field>))
  (error "Not implemented."))

(defmethod render-widget :around ((this <form-field>)) 
  (with-output-to-string (ret-val)
    (format ret-val "<div class=\"form-field\">") 
    (format ret-val "<div class=\"form-field-label\">~a</div>" 
            (funcall +translate+ (label this)))
    (format ret-val (call-next-method this))
    (cond
      ((error-happened this)
       (format ret-val "<div class=\"error-message\">~a</div>"
               (funcall +translate+ (error-message this)))
       (setf (error-happened this) nil))
      ((and
        (required this)
        (not (supplied this)))
       (format ret-val "<div class=\"not-supplied\">~a</div>"
               (funcall +translate+ "(not supplied)"))
       (setf (supplied this) t))
      ((required this)
       (format ret-val "<div class=\"required-field\">~a</div>"
               (funcall +translate+ "(form field required)")))
      (t
       ;; nothing bad happened
       ))
    (format ret-val "</div>")))

(defclass <input-field> (<form-field>)
  ((input-type
    :initarg :input-type
    :initform (error "Must specify an input type.")
    :reader input-type) 
   (value
    :initarg :value
    :initform (error "Must specify an input value.")
    :reader value))) 

(defmethod render-widget ((this <input-field>))
  (format nil "<div class=\"input-field\">
<input type=\"~a\" name=\"~a\" value=\"~a\" />
</div>"
          (input-type this)
          (name this)
          (value this)))

(defclass <option-field> ()
  ((value
    :initarg :value
    :initform (error "Must supply a value.")
    :reader value)
   (display-value
    :initarg :display-value
    :initform nil
    :reader display-value
    :documentation "If NIL then the displayed value will equal the
used value.")))

(defmethod render-widget ((this <option-field>))
  (format nil "<option value=\"~a\">~a</option>"
          (value this)
          (or (display-value this) (value this))))

(defclass <select-field> (<form-field>)
  ((options
    :initarg :options
    :initform '()
    :accessor options)
   (multiple
    :initarg :multiple
    :initform nil
    :accessor multiple
    :documentation "Non-nil allows multiple choices.")))

(defmethod append-item ((this <select-field>) (item <option-field>))
  (setf (options this)
        (append (options this)
                (list item))))

(defmethod render-widget ((this <select-field>))
  (with-output-to-string (ret-val)
    (format ret-val "<div class=\"select-field\">
<select name=\"~a\" ~a>"
            (name this)
            (if (required this) "required-field" "")
            (if (multiple this)
                "multiple"
                ""))
    (dolist (option (options this))
      (format ret-val (render-widget option)))
    (format ret-val "</select>
</div>")))

(defclass <radio-field> (<form-field>)
  ((options
    :initarg :options
    :initform '()
    :accessor options)
   (checked-option
    :initform nil
    :initarg :checked-option
    :accessor checked-option
    :documentation "The option which is checked. Must be a
number. Start with 0.")))

(defmethod append-item ((this <radio-field>) (item string))
  (setf (options this)
        (append (options this)
                (list item)))) 

(defmethod render-widget ((this <radio-field>))
  (with-output-to-string (ret-val)
    (format ret-val "<div class=\"radio-field\">")
    (let ((i 0))
      (dolist (option (options this))
        (format ret-val "<div class=\"option\"><input type=\"radio\" name=\"~a\" value=\"~a\" ~a>~a
</div>"
                (name this)
                option
                (if (and
                     (checked-option this)
                     (= (checked-option this) i))
                    "checked"
                    "")
                (funcall +translate+ option))
        (incf i)))
    (format ret-val "</div>")))

(defclass <form-widget> (<button-widget>)
  ((input-fields
    :initarg :input-fields
    :initform '()
    :reader input-fields
    :documentation "A list of <FORM-FIELD> objects.")))

(defmethod initialize-instance :after ((this <form-widget>) &key)
  (setf (slot-value this 'uri-path)
        (concatenate 'string
                     "/"
                     *button-call-path*
                     "/"
                     (id this)))
  (setf (ningle:route *web*
                      (uri-path this)
                      :method (http-method this))
        #'(lambda (params)
            (test-widget-if-session (widget-scope this)
                                    (id this))
            ;; Check if all required fields have been
            ;; filled and if they have been filled
            ;; correctly
            (if (not (set-required-present this
                                           params))
                (mark-dirty this) ;; something happened
                (funcall (slot-value this 'callback) ;; everything ok
                         params))
            (let ((oldUrl (get-value-for-cons-list
                           params
                           *input-field-for-old-uri*)))
              (when oldUrl
                (redirect oldUrl))))))

(defmethod append-item ((this <form-widget>) (item <form-field>))
  (setf (slot-value this 'input-fields)
        (append (input-fields this)
                (list item))))

(defgeneric set-required-present (this container))
(defmethod set-required-present ((this <form-widget>) (container list))
  "Sets the SUPPLIED slot of the INPUT-FIELDS based on a given list. Sets
also the ERROR-HAPPENED slot by running the check function.

@param container Should be the ARGS parameter of the the callback
which is an ALIST.

@return Returns NIL if any requirement is not met or any . Non-nil value if
all requirements are met."
  (let ((ret t)
        (value-of-field nil))
    (dolist (field (input-fields this))
      (setf value-of-field
            (cdr
             (assoc-if #'(lambda (item)
                           (string= (string-downcase item)
                                    (string-downcase (name field))))
                       container)))
      (cond
        ((and ;; not supplied
          (required field)
          (string= ""
                   value-of-field))
         (setf (supplied field) nil)
         (setf ret nil))
        ((not
          (funcall (check-function field)
                   value-of-field))
         (setf (error-happened field) t)
         (setf ret nil))
        (t
         (setf (supplied field) t))))
    ret))

(defmethod render-widget ((this <form-widget>))
  (with-output-to-string (ret-val)
    (format ret-val "<form method=\"post\" action=\"~a\">"
            (uri-path this)) 
    (dolist (input-field (input-fields this))
      (format ret-val
              (render-widget input-field)))
    (format ret-val "<button type=\"submit\" class=\"~a\">~a</button>"
            (or (classes this) "")
            (funcall +translate+ (label this))) 
    (format ret-val "<input type=\"hidden\" name=\"~a\" value=\"~a\" /> 
</form>"
            *input-field-for-old-uri*
            (getf (request-env *request*) :request-uri))))
