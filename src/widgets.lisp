;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; This file is a part of the caveman2-widgets project.
;;
;; Copyright (c) 2016 Richard Paul Bäck (richard.baeck@free-your-pc.com)
;; LICENSE: LLGPLv3
;;
;; Purpose:
;; --------
;; This package provides the basic/default widgets which are most likely to be
;; used in an every day project.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package :cl-user)
(defpackage caveman2-widgets.widgets
  (:use :cl
        :caveman2
        :caveman2-widgets.util
        :caveman2-widgets.widget
        :caveman2-widgets.callback-widget)
  (:export
   :<string-widget>
   :text

   :<composite-widget>
   :widgets

   :<function-widget>
   :fn

   :<table-item>
   :get-as-list
   :<table-widget>
   :column-descriptions))
(in-package :caveman2-widgets.widgets)

(defclass <string-widget> (<widget>)
  ((text
    :initform "" ; (error "Must supply a text for a <string-widget>.")
    :initarg :text
    :accessor text)))

(defmethod render-widget ((this <string-widget>))
  (text this))

(defclass <composite-widget> (<widget>)
  ((widgets
    :initform '()
    :initarg :widgets
    :reader widgets)))

(defmethod append-item ((this <composite-widget>) (item <widget>))
  (setf
   (slot-value this 'widgets)
   (append (slot-value this 'widgets)
           (list
            item))))

(defmethod render-widget ((this <composite-widget>))
  (with-output-to-string (ret-val)
    (dolist (widget (slot-value this 'widgets))
      (format ret-val (render-widget widget)))))

(defclass <function-widget> (<widget>)
  ((fn
    :initform #'(lambda () "")
    :initarg :function
    :accessor fn))
  (:documentation "Uses a fucntion for rendering. The given function
should return a string which will then be rendered."))

(defmethod render-widget ((this <function-widget>))
  (funcall (fn this)))

(defclass <table-item> ()
  ()
  (:documentation "This class is used to display items in a widget. It
essentially is only relevant for the GET-AS-LIST method which is
used by the <TABLE-WIDGET>."))

(defgeneric get-as-list (this)
  (:documentation "This generic method should return the entire object
as cons-list. If you want to hide certain slots, just keep them out of
the list!"))

(defmethod get-as-list ((this <table-item>))
  (error "Not implemented."))

(defclass <table-widget> (<composite-widget>)
  ((producer
    :initarg :producer
    :initform (error "Must supply a producer.")
    :reader producer
    :documentation "A function which supplies the table with
<TABLE-ITEM> objects. It is possible to use a heterogenous list of
<TABLE-ITEM> objects but it is strongly advised to watch out by doing
so (accessing not available slots might cause an error!). The producer
should be able to deliver a specific amount of items too. Consider the
following lambda as producer:
(lambda (&key amount)
  (list (make-instance '<table-item>)))")
   (colum-descriptions
    :initform '()
    :initarg :column-descriptions
    :reader column-descriptions
    :documentation "This is a list of cons which where the cons is one column. The first value of the cons is the keyword to display. The second value of the cons is the table header for that column. For the header (second value) you can use HTML code!

Example:
(list (list :firstcolumn \"First column\")) ")
(progressive-p
 :initform nil
 :initarg :progressive-p
 :reader progressive-p
 :documentation "If non-nil the table loads the next items
progressively when the end of the table/side is reached. This is only
available if JavaScript is enabled. The non-nil value describes how
many lines will be delivered on each load."))
(:documentation "The RENDER-WIDGET-REST method is splitted:
- GET :: returns the entire widget
- POST :: returns ARGS lines of the table"))

(defmethod append-item ((this <table-widget>) (item cons))
  "@param item Must not have more than two values. The first value is
the keyword which should be displayed when a <TABLE-ITEM> object is
accessed. The second value is the header text for the column."

  (setf (slot-value this 'column-descriptions)
        (append (slot-value this 'column-descriptions)
                item)))

(defmethod render-widget ((this <table-widget>))
  (with-output-to-string (ret-val)
    (format ret-val "<table>")
    (format ret-val "<tr>")
    (dolist (column (column-descriptions this))
      (format ret-val
              "<th>~a</th>"
              (second column)))
    (format ret-val "</tr>")
    (dolist (item (if (null (progressive-p this))
                      (funcall (producer this))
                      (funcall (producer this) (progressive-p this))))
      (format ret-val "<tr>")
      (dolist (column (column-descriptions this))
        (format ret-val "<td>~a</td>"
                (getf (get-as-list item)
                      (first column))))
      (format ret-val "</tr>"))
    (format ret-val "</table>")))

(defmethod render-widget-rest ((this <widget>)
                               (method (eql :get))
                               (args t))
  (render-widget this))

(defmethod render-widget-rest ((this <widget>)
                               (method (eql :post))
                               (args string))
  (format t args)
  (render-widget this))
