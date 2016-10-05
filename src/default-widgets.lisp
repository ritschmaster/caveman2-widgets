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
(defpackage caveman2-widgets.default-widgets
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

   :<hcomposite-widget>

   :<function-widget>
   :fn

   :<table-item>
   :get-as-list
   :<table-widget>
   :column-descriptions
   :progressive-p
   :default-progressive-load-value

   :<viewgrid-item>
   :render-as
   :<viewgrid-widget>
   :view
   :on-view
   :on-view-label
   :max-items-to-display

   :<border-widget>))
(in-package :caveman2-widgets.default-widgets)

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
    :accessor widgets)))

(defmethod append-item ((this <composite-widget>) (item <widget>))
  (setf
   (slot-value this 'widgets)
   (append (slot-value this 'widgets)
           (list
            item))))

(defmethod render-widget ((this <composite-widget>))
  (with-output-to-string (ret-val)
    (dolist (widget (slot-value this 'widgets))
      (format ret-val "<div class=\"item\">~a</div>"
              (render-widget widget)))))

(defclass <hcomposite-widget> (<composite-widget>)
  ()
  (:documentation "This is a Horizontal composite widget. Therefore it
is essentially the same as the <COMPOSITE-WIDGET> with the
difference that is displays its widgets horizontally. "))

(defmethod render-widget ((this <hcomposite-widget>))
  (with-output-to-string (ret-val)
    (format ret-val "<div style=\"overflow:hidden\">
<div style=\"overflow:hidden\">")
    (dolist (widget (slot-value this 'widgets))
      (format ret-val "<div class=\"item\" style=\"float:left\">")
      (format ret-val (render-widget widget))
      (format ret-val "</div>"))
    (format ret-val "</div>
</div>")))

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
should be able to deliver a specific amount of items too (AMOUNT = m,
ALREADY = n, LENGTH-P = nil => gets items from (m) to (m + n)). To
know how many items are avaible please supply the key LENGTH-P which
returns a number when non-nil. Consider the following lambda as
producer:
(lambda (&key
           amount
           (already 0)
           (length-p nil))
  (list (make-instance '<table-item>)))")
   (colum-descriptions
    :initform '()
    :initarg :column-descriptions
    :reader column-descriptions
    :documentation "This is a list of cons which where the cons is one column. The first value of the cons is the keyword to display. The second value of the cons is the table header for that column. For the header (second value) you can use HTML code!

Example:
(list (list :firstcolumn \"First column\")) ")))

(defmethod append-item ((this <table-widget>) (item cons))
  "@param item Must not have more than two values. The first value is
the keyword which should be displayed when a <TABLE-ITEM> object is
accessed. The second value is the header text for the column."

  (setf (slot-value this 'column-descriptions)
        (append (slot-value this 'column-descriptions)
                item)))

(defmethod render-widget-header ((this <table-widget>))
  (with-output-to-string (ret-val)
    (format ret-val "<tr>")
    (dolist (column (column-descriptions this))
      (format ret-val
              "<th>~a</th>"
              (second column)))
    (format ret-val "</tr>")))

(defmethod render-widget-body ((this <table-widget>))
  (with-output-to-string (ret-val)
    (dolist (item (funcall (producer this)))
      (format ret-val "<tr>")
      (dolist (column (column-descriptions this))
        (format ret-val "<td>~a</td>"
                (getf (get-as-list item)
                      (first column))))
      (format ret-val "</tr>"))))

(defmethod render-widget ((this <table-widget>))
  (with-output-to-string (ret-val)
    (format ret-val (render-widget-header this))
    (format ret-val (render-widget-body this))))

(defmethod render-widget :around ((this <table-widget>))
  (with-output-to-string (ret-val)
    (format ret-val "<table>")
    (format ret-val (call-next-method this))
    (format ret-val "</table>")))

;; (defclass <limited-table-widget> (<table-widget>)
;;   ((progressive-p
;;     :initform nil
;;     :initarg :progressive-p
;;     :reader progressive-p
;;     :documentation "If non-nil the table loads the next items
;; progressively when the end of the table/side is reached. This is only
;; available if JavaScript is enabled.")
;;    (default-progressive-load-value
;;        :initform 10
;;      :initarg :default-progressive-load-value
;;      :accessor default-progressive-load-value))
;;   (:documentation "The RENDER-WIDGET-REST method is splitted:
;; - GET :: returns the entire widget
;; - POST :: returns ARGS lines of the table"))

;; (defmethod render-widget ((this <limited-table-widget>))
;;   (with-output-to-string (ret-val)
;;     (format ret-val (render-widget-header this))

;;     (when  (null (javascript-available *session*))
;;       (format ret-val (render-widget-body this)))))

;; (defmethod render-widget-rest ((this <limited-table-widget>)
;;                                (method (eql :post))
;;                                (args t))
;;   "The POST render returns only the table rows.

;; @param args In the args there must be two specific cons. The first one
;; is a cons which describes how many lines to load - this must have the
;; accessor AMOUNT. The second cons describes how many lines are already
;; loaded. The accessor for this must be ALREADY. If the cons
;; '(\"length_p\" . \"true\") is within the args everything else will be
;; ignored and the amount of available items will be returned."


;;   (with-output-to-string (ret-val)
;;     (if (string-case-insensitive=
;;          (cdr
;;           (assoc 'length_p args
;;                  :test #'string-case-insensitive=))
;;          "true")
;;         (progn
;;           (format ret-val "~a"
;;                   (funcall (producer this)
;;                            :length-p t)))
;;         (progn
;;           (dolist (item
;;                     (if (null (progressive-p this))
;;                         (funcall (producer this))
;;                         (funcall (producer this)
;;                                  :amount (or (parse-integer
;;                                               (cdr
;;                                                (assoc 'amount args
;;                                                       :test #'string-case-insensitive=))
;;                                               :junk-allowed t)
;;                                              (default-progressive-load-value this))
;;                                  :already (or (parse-integer
;;                                                (cdr
;;                                                 (assoc 'already args
;;                                                        :test #'string-case-insensitive=))
;;                                                :junk-allowed t)
;;                                               0))))
;;             (dolist (column (column-descriptions this))
;;               (format ret-val "<td>~a</td>"
;;                       (getf (get-as-list item)
;;                             (first column))))
;;             (format ret-val "</tr>"))))))


(defclass <viewgrid-item> ()
  ())

(defgeneric render-as (this view))
(defmethod render-as ((this <viewgrid-item>) (view (eql :full)))
  (error "Not implemented."))
(defmethod render-as ((this <viewgrid-item>) (view (eql :short)))
  (error "Not implemented."))
(defmethod render-as ((this <viewgrid-item>) (view (eql :oneline)))
  (error "Not implemented."))

(defclass <viewgrid-widget> (<widget>)
  ((producer
    :initarg :producer
    :initform (error "Must supply a producer.")
    :reader producer
    :documentation "A function which supplies the table with
<VIEWGRID-ITEM> objects. It is possible to use a heterogenous list of
<VIEWGRID-ITEM> objects. The producer should be able to deliver a
specific amount of items too (FROM = m, TO = n, LENGTH-P = nil => gets
items from (m) to (m + n); FROM = m, TO = NIL, LENGTH-P = nil => gets
items from (m) to LENGTH). To know how many items are avaible please
supply the key LENGTH-P which returns a number when non-nil. Consider
the following lambda as producer:
(lambda (&key
           (from 0)
           (to nil)
           (length-p nil))
  (list (make-instance '<table-item>)))")
   (view
    :initarg :view
    :initform (error "Must supply which view should be used.")
    :accessor view
    :documentation "The value of this slot is passed to the RENDER-AS
method. Therefore you can use an arbitary value but you have to
provide an according method for that value!")
   (on-view
    :initarg :on-view
    :initform nil
    :accessor on-view
    :documentation "Must either be NIL or a function which takes one
argument. The passed argument is the item in the viewgrid which is
viewed. If the value is NIL the client can't view a specific item.")
   (on-view-label
    :initarg :on-view-label
    :initform "View dataview item"
    :accessor on-view-label
    :documentation "The text which should be displayed on the button for the
ON-VIEW callback.")
   (max-items-to-display
    :initarg :max-items-to-display
    :initform nil
    :accessor max-items-to-display
    :documentation "Non-nil value must be a number which describes how
many items will be displayed on each viewgrid page.")
   (display-selector
    :initarg :display-selector
    :accessor display-selector
    :documentation "A page selector are the page numbers which the
user can click to access the page. If NIL don't display a page
selector. The non-nil value has to be the URI path in which the
viewgrid is used. E.g. display-selector = \"view\" if the viewgrid is
accessed on the page /view.")
   (current-from
    :initform 0)
   (selector
    :initform nil)
   (prev-button
    :initform nil)
   (next-button
    :initform nil)))

(defmethod render-widget ((this <viewgrid-widget>))
  (with-output-to-string (ret-val)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Actually rendering the items:
    (let ((producer-args (list :from (slot-value this 'current-from)))
          (all-items nil))
      (when (max-items-to-display this)
        (setf producer-args
              (append producer-args
                      (list :to (+
                                 (slot-value this 'current-from)
                                 (max-items-to-display this))))))
      (setf all-items (apply (producer this) producer-args))
      (when (not (apply (producer this) producer-args))
        (format ret-val "<div class=\"error-no-items\">~a</div>"
                (funcall
                 +translate+
                 "Sorry but there are no viewgrid items available!")))
      (dolist (item all-items)
        (format ret-val "<div class=\"viewgrid-line\">
      <div class=\"viewgrid-item\">~a</div>
      <div class=\"viewgrid-item-view\">~a</div>
      </div>"
                (render-as item (view this))
                (if (on-view this)
                    (render-widget
                     (make-widget :session '<link-widget>
                                  :label (on-view-label this)
                                  :callback
                                  #'(lambda (args)
                                      (funcall (on-view this) item))))
                    "")))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

      (when (max-items-to-display this)
        (format ret-val "<div class=\"pagenumber\">~a~a~a~a</div>"
                (funcall +translate+ "Viewing page ")
                (+ 1
                   (ceiling
                    (if (= (slot-value this 'current-from)
                           (max-items-to-display this))
                        (max-items-to-display this)
                        (slot-value this 'current-from))
                    (max-items-to-display this)))
                (funcall +translate+ " of ")
                (ceiling (funcall (producer this)
                                  :length-p t)
                         (max-items-to-display this)))

        (when (display-selector this)
          (when (null (slot-value this 'selector))
            (setf (slot-value this 'selector)
                  (make-widget :session '<hcomposite-widget>))
            (dolist (i (loop for i from 0 to (funcall (producer this)
                                                      :length-p t)
                          by (max-items-to-display this)
                          for j from 1 by 1
                          collect (list i j)))
              (append-item (slot-value this 'selector)
                           (make-widget :session '<link-widget>
                                        :label (format nil "~a" (second i))
                                        :callback
                                        #'(lambda (args)
                                            (setf (slot-value this 'current-from)
                                                  (first i))
                                            (mark-dirty this)
                                            (display-selector this))))))
          (format ret-val "<div class=\"viewgrid-selector\">")
          (format ret-val
                  (render-widget
                   (slot-value this 'selector)))
          (format ret-val "<div>"))

        (format ret-val "<div class=\"viewgrid-buttons\">")
        (when (null (slot-value this 'prev-button))
          (setf (slot-value this 'prev-button)
                (make-widget :session '<button-widget>
                             :label "Previous"
                             :callback
                             #'(lambda (args)
                                 (when (>=
                                        (-
                                         (slot-value this 'current-from)
                                         (max-items-to-display this))
                                        0)
                                   (decf (slot-value this 'current-from)
                                         (max-items-to-display this))
                                   (mark-dirty this))))))
        (format ret-val
                (render-widget
                 (slot-value this 'prev-button)))

        (when (null (slot-value this 'next-button))
          (setf (slot-value this 'next-button)
                (make-widget :session '<button-widget>
                             :label "Next"
                             :callback
                             #'(lambda (args)
                                 (when (<
                                        (+
                                         (slot-value this 'current-from)
                                         (max-items-to-display this))
                                        (funcall (producer this)
                                                 :length-p t))
                                   (incf (slot-value this 'current-from)
                                         (max-items-to-display this))
                                   (mark-dirty this))))))
        (format ret-val
                (render-widget
                 (slot-value this 'next-button)))
        (format ret-val "</div>")))))

(defclass <border-widget> (<widget>)
  ((north
    :initarg :north
    :initform nil
    :accessor north)
   (east
    :initarg :east
    :initform nil
    :accessor east)
   (south
    :initarg :south
    :initform nil
    :accessor south)
   (west
    :initarg :west
    :initform nil
    :accessor west)
   (center
    :initarg :center
    :initform nil
    :accessor center)))

(defmethod render-widget ((this <border-widget>))
  (with-output-to-string (ret-val)
    (when (north this)
      (format ret-val "<div class=\"north\">~a</div>"
              (render-widget (north this))))
    (when (east this)
      (format ret-val "<div class=\"east\">~a</div>"
              (render-widget (east this))))
    (when (south this)
      (format ret-val "<div class=\"south\">~a</div>"
              (render-widget (south this))))
    (when (west this)
      (format ret-val "<div class=\"west\">~a</div>"
              (render-widget (west this))))
    (when (center this)
      (format ret-val "<div class=\"center\">~a</div>"
              (render-widget (center this))))))
