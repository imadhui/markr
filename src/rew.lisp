(defpackage rew
  (:use :cl :clog :cells)
  (:shadow :value)
  (:export :defcom :render))

(in-package :rew)

(defparameter *body* nil)
(defparameter *old-guard* nil)

(defmodel sig ()
  ((val :accessor val :initarg :val :initform (c-in 0))))

(defparameter *recom-signal* (make-instance 'sig))
(defobserver val ((self sig))
  ;; reconcile after recompilation of a component
  (and *old-guard* *body* (reconcile *old-guard* (get-jsx) *body*)))

(defmodel component ()
  ((fbody :accessor fbody :initarg :fbody)))

(defobserver fbody ((self component))
  ;; reconcile after dependencies change
  (and old-value
            *body*
            (reconcile *old-guard* (get-jsx) *body*)))

(defmacro defcom (name args com)
  `(progn
     (defun ,name ,args
       (make-instance 'component :fbody (c? ,com)))
     (incf (val *recom-signal*))))

(defun my-subst-if (mod test tree)
  "Substitutes new for subtrees for which test is true."
  (labels ((s (subtree)
             (cond ((funcall test subtree) (funcall mod subtree))
                   ((atom subtree) subtree)
                   (t (let ((car (s (car subtree)))
                            (cdr (s (cdr subtree))))
                        (if (and (eq car (car subtree))
                                 (eq cdr (cdr subtree)))
                            subtree
                            (cons car cdr)))))))
    (s tree)))

(defun query-htcl (instance)
  (let ((s (my-subst-if 'fbody (lambda (x) (typep x 'component)) instance)))
    (if (equal s (my-subst-if 'fbody (lambda (x) (typep x 'component)) s))
        s (query-htcl s))))

;; Parsing

(defclass node ()
  ((tag :initarg :tag :accessor tag)
   (traits :initarg :traits :accessor traits)
   (sons :initarg :sons :accessor sons)
   (el :initarg :el :accessor el :initform nil)))

(defclass text-node (node)
  ((tag :initform :text)
   (traits :initform nil)
   (sons :initform nil)
   (value :initarg :value :accessor value)))

(defun hashify-traits (trait-list)
  (let ((h (make-hash-table :test #'equal)))
    (do ((first (pop trait-list) (pop trait-list))
         (second (pop trait-list) (pop trait-list)))
        ((null trait-list) (setf (gethash first h) second))
      (setf (gethash first h) second))
    h))

(defgeneric parse (com)
  (:documentation "parses lisp into nodes"))

(defmethod parse ((com string))
  (make-instance 'text-node :value com))

(defmethod parse (com)
  (make-instance 'node
                 :tag (car com)
                 :traits (hashify-traits (cadr com))
                 :sons (mapcar 'parse (cddr com))))

;; Render

(defun eventp (attribute)
  "return the setter for the event, if attribute is an event"
  (gethash attribute markr::*eve*))

(defmacro aif (test then &optional else)
  `(let ((it ,test))
     (if it ,then ,else)))

(defun set-traits (hash-table el)
  (maphash (lambda (trait value)
             (aif (eventp trait)
                  (funcall it el value)
                  (setf (attribute el trait) value)))
           hash-table))

(defgeneric render-node (node container)
  (:documentation "renders node to the screen"))

(defun create-tag-string (tag) (format nil "<~A></~A>" tag tag))

(defmethod render-node ((node node) container)
  (let ((current (create-child container (create-tag-string (tag node)))))
    (set-traits (traits node) current)
    (setf (el node) current)
    (mapcar (lambda (son) (render-node son current)) (sons node))
    current))

(defmethod render-node ((node text-node) container)
  (setf (el node) (create-div container :content (value node))))

(defmacro render (form root)
  `(let ((ins ,form))
     (setf *body* ,root)
     (defun get-jsx ()
       (parse (query-htcl ins)))
     (setf (inner-html ,root) "" *old-guard* (get-jsx))
     (render-node *old-guard* ,root)))

;; Reconciler

(defgeneric rec (n1 n2 container)
  (:documentation "reconcile the old screen and new data"))

(defmethod rec ((n1 text-node) (n2 text-node) container)
  (setf (el n2) (el n1))
  (setf (text-value (el n2)) (value n2)))

(defmethod rec (n1 n2 container)
  (if (and (equal (tag n1) (tag n2))
           (equalp (traits n1) (traits n2)) ; fix this. it's inefficient on the browser side.
           (= (length (sons n1)) (length (sons n2))))
      (progn
        (setf (el n2) (el n1))
        (mapcar (lambda (node1 node2)
                  (rec node1 node2 (el n1)))
                (sons n1) (sons n2)))
      (progn
        (setf (el n2) (place-after
                       (el n1)
                       (render-node n2 container)))
        (remove-from-dom (el n1)))))

(defun reconcile (old-node new-node container)
  (rec old-node new-node container)
  (setf *old-guard* new-node))
