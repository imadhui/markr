(in-package :markr)

(defmodel var ()
  ((val :accessor val
        :initarg :val
        :initform (c-in "vasi"))))

(defobserver val ((self var))
  (and *old-guard* *body* (reconcile *old-guard* (get-jsx) *body*)))

(defparameter *body* nil)

(defparameter *counter* (make-instance 'var :val (c-in 0)))

(defmacro defcom (name args com)
  `(progn
     (defmodel ,name ()
       ((fbody :accessor fbody :initarg :fbody)))
     (defobserver fbody ((self ,name))
       (and old-value
            *body*
            (reconcile *old-guard* (get-jsx) *body*)))
     (defun ,name ,args
       (fbody (make-instance ',name :fbody (c? ,com))))
     (incf (val *counter*))))
