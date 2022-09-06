(in-package :markr)

(defmodel var ()
  ((val :accessor val
        :initarg :val
        :initform (c-in "vasi"))))

(defobserver val ((self var))
  (and *old-guard* *body* (reconcile *old-guard* (get-jsx) *body*)))

(defparameter *counter* (make-instance 'var :val (c-in 0)))

(defmacro defcom (name args com)
  `(progn
     (defmodel ,name ()
       ((fbody :accessor fbody :initarg :fbody)))
     (defobserver fbody ((self ,name))
       (and old-value
            *body*
            (reconcile *old-guard* (get-jsx) *body*)))
     (defun ,name (,@args &key lid)
       (make-instance
        'chrep
        :lid lid
        :tobe (fbody (make-instance ',name :fbody (c? ,com)))))
     (incf (val *counter*))))
