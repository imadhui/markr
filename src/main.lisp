(in-package :markr)

(defmodel var ()
  ((val :accessor val
        :initarg :val
        :initform (c-in "vasi"))))

(defparameter r (make-instance 'var))

(defparameter *counter* (make-instance 'var :val (c-in 0)))

(defobserver val ((self var))
  (print "recompile - reconcile")
  (and *old-guard* *body* (reconcile *old-guard* (get-jsx) *body*)))

(defparameter *body* nil)

(defun click-print (&rest args)
  (setf (color (car args)) (if (evenp (get-universal-time))
                               :green
                               :red)))

(defmacro defcom (name args com)
  `(progn
     (defmodel ,name ()
       ((fbody :accessor fbody
               :initarg :fbody)))
     (defobserver fbody ((self ,name))
       (and old-value
            *body*
            (reconcile *old-guard* (get-jsx) *body*)))
     (defun ,name ,args
       (fbody (make-instance ',name :fbody (c? ,com))))
     (incf (val *counter*))))

(defcom :hello ()
        `(:h5 (:on-click click-print) ,(format nil "Hellooo ~A!" "Madhu")))

(defcom :test ()
  `(:div ()
         (:h1 () "This is a Header")
         (:p (:style "color: red") "This is aa paragraph")
         ,(:hello)
         ,(val r)))

(defun on-new-window (body)
  (setf *body* body)
  (render (:test) body))

(defun run-app ()
  (initialize 'on-new-window)
  (open-browser))
