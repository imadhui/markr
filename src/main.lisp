(in-package :markr)

(defparameter *body* nil)

(defun click-print (&rest args)
  (setf (color (car args)) (if (evenp (get-universal-time))
                               :green
                               :red)))

(defmacro newcom (name args com)
  `(progn
     (defmodel ,name ()
       ((fbody :accessor fbody
               :initarg :fbody)))
     (defobserver fbody ((self ,name))
       (and old-value
            *body*
            (reconcile *old-guard* (get-jsx) *body*)))
     (defun ,name ,args
       (fbody (make-instance ',name :fbody (c? ,com))))))

(defmodel var ()
  ((val :accessor val
        :initarg :val
        :initform (c-in "vasi"))
   (rul :accessor rul
        :initarg :rul
        :initform (c? (concatenate 'string "va" "ms" "i" (val self))))
   (new :accessor new
        :initarg :new
        :initform (c? (concatenate 'string (rul self) "jing")))))

(defobserver rul ((self var))
  (format t "ruled cell changed!!!"))

(defparameter r (make-instance 'var))

(newcom :hello ()
        `(:h2 (:on-click click-print) ,(format nil "Hellooo ~A!" (val r))))

(newcom :test ()
  `(:div ()
         (:h1 () "This is a Header")
         (:p (:style "color: blue") "This is aa paragraph")
         ,(:hello)
         ,(val r)))

(defun on-new-window (body)
  (setf *body* body)
  (render (:test) body))

(defun run-app ()
  (initialize 'on-new-window)
  (open-browser))
