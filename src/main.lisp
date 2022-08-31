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
               :initarg :fbody
               :initform (c? (lambda ,args ,com)))))
     (defun ,name ,args
       (apply (fbody (make-instance ',name))
              (list ,@args)))))

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

(newcom :hello (pname)
        `(:h2 (:on-click click-print) ,(format nil "Hellooo ~A!" pname)))

(newcom :test ()
  `(:div ()
         (:h1 () "This is a Header")
         (:p (:style "color: blue") "This is aa paragraph")
         ,(:hello (val r))))

(defun on-new-window (body)
  (setf *body* body)
  (render (:test) body))

(defun run-app ()
  (initialize 'on-new-window)
  (open-browser))
