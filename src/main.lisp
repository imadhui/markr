(defpackage markr
  (:use :cl :clog))

(in-package :markr)

(defparameter counter 0)

(defparameter my-node nil)

(defun click-print (&rest args)
  (setf (color (car args)) (if (evenp (incf counter))
                               :green
                               :red)))

(defmacro defcom (name args component)
  (declare (ignore args))
  `(defun ,name ,args ,component))

(defparameter s "heheh")

(defcom :hello ()
    `(:h1 (:on-click click-print) ,s))

(defcom :test ()
  `(:div ()
         (:h1 () "This is a Header")
         (:p (:style "color: blue") "This is a paragraph")))

(defun on-new-window (body)
  (render (:test) body))

(defun run ()
  (initialize 'on-new-window)
  (open-browser))
