(defpackage markr
  (:use :cl :clog))
(in-package :markr)

(defparameter counter 0)
(defun click-print (&rest args)
  (setf (color (car args)) (if (evenp (incf counter))
                               :green
                               :red)))

(defun text-onlyp (sons)
  (and (null (cdr sons)) (stringp (car sons))))

(defun read-traits (trait-list)
  (let ((h (make-hash-table :test #'equal)))
    (do ((first (pop trait-list) (pop trait-list))
         (second (pop trait-list) (pop trait-list)))
        ((null trait-list) (setf (gethash first h) second))
      (setf (gethash first h) second))
    h))

(defun set-traits (ht el)
  (maphash (lambda (trait value)
             (if (equal trait :on-click)
                 (set-on-click el value)
                 (setf (attribute el trait) value)))
           ht))


(defclass node ()
  ((tag :initarg :tag
        :accessor tag)
   (traits :initarg :traits
           :accessor traits)
   (sons :initarg :sons
         :accessor sons)))

(defparameter my-node nil)

(defun parse (com)
  "converts component to node"
  (setf my-node (make-instance 'node :tag (car com)
                                     :traits (read-traits (cadr com))
                                     :sons (cddr com))))

(defmacro defcom (args component)
  (declare (ignore args))
  `(parse ',component))

(defun render (node container)
  (let ((current (create-child
                  container
                  (format nil "<~A>Tt</~A>" (tag node) (tag node)))))
    (set-traits (traits node) current)
    (if (text-onlyp (sons node))
        (setf (text-value current) (car (sons node))))
    current))


;(defun render (node container)
;  )

(defun on-new-window (body)
  (render (defcom ()
              (:h1 (:on-click click-print)
                   "Hello World! (click me!)"))
          body))

(defun run ()
  (initialize 'on-new-window)
  (open-browser))
