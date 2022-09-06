(in-package :markr)

(defclass chrep ()
  ((lid :accessor lid :initarg :lid :initform nil)
   ;; lisp id
   (tobe :accessor tobe :initarg :tobe :initform nil)
   ;; to be parsed
   ))

(defclass node ()
  ((tag :initarg :tag :accessor tag)
   (traits :initarg :traits :accessor traits)
   (sons :initarg :sons :accessor sons)
   (el :initarg :el :accessor el :initform nil)
   (lid :initarg :lid :accessor lid :initform nil)))

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

(defmethod parse ((com chrep))
  (make-instance 'node
                 :lid (lid com)
                 :tag (car (tobe com))
                 :traits (hashify-traits (cadr (tobe com)))
                 :sons (mapcar 'parse (cddr (tobe com)))))

(defmethod parse (com)
  (make-instance 'node
                 :tag (car com)
                 :traits (hashify-traits (cadr com))
                 :sons (mapcar 'parse (cddr com))))
