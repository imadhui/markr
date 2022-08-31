(in-package :markr)

(defparameter *old-guard* nil)

(defun text-onlyp (sons)
  (and (null (cdr sons)) (stringp (car sons))))

(defun set-traits (hash-table el)
  (maphash (lambda (trait value)
             (if (equal trait :on-click)
                 (set-on-click el value)
                 (setf (attribute el trait) value)))
           hash-table))

(defgeneric render-node (node container)
  (:documentation "renders node to the screen"))

(defmethod render-node ((node node) container)
  (let ((current (create-child
                  container
                  (format nil "<~A></~A>" (tag node) (tag node)))))
    (set-traits (traits node) current)
    (setf (el node) current)
    (mapcar (lambda (son)
              (render-node son current))
            (sons node))
    current))

(defmethod render-node ((node text-node) container)
  (setf (el node)
        (create-child container (format nil "<div>~A</div>" (value node)))))

(defparameter *top-level* nil)

(defmacro render (component root)
  `(progn
     (defun get-jsx ()
       (parse ,component))
     (setf (inner-html ,root) "")
     (setf *old-guard* (get-jsx))
     (render-node *old-guard* ,root)))
