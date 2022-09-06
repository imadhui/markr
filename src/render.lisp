(in-package :markr)

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

(defmacro render (component root)
  `(progn
     (setf *body* ,root)
     (defun get-jsx ()
       (parse ,component))
     (setf (inner-html ,root) "" *old-guard* (get-jsx))
     (render-node *old-guard* ,root)))
