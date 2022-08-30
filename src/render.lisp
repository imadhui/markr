(in-package :markr)

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
    (mapcar (lambda (son)
              (render-node son current))
            (sons node))
    current))

(defmethod render-node ((node text-node) container)
  (place-text-inside-bottom-of container (value node)))

(defmacro render (component root)
  `(render-node (parse ,component) ,root))
