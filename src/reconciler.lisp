(in-package :markr)

(defgeneric reconcile (n1 n2 container)
  (:documentation "reconcile the old screen and new data"))

(defmethod reconcile ((n1 text-node) (n2 text-node) container)
  (setf (el n2) (el n1))
  (setf (text-value (el n2)) (value n2)))

(defmethod reconcile (n1 n2 container)
  (setf (el n2) (el n1))
  (if (equal (tag n1) (tag n2))
      (mapcar (lambda (node1 node2)
                (reconcile node1 node2 (el n1)))
              (sons n1) (sons n2))
      (setf (el n2) (replace-element
                     (el n1)
                     (render-node n2 container)))))
