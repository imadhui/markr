(in-package :markr)

(defgeneric rec (n1 n2 container)
  (:documentation "reconcile the old screen and new data"))

(defmethod rec ((n1 text-node) (n2 text-node) container)
  (setf (el n2) (el n1))
  (setf (text-value (el n2)) (value n2)))

(defmethod rec (n1 n2 container)
  (if (and (equal (tag n1) (tag n2))
           (equalp (traits n1) (traits n2)) ; fix this. it's inefficient on the browser side.
           (= (length (sons n1)) (length (sons n2))))
      (progn
        (setf (el n2) (el n1))
        (mapcar (lambda (node1 node2)
                  (rec node1 node2 (el n1)))
                (sons n1) (sons n2)))
      (progn
        (setf (el n2) (place-after
                       (el n1)
                       (render-node n2 container)))
        (remove-from-dom (el n1)))))

(defun reconcile (old-node new-node container)
  (rec old-node new-node container)
  (setf *old-guard* new-node))
