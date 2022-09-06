(in-package :markr)

(defparameter *body* nil "stores the html body object")

(defparameter *old-guard* nil "stores the top level old node, which is used for comparison during reconciliation")

(defparameter *events* (list :on-click :on-resize :on-focus :on-blur
                             :on-change :on-input :on-drag-start :on-drag
                             :on-drag-end :on-drag-enter :on-drag-leave :on-reset
                             :on-drag-over :on-drop :on-focus-in :on-focus-out
                             :on-search :on-select :on-submit :on-select
                             :on-context-menu :on-double-click :on-mouse-right-click :on-mouse-enter
                             :on-mouse-leave :on-mouse-over :on-mouse-out :on-mouse-down
                             :on-mouse-up :on-mouse-move :on-pointer-enter :on-pointer-leave
                             :on-pointer-over :on-pointer-out :on-pointer-down :on-pointer-up
                             :on-pointer-move :on-touch-start :on-touch-move :on-touch-end
                             :on-touch-cancel :on-character :on-key-down :on-key-up
                             :on-key-press :on-copy :on-cut :on-paste))

(defparameter *event-setters* (list 'set-on-click 'set-on-resize 'set-on-focus 'set-on-blur
                                    'set-on-change 'set-on-input 'set-on-drag-start 'set-on-drag
                                    'set-on-drag-end 'set-on-drag-enter 'set-on-drag-leave 'set-on-reset
                                    'set-on-drag-over 'set-on-drop 'set-on-focus-in 'set-on-focus-out
                                    'set-on-search 'set-on-select 'set-on-submit 'set-on-select
                                    'set-on-context-menu 'set-on-double-click 'set-on-mouse-right-click 'set-on-mouse-enter
                                    'set-on-mouse-leave 'set-on-mouse-over 'set-on-mouse-out 'set-on-mouse-down
                                    'set-on-mouse-up 'set-on-mouse-move 'set-on-pointer-enter 'set-on-pointer-leave
                                    'set-on-pointer-over 'set-on-pointer-out 'set-on-pointer-down 'set-on-pointer-up
                                    'set-on-pointer-move 'set-on-touch-start 'set-on-touch-move 'set-on-touch-end
                                    'set-on-touch-cancel 'set-on-character 'set-on-key-down 'set-on-key-up
                                    'set-on-key-press 'set-on-copy 'set-on-cut 'set-on-paste))

(defparameter *eve* (make-hash-table :test #'equal) "keeps a list of events and their setters")

;; populate the hashtable
(mapcar (lambda (event setter) (setf (gethash event *eve*) setter)) *events* *event-setters*)

(defun eventp (attribute)
  "return the setter for the event, if attribute is an event"
  (gethash attribute *eve*))

(defmacro aif (test then &optional else)
  `(let ((it ,test))
     (if it ,then ,else)))

(defun ret (lid &optional (node *old-guard*))
  (unless (null node)
    (if (equal (lid node) lid)
        (el node)
        (let ((kids (sons node)))
          (do ((kid (ret lid (pop kids)) (ret lid (pop kids))))
              ((or (null kids) (typep kid 'clog:clog-element)) kid))))))
