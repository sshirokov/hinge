(in-package :hinge)

;; Dynamic default hinge
(defvar *hinge* nil "The default hinge reactor.")
(defun get-default-hinge ()
  (or *hinge*
      (setf *hinge* (make-instance 'hinge))))

;; Classes
(defclass hinge (ev:ev-loop)
  ((bg-pool :accessor bg-pool
            :documentation "Background work threadpool")))



