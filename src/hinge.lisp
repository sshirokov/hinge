(in-package :hinge)

;; Classes
(defclass hinge (ev:ev-loop)
  ((bg-pool :initform (make-instance 'pool :size 5)
            :accessor bg-pool)))

;; Package Dynamics
(defvar *hinge* (make-instance 'hinge) "The current instance of the event reactor.")
