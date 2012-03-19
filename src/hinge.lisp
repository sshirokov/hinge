(in-package :hinge)

(defclass hinge (ev:ev-loop)
  ())

;; Dynamics
(defvar *hinge* (make-instance 'hinge) "The current instance of the event reactor.")
