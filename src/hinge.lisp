(in-package :hinge)

;; Dynamic default hinge
(defvar *hinge* nil "The default hinge reactor.")
(defun get-default-hinge (&optional reset)
  (when (and reset *hinge*)
      (close *hinge*)
      (setf *hinge* nil))

  (or *hinge*
      (setf *hinge* (make-instance 'hinge))))

;; Classes
(defclass hinge (ev:ev-loop)
  ((bg-pool :accessor bg-pool
            :documentation "Background work threadpool")

   (emit-queue :accessor emit-queue
               :initform (make-instance 'queue)
               :documentation "Queue of event emissions.")
   (emit-runner :initform nil
                :accessor emit-runner
                :documentation "The queue runner of the `emit-queue'")

   (deliver-queue :accessor deliver-queue
                  :initform (make-instance 'queue)
                  :documentation "Queue of event deliveries.")
   (deliver-runner :initform nil
                   :accessor deliver-runner
                   :documentation "The queue runner of the `deliver-queue'")))
