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
;;; Queue drained by an idle runner
(defclass running-queue (c2mop:funcallable-standard-object)
  ((owner :initarg :owner :accessor owner)

   (queue :initform (make-instance 'queue) :accessor queue)
   (priority :initform 0 :accessor priority)
   (runner :initform (make-instance 'ev:ev-idle) :accessor runner))
  (:metaclass c2mop:funcallable-standard-class)
  (:documentation "A wrapper that binds together a queue and an idle runner
to process it."))

(defgeneric event-callback (inst l w e)
  (:documentation "Callback curried and submitted to libev for the `runner' of `inst'"))

(defmethod initialize-instance :after ((inst running-queue) &key)
  (c2mop:set-funcallable-instance-function inst (curry #'event-callback inst))
  (ev:set-idle (owner inst) (runner inst) inst))

;;; Hinge
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
