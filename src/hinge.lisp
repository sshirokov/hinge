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
   (priority :initarg :priority :initform 0 :accessor priority)
   (runner :initform (make-instance 'ev:ev-idle) :accessor runner))
  (:metaclass c2mop:funcallable-standard-class)
  (:documentation "A wrapper that binds together a queue and an idle runner
to process it."))

(defgeneric event-callback (inst l w e)
  (:documentation "Callback curried and submitted to libev for the `runner' of `inst'")
  (:method (inst l w e)
    (declare (ignore l w e))
    (if-let (thunk (dequeue (queue inst)))
      (funcall thunk)
      (ev:stop-watcher (owner inst) (runner inst) :keep-callback t))))

(defmethod enqueue ((rqueue running-queue) thunk)
  (enqueue (queue rqueue) thunk)
  (ev:start-watcher (owner rqueue) (runner rqueue)))

(defmethod close ((rqueue running-queue) &key &allow-other-keys)
  (ev:stop-watcher (owner rqueue) (runner rqueue)))

(defmethod initialize-instance :after ((inst running-queue) &key)
  "Bind the `inst' funcallable callback.
Set the watcher priority and bind the instance as the callback"
  (c2mop:set-funcallable-instance-function inst (curry #'event-callback inst))
  (setf (ev:watcher-slot (runner inst) :priority) (priority inst))
  (ev:set-idle (owner inst) (runner inst) inst))

;;; Hinge
(defclass hinge (ev:ev-loop)
  ((bg-pool :accessor bg-pool
            :documentation "Background work threadpool")

   (queues :initform '((:low . 0)
                       (:normal . 1)
                       (:high . 2))
           :accessor queues
           :documentation "A set of work queues.
If an alist of (:name . priority) it will be transformed
to a hashtable of {:name => (ev:idle-watcher :priority priority)}")))
