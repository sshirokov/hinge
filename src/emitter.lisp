(in-package :hinge)

;; Class
(defclass emitter ()
  ((owner :accessor owner
          :initarg :owner
          :initform (get-default-hinge)
          :documentation "The hinge instance the event emitter functions within.")

   (oneshots :accessor oneshots)
   (listeners :accessor listeners)

   (events :accessor events
           :initform (make-instance 'queue :size 20)
           :documentation "A queue of emitted events. Drained with an interval
to avoid blocking the event loop delivering events.")
   (runner :accessor event-runner
           :initform nil
           :documentation "A watcher that runs through the event queue
delivering events to listeners.")))

(defgeneric reset (emitter)
  (:documentation "Reset the emitter to its empty initial state.")
  (:method ((emitter emitter))
    (setf (oneshots emitter) (make-hash-table)
          (listeners emitter) (make-hash-table :test 'equalp))))

(defmethod initialize-instance :after ((emitter emitter) &key)
  (unless (owner emitter)
    (error "Emitter ~A has no owner." emitter))
  (reset emitter))

;; Generics
(defgeneric emit (emitter event &rest args)
  (:documentation "Emit `event' on the `emitter', invoking callbacks, if any, with `args'"))

(defgeneric add-listener (emitter event callback)
  (:documentation "Add a listener to `emitter' so when `event'
is emitted `callback' is called with any arguments that are emitted with the event."))
(defgeneric listen-once (emitter event callback)
  (:documentation "Add a listener to `emitter' so when `event'
is emitted `callback' is called with any arguments that are emitted. The callback
only fires once, then it is removed."))

(defgeneric remove-listener (emitter event callback)
  (:documentation "Remove a listener invoking `callback' from `emitter' from the chain for `event'."))
(defgeneric clear-listeners (emitter &optional event)
  (:documentation "remove either all listeners on `emitter' or just those bound to `event' if given."))

;; Methods
(defmethod emit ((emitter emitter) (event string) &rest args)
  "Enqueue the delivery of an `event' with `args'. The event will
be delivered at some point in the future, but very soon, in the order
that it was emitted relative to other events."
  (let ((registered (gethash event (listeners emitter))))
    (flet ((deliver (cb)
             (when (remhash cb (oneshots emitter))
               (remove-listener emitter event cb))
             (enqueue (events emitter) (lambda () (apply cb args)))))
      (mapc #'deliver registered))))

(defmethod emit :after ((emitter emitter) (event string) &rest args)
  "Register an interval to deliver the emitted events as they are fired
in the order that they are fired. When the queue is empty the delivery
timer goes to sleep until the next event is emitted."
  (declare (ignore args))
  (flet ((emitter-cb (&rest _)
           (declare (ignore _))
           (if-let (thunk (dequeue (events emitter)))
             (funcall thunk)
             (ev:stop-watcher (owner emitter) (event-runner emitter) :keep-callback t))))

    (unless (event-runner emitter)
      (setf (event-runner emitter) (let ((watcher (make-instance 'ev:ev-idle)))
                                     (prog1 watcher
                                       (ev:set-idle (owner emitter) watcher #'emitter-cb)))))

    (ev:start-watcher (owner emitter) (event-runner emitter))))

(defmethod add-listener ((emitter emitter) (event string) (callback symbol))
  (add-listener emitter event (symbol-function callback)))
(defmethod add-listener ((emitter emitter) (event string) (callback function))
  (prog1 emitter
    (let ((registered (gethash event (listeners emitter) (list))))
      (setf (gethash event (listeners emitter))
            (append registered (list callback)))
      emitter)))
(defmethod add-listener :after ((emitter emitter) (event string) (callback function))
  (emit emitter "new-listener" event callback))

(defmethod listen-once ((emitter emitter) (event string) (callback symbol))
  (listen-once emitter event (symbol-function callback)))
(defmethod listen-once ((emitter emitter) (event string) (callback function))
  (prog1 emitter
    (add-listener emitter event callback)
    (setf (gethash callback (oneshots emitter)) t)))

(defmethod remove-listener ((emitter emitter) (event string) (callback symbol))
  (remove-listener emitter event (symbol-function callback)))
(defmethod remove-listener ((emitter emitter) (event string) (callback function))
  (prog1 emitter
    (let ((registered (remove callback (gethash event (listeners emitter)))))
      (if registered
          (setf (gethash event (listeners emitter)) registered)
          (remhash event (listeners emitter))))))

(defmethod clear-listeners ((emitter emitter) &optional event)
  (prog1 emitter
    (if event
        (and (gethash event (listeners emitter))
             (remhash event (listeners emitter)))
        (reset emitter))))
