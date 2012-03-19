(in-package :hinge)

;; Class
(defclass emitter ()
  ((oneshots :accessor oneshots)
   (listeners :accessor listeners)))

(defgeneric reset (emitter)
  (:documentation "Reset the emitter to its empty initial state.")
  (:method ((emitter emitter))
    (setf (oneshots emitter) (make-hash-table)
          (listeners emitter) (make-hash-table :test 'equalp))))

(defmethod initialize-instance :after ((emitter emitter) &key)
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
  (let ((registered (gethash event (listeners emitter))))
    (flet ((deliver (cb)
             (when (remhash cb (oneshots emitter))
               (remove-listener emitter event cb))
             (set-timeout 0 #'(lambda () (apply cb args)))))
      (mapc #'deliver registered))))

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
