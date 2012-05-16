(in-package :hinge)

;; Methods
(defmethod initialize-instance :after ((hinge hinge) &key)
  "Remap the queues descriptions to an HT, then init the pool"
  (when-let (q-desc (and (listp (queues hinge)) (every #'consp (queues hinge))
                         (queues hinge)))
    (setf (queues hinge) (make-hash-table))
    (mapc #'(lambda (name-priority)
              (destructuring-bind (name . priority) name-priority
                (setf (gethash name (queues hinge))
                      (make-instance 'running-queue :owner hinge :priority priority))))
          q-desc))


  (setf (bg-pool hinge) (make-instance 'pool :owner hinge)))

(defmethod queue-work ((hinge hinge) work &optional (queue :low))
  "Enqueue the `work' thunk into the `queue' queue within `hinge'"
  (enqueue (gethash queue (queues hinge)) work))

(defmethod close ((hinge hinge) &key &allow-other-keys)
  (maphash #'(lambda (name queue)
               (declare (ignore name))
               (close queue))
           (queues hinge))
  (close (bg-pool hinge)))

(defmethod run ((hinge (eql :default)))
  "Run the event loop then bind a new event loop
instance then bind a new one after completion."
  (unwind-protect (run (get-default-hinge))
    (get-default-hinge t)))

(defmethod run ((hinge hinge))
  "Run the event loop held by `hinge'"
  (unwind-protect (ev:event-dispatch hinge nil)
    (close hinge)))

(defmethod set-timeout (hinge timeout (callback symbol))
  "Fetches the function value of `callback' and passes it down."
  (set-timeout hinge timeout (symbol-function callback)))
(defmethod set-timeout ((hinge hinge) (timeout number) (callback function))
  "Typechecking and sanitizing wrapper to add a timeout callback."
  (flet ((timeout-fun (l w e)
           (declare (ignore l e))
           (clear hinge w)
           (funcall callback)))
    (let ((timeout-cb (make-instance 'ev:ev-timer)))
      (ev:set-timer hinge timeout-cb #'timeout-fun (coerce timeout 'double-float))
      (ev:start-watcher hinge timeout-cb)
      timeout-cb)))

(defmethod set-interval (hinge timeout (callback symbol))
  (set-interval hinge timeout (symbol-function callback)))
(defmethod set-interval ((hinge hinge) (timeout number) (callback function))
  (flet ((timeout-fun (l w e)
           (declare (ignore l w e))
           (funcall callback)))
    (let ((timeout-cb (make-instance 'ev:ev-timer))
          (timeout (coerce timeout 'double-float)))
      (ev:set-timer hinge timeout-cb #'timeout-fun timeout :repeat timeout)
      (ev:start-watcher hinge timeout-cb)
      timeout-cb)))

(defmethod clear ((hinge hinge) (handle ev:ev-watcher))
  (ev:stop-watcher hinge handle))
