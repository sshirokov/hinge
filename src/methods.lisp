(in-package :hinge)

;; Methods
(defmethod run ()
  "Run the event loop then bind a new event loop
instance then bind a new one after completion."
  (unwind-protect (ev:event-dispatch *hinge* nil)
    (setf *hinge* (make-instance 'hinge))))

(defmethod set-timeout (timeout (callback symbol))
  "Fetches the function value of `callback' and passes it down."
  (set-timeout timeout (symbol-function callback)))
(defmethod set-timeout ((timeout number) (callback function))
  "Typechecking and sanitizing wrapper to add a timeout callback."
  (flet ((timeout-fun (l w e)
           (declare (ignore l e))
           (clear w)
           (funcall callback)))
    (let ((timeout-cb (make-instance 'ev:ev-timer)))
      (ev:set-timer *hinge* timeout-cb #'timeout-fun (coerce timeout 'double-float))
      (ev:start-watcher *hinge* timeout-cb)
      timeout-cb)))

(defmethod set-interval (timeout (callback symbol))
  (set-interval timeout (symbol-function callback)))
(defmethod set-interval ((timeout number) (callback function))
  (flet ((timeout-fun (l w e)
           (declare (ignore l w e))
           (funcall callback)))
    (let ((timeout-cb (make-instance 'ev:ev-timer))
          (timeout (coerce timeout 'double-float)))
      (ev:set-timer *hinge* timeout-cb #'timeout-fun timeout :repeat timeout)
      (ev:start-watcher *hinge* timeout-cb)
      timeout-cb)))

(defmethod clear ((handle ev:ev-watcher))
  (ev:stop-watcher *hinge* handle))
