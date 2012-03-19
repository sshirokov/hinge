(defpackage :hinge
  (:use :cl :log5)

  (:export :output
           :*hinge*))

(in-package :hinge)
(defcategory output)

;; Classes
(defclass hinge (ev:ev-loop)
  ())

;; Dynamics
(defvar *hinge* (make-instance 'hinge) "The current instance of the event reactor.")

;; Generics
(defgeneric run ()
  (:documentation "Run the event loop."))

(defgeneric set-timeout (timeout callback)
  (:documentation "Creates and registers a `callback' of no arguments
to be invoked after `timeout' elapses."))
(defgeneric set-interval (interval callback)
  (:documentation "Crates and registers a `callback' of no arguments
to be invoked every `timeout'"))

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
           (ev:stop-watcher *hinge* w)
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
