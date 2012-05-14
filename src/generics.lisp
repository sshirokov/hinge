(in-package :hinge)

;; Generics
(defgeneric run (hinge)
  (:documentation "Run the event loop."))

(defgeneric set-timeout (hinge timeout callback)
  (:documentation "Creates and registers a `callback' of no arguments
to be invoked after `timeout' elapses."))
(defgeneric set-interval (hinge interval callback)
  (:documentation "Crates and registers a `callback' of no arguments
to be invoked every `timeout'"))
(defgeneric clear (hinge handle)
  (:documentation "Clear the registration of a watcher (e.g. timeout or interval) named by `handle'"))

(defgeneric queue-work (hinge work &optional queue)
  (:documentation "Queue a thunk, `work', into a work queue on
`hinge'. If no `queue' is named, `:low' should be used."))

;; Wrappers
(defmacro defer ((hinge) &body forms)
  "Enqueue work `forms' into the low priority queue of `hinge'"
  `(queue-work ,hinge (lambda () ,@forms) :low))
