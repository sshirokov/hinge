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

;; Wrappers
(defmacro defer ((hinge) &body forms)
  `(enqueue (defer-queue ,hinge)
            (lambda () ,@forms)))
