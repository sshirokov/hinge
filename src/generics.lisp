(in-package :hinge)

;; Generics
(defgeneric run ()
  (:documentation "Run the event loop."))

(defgeneric set-timeout (timeout callback)
  (:documentation "Creates and registers a `callback' of no arguments
to be invoked after `timeout' elapses."))
(defgeneric set-interval (interval callback)
  (:documentation "Crates and registers a `callback' of no arguments
to be invoked every `timeout'"))
(defgeneric clear (handle)
  (:documentation "Clear the registration of a watcher (e.g. timeout or interval) named by `handle'"))

;; Wrappers
(defmacro defer (&body forms)
  "Defer execution of `forms' to later, but very soon."
  `(set-timeout 0 (lambda () ,@forms)))
