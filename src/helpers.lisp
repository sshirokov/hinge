(in-package :hinge)

;; Helper
(defgeneric socket-fd (socket)
  (:documentation "Translate a heavy `socket' object to a numeric file descriptor.")
  (:method ((socket sockets:socket))
    (sockets:socket-os-fd socket))
  (:method (socket)
    nil))

(defmacro sock-of (place) `(svref ,place 0))
(defmacro watcher-of (place) `(svref ,place 1))
