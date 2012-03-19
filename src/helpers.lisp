(in-package :hinge)

;; Helper
(defun socket-fd (socket)
  "Translate a heavy `socket' object to a numeric file descriptor."
  (sockets:socket-os-fd socket))

(defmacro sock-of (place) `(svref ,place 0))
(defmacro watcher-of (place) `(svref ,place 1))
