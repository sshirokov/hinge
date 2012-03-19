(in-package :hinge)

;; Class
(defclass server (emitter)
  ((acceptor :initform (vector nil nil)
             :accessor acceptor
             :documentation "The listen socket of the server and the watcher for it as a 2-vector.")
   (peers :initform (list)
          :accessor peers
          :documentation "Sockets of any accepted peers.")))

;; Generics -- User API
(defgeneric bind (server port &optional host)
  (:documentation "Start listening for incoming connections for this `server' on the
given `port' and `host', if given. If `host' the server will listen on any interface."))
(defgeneric connection (server)
  (:documentation "Called when a new connection is signaled on `server'.
The connection is accepted and emitted with the `connection' event."))

;; Methods
(defmethod bind ((server server) (port number) &optional (host sockets:+ipv4-unspecified+))
  (let* ((sock (sockets:make-socket :ipv6 nil :connect :passive
                                    :local-host host
                                    :local-port port
                                    :reuse-address t))
         (watcher (make-instance 'ev:ev-io-watcher)))

    (ev:set-io-watcher *hinge* watcher (socket-fd sock) ev:EV_READ
                       #'(lambda (l w e)
                           (declare (ignore l w e))
                           (connection server)))
    (ev:start-watcher *hinge* watcher)

    (setf (sock-of (acceptor server)) sock
          (watcher-of (acceptor server)) watcher)

    (prog1 server
      (emit server "listening" server))))

(defmethod connection ((server server))
  (let* ((peer-sock (sockets:accept-connection (sock-of (acceptor server)) :wait t))
         (hinge-sock (make-instance 'socket :sock peer-sock)))
    (push hinge-sock (peers server))
    (prog1 server
      (emit server "connection" hinge-sock))))

(defmethod close ((server server) &key abort)
  "Close the accepting socket to prevent further connections
from arriving."
  (declare (ignore abort))
  (when (acceptor server)
    (ev:stop-watcher *hinge* (watcher-of (acceptor server)))
    (close (socket-of (acceptor server)))
    (emit server "close" server)))
