(in-package :hinge)

(defclass zmq-socket (socket)
  ((context :initarg :context
            :accessor context)
   (sock-type :initarg :type
              :accessor sock-type))

  (:default-initargs . (:sock nil)))

;; Init
(defmethod initialize-instance :after ((zsock zmq-socket) &key)
  "Create a socket on the class if one did not exist before.
Intentionally trips on undefined `context' or `sock-type' if
they are used."
  (setf (sock zsock)
        (or (sock zsock)
            (zmq:socket (context zsock) (sock-type zsock)))))

;; API
(defmethod connect ((zsock zmq-socket) (spec string) &optional host)
  "Connect the socket `zsock' to the ZMQ endpoint declared by `spec'. The `host'
parameter is ignored to remain API compatible with the `socket' class."
  (declare (ignore host))
  (prog1 zsock
    (zmq:connect (sock zsock) spec)))

(defmethod bind ((zsock zmq-socket) (spec string) &optional host)
  "Bind the socket `zsock' to the ZMQ endpoint declared by `spec'. The `host'
parameter is ignored to remain API compatible with the `server' class."
  (declare (ignore host))
  (prog1 zsock
    (zmq:bind (sock zsock) spec)))

(defmethod close :before ((zsock zmq-socket) &key &allow-other-keys)
  (zmq:close (sock zsock)))

(defmethod send ((zsock zmq-socket) data &optional (when-block-fn (lambda (zsock) (declare (ignore zsock)))))
  "Attempt to send the data `data' to the socket, if the send operation would block
the callback `when-block-fn' will be invoked with the socket instance."
  (handler-case (zmq:send! (sock zsock) data '(:noblock))
    (zmq:eagain-error ()
      (funcall when-block-fn zsock))))

;; Hooks
(defmethod on-read ((zsock zmq-socket))
  (emit zsock "data" (zmq:recv! zsock :array)))

(defmethod on-write ((zsock zmq-socket))
  (format t "TODO: What do I do when ~A is ready for write!?~%" zsock)
  :TODO)

