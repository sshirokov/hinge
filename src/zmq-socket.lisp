(in-package :hinge)

(defclass zmq-socket (socket)
  ((context :initarg :context
            :accessor context)
   (sock-type :initarg :type
              :accessor sock-type))

  (:default-initargs . (:sock nil)))

;; Init
(defmethod init-watchers :before ((zsock zmq-socket))
  (setf (sock zsock) (or (sock zsock)
                         (zmq:socket (context zsock) (sock-type zsock)))
        (fd zsock) (zmq:getsockopt (sock zsock) :fd)))

;; API
(defmethod connect ((zsock zmq-socket) (spec string) &optional host)
  "Connect the socket `zsock' to the ZMQ endpoint declared by `spec'. The `host'
parameter is ignored to remain API compatible with the `socket' class."
  (declare (ignore host))
  (prog1 zsock
    (zmq:connect (sock zsock) spec)
    (emit zsock "connect" zsock)))

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
  (flet ((get-msg ()
           (handler-case (zmq:recv! (sock zsock) :array '(:noblock))
             (zmq:eagain-error () nil))))

    (when (member :pollin (handler-case (zmq:getsockopt (sock zsock) :events)
                            (zmq:einval-error () nil)))
      (do ((msg (get-msg) (get-msg)))
          ((not msg) :done)
        (emit zsock "data" msg)))))

(defmethod on-write ((zsock zmq-socket))
  "The ZMQ socket should never be notified of write through libev.
Blocking write failures should be handled with the `send' failure callback."
  (format t "WARNING: The `on-write' fired for a zmq-socket: ~S~%" zsock)
  :noop)
