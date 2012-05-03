(in-package :hinge)

;; Class
(defclass socket (emitter)
  ((sock :initform (sockets:make-socket :ipv6 nil :reuse-address t)
         :initarg :sock
         :accessor sock)
   (watchers :initform (vector nil nil nil)
             :accessor watchers
             :documentation "#(read write timeout) watchers.")
   (writes :initform (list)
           :accessor writes
           :documentation "List of 3-vectors in the format
#(buffer offset callback) of pending write operations. When possible
write operations are performed from the head of the list. Each write
a portion of the buffer will be written and a new offset into the
buffer is stored. If the entire buffer is sent, callback is invoked,
if provided. When the writes list becomes empty a \"drain\" event
is emitted.")

   (fd :initform nil
       :initarg :fd
       :accessor fd
       :documentation "File descriptor of the socket.")))

;; Generics
(defgeneric init-watchers (socket)
  (:documentation "Initialize the watchers of the `socket'
and add them to the reactor. If any previous watchers exist
they are first disposed."))

(defgeneric on-read (socket)
  (:documentation "Fired to handle a ready for read operation on the socket."))
(defgeneric on-write (socket)
  (:documentation "Fired to handle a ready for write operation on this socket."))

(defgeneric pause (socket)
  (:documentation "Pause the incoming data events."))
(defgeneric resume (socket)
  (:documentation "Resume the incoming data events."))

(defgeneric connect (socket port &optional host)
  (:documentation "Connect `socket' to `port' on `host'.
If host is omitted localhost is assumed."))
(defgeneric send (socket data &optional callback)
  (:documentation "Schedule a write of `data' on `socket'.
`callback' is invoked when the data is written as in:
 (funcall callback socket offset data-written).
\"drain\" will be emitted on the socket when the write operation
completes."))

;; Interface methods
(defmethod timeout-activity ((socket socket) &optional timeout start)
  "Signal activity on the `socket' to reschedule the timeout into the
future, if one exists. If `timeout' is given, it is set to be the new
timeout interval to use. Unless the timer is already active, or `start'
is non-nil, the timer is not restarted."
  (when (svref (watchers socket) 2)
    (when timeout
      (cffi:with-foreign-slots ((ev::repeat) (ev::ev-pointer (svref (watchers socket) 2)) ev::ev_timer)
        (setf ev::repeat (coerce timeout 'double-float))))

    (when (or start (ev:watcher-active-p (svref (watchers socket) 2)))
      (ev::ev_timer_again (ev::event-loop (owner socket)) (ev::ev-pointer (svref (watchers socket) 2))))))

(defmethod set-timeout ((socket socket) (timeout number) callback)
  "Set an inactivity timeout on the socket `socket' of `timeout' seconds.
Callback must be specified, but can be `nil'. The `callback' parameter
does not affect the emission of the \"timeout\" event."
  (unless (svref (watchers socket) 2)
    (flet ((timeout-cb (l watcher e)
             (declare (ignore e))
             (ev:stop-watcher l watcher :keep-callback t)
             (emit socket "timeout" socket)))

      (let ((watcher (make-instance 'ev:ev-timer)))
        (ev:set-timer (owner socket) watcher #'timeout-cb (coerce timeout 'double-float))
        (setf (svref (watchers socket) 2) watcher))))

  (if (zerop timeout)
      (ev:stop-watcher (owner socket) (svref (watchers socket) 2) :keep-callback t)
      (timeout-activity socket timeout t))

  socket)

(defmethod connect ((socket socket) (port number) &optional (host #(127 0 0 1)))
  (async (:hinge (owner socket)
          :success (curry #'emit socket "connect")
          :failure (curry #'emit socket "error"))
    (sockets:connect (sock socket) (sockets:make-address host) :port port)
    socket)
  socket)


(defmethod send ((socket socket) (data sequence) &optional (callback (lambda (sock) (declare (ignore sock)))))
  (let ((watcher (svref (watchers socket) 1)))
    (appendf (writes socket)
             (list (vector data 0 callback)))
    (when (zerop (ev::ev_is_active (ev::ev-pointer watcher)))
      (ev:start-watcher (owner socket) watcher))))

(defmethod close ((socket socket) &key &allow-other-keys)
  "Close the actual socket before the close method cleans up the watchers
and emits the event."
  (close (sock socket)))

(defmethod close :after ((socket socket) &key &allow-other-keys)
  "Close the socket, emit \"close\" event."
  (ev:stop-watcher (owner socket) (svref (watchers socket) 0))
  (ev:stop-watcher (owner socket) (svref (watchers socket) 1))
  (when (svref (watchers socket) 2)
    (ev:stop-watcher (owner socket) (svref (watchers socket) 2)))
  (emit socket "close" socket))

;; Event methods
(defmethod on-read :after ((socket socket))
  "Signal timeout activity"
  (timeout-activity socket))

(defmethod on-write :after ((socket socket))
  "Signal timeout activity"
  (timeout-activity socket))

(defmethod send :after ((socket socket) _ &optional __)
  "Signal timeout activity"
  (declare (ignore _ __))
  (timeout-activity socket))

(defmethod on-read ((socket socket))
  (if (sockets:socket-open-p (sock socket))
      (when (sockets:socket-connected-p (sock socket))
        (handler-case
            (multiple-value-bind (data size)
                (sockets:receive-from (sock socket) :size (* 8 1024) :dont-wait t)
              (if (zerop size)
                  (close socket)
                  (emit socket "data" (subseq data 0 size))))
          (iolib.syscalls:ewouldblock () nil)))
      (close socket)))

(defmethod on-write ((socket socket))
  (let ((data (first (writes socket))))
    (if data
        (let* ((buffer (svref data 0))
               (start (svref data 1))
               (callback (svref data 2))
               (written (sockets:send-to (sock socket) buffer :start start :dont-wait t)))
          (when (= (incf (svref data 1) written) (length buffer))
            (pop (writes socket))
            (defer ((owner socket))
              (funcall callback socket))))

        (progn
          (format t "Socket drained: ~A~%" socket)
          (ev:stop-watcher (owner socket) (svref (watchers socket) 1) :keep-callback t)
          (emit socket "drain" socket)))))

(defmethod pause ((socket socket))
  (prog1 socket
    (when (ev:watcher-active-p (svref (watchers socket) 0))
      (ev:stop-watcher (owner socket) (svref (watchers socket) 0) :keep-callback t))
    (when (ev:watcher-active-p (svref (watchers socket) 1))
      (ev:stop-watcher (owner socket) (svref (watchers socket) 1) :keep-callback t))))

(defmethod resume ((socket socket))
  (prog1 socket
    (ev:start-watcher (owner socket) (svref (watchers socket) 0))
    (ev:start-watcher (owner socket) (svref (watchers socket) 1))))

;; Init Methods
(defmethod initialize-instance :after ((inst socket) &key)
  (setf (fd inst) (or (fd inst) (socket-fd (sock inst))))
  (init-watchers inst))

(defmethod init-watchers :before ((socket socket))
  (when (svref (watchers socket) 0) ;; Reader watcher
    (ev:stop-watcher (owner socket) (svref (watchers socket) 0))
    (setf (svref (watchers socket) 0) nil))

  (when (svref (watchers socket) 1) ;; Writer watcher
    (ev:stop-watcher (owner socket) (svref (watchers socket) 1))
    (setf (svref (watchers socket) 1) nil))

  (when (svref (watchers socket) 2) ;; Timeout watcher
    (ev:stop-watcher (owner socket) (svref (watchers socket) 2))
    (setf (svref (watchers socket) 2) nil)))

(defmethod init-watchers ((socket socket))
  (let ((read-watcher (make-instance 'ev:ev-io-watcher)))
    (ev:set-io-watcher (owner socket) read-watcher (fd socket) ev:EV_READ
                       #'(lambda (ev watcher events)
                           (declare (ignore ev watcher events))
                           (on-read socket)))
    (ev:start-watcher (owner socket) read-watcher)
    (setf (svref (watchers socket) 0) read-watcher))

  (let ((write-watcher (make-instance 'ev:ev-io-watcher)))
    (ev:set-io-watcher (owner socket) write-watcher (fd socket) ev:EV_WRITE
                       #'(lambda (ev watcher events)
                           (declare (ignore ev watcher events))
                           (on-write socket)))
    (unless (null (writes socket))
      (ev:start-watcher (owner socket) write-watcher))
    (setf (svref (watchers socket) 1) write-watcher)))
