(in-package :hinge)

;; Class
(defclass socket (emitter)
  ((sock :initform (sockets:make-socket :ipv6 nil :reuse-address t)
         :initarg :sock
         :accessor sock)
   (watchers :initform (vector nil nil)
             :accessor watchers
             :documentation "#(read write) watchers.")
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
(defmethod connect ((socket socket) (port number) &optional (host #(127 0 0 1)))
  (prog1 socket
    (format t "Connect: ~A~%"
            (sockets:connect (sock socket) (sockets:make-address host) :port port))
    (emit socket "connect" socket)))

(defmethod send ((socket socket) (data sequence) &optional (callback (lambda (sock) (declare (ignore sock)))))
  (let ((watcher (svref (watchers socket) 1)))
    (appendf (writes socket)
             (list (vector data 0 callback)))
    (when (zerop (ev::ev_is_active (ev::ev-pointer watcher)))
      (ev:start-watcher (owner socket) watcher))))

(defmethod close ((socket socket) &key abort)
  "Close the socket, emit \"close\" event."
  (close (sock socket))
  (ev:stop-watcher (owner socket) (svref (watchers socket) 0))
  (ev:stop-watcher (owner socket) (svref (watchers socket) 1))
  (emit socket "close" socket))

;; Event methods
(defmethod on-read ((socket socket))
  (if (sockets:socket-open-p (sock socket))
      (multiple-value-bind (data size)
          (sockets:receive-from (sock socket) :size (* 8 1024) :dont-wait t)
        (if (zerop size)
            (close socket)
            (emit socket "data" (subseq data 0 size))))
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
    (setf (svref (watchers socket) 1) nil)))

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
