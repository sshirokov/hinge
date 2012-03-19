(in-package :hinge)

;; Class
(defclass socket (emitter)
  ((sock :initform nil
         :initarg :sock
         :accessor sock)
   (watchers :initform (vector nil nil nil)
             :accessor watchers
             :documentation "#(read write error) watchers.")

   (fd :initform nil
       :initarg :fd
       :accessor fd
       :documentation "File descriptor of the socket.")))

;; Generics
(defgeneric init-watchers (socket)
  (:documentation "Initialize the watchers of the `socket'
and add them to the reactor. If any previous watchers exist
they are first disposed."))

(defgeneric connect (socket port &optional host)
  (:documentation "Connect `socket' to `port' on `host'.
If host is omitted localhost is assumed."))
(defgeneric send (socket data &optional callback)
  (:documentation "Schedule a write of `data' on `socket'.
`callback' is invoked when the data is written as in:
 (funcall callback socket offset data-written).
\"drain\" will be emitted on the socket when the write operation
completes."))
(defgeneric end (socket)
  (:documentation "Close the `socket'."))



;; Methods
(defmethod initialize-instance :after ((inst socket) &key)
  (setf (fd inst) (or (fd inst) (socket-fd (sock inst))))
  (init-watchers inst))

(defmethod init-watchers :before ((socket socket))
  (when (svref (watchers socket) 0) ;; Reader watcher
    (ev:stop-watcher *hinge* (svref (watchers socket) 0))
    (setf (svref (watchers socket) 0) nil))

  (when (svref (watchers socket) 1) ;; Writer watcher
    (ev:stop-watcher *hinge* (svref (watchers socket) 1))
    (setf (svref (watchers socket) 1) nil))

  (when (svref (watchers socket) 2) ;; Error watcher
    (ev:stop-watcher *hinge* (svref (watchers socket) 2))
    (setf (svref (watchers socket) 2) nil)))

(defmethod init-watchers ((socket socket))
  (let ((read-watcher (make-instance 'ev:ev-io-watcher)))
    (ev:set-io-watcher *hinge* read-watcher (fd socket) ev:EV_READ
                       #'(lambda (ev watcher events)
                           (let* ((data (make-array (* 8 1024) :initial-element nil))
                                  (got (read-sequence data (usocket:socket-stream (sock socket)))))
                             (emit socket "data" (subseq data 0 got)))))

    (ev:start-watcher *hinge* read-watcher)
    (setf (svref (watchers socket) 0) read-watcher)))
