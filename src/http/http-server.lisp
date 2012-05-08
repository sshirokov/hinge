(in-package :hinge.http)

;; Server
(defclass http-server (server)
  ())

(defmethod initialize-instance :after ((server http-server) &key)
  (add-listener server "connection"
                (lambda (client)
                  (make-instance 'http-peer :server server :socket client))))

;; HTTP Peer
(defclass http-peer (emitter)
  ((server :initarg :server
           :accessor server)
   (socket :initarg :socket
           :accessor sock)
   (parser :accessor parser)))

(defmethod initialize-instance :after ((peer http-peer) &key)
  (setf (parser peer) (make-instance 'request-parser :peer peer))

  (add-listener peer "request"
                (lambda (parser)
                  (let* ((request (make-instance 'http-request :peer peer
                                                 :http-method (http-method (request-fsm parser))
                                                 :resource (resource (request-fsm parser))
                                                 :version (version (request-fsm parser))
                                                 :headers (headers (headers-fsm parser))
                                                 :body (body (body-fsm parser))))
                         (response (make-instance 'http-response :request request)))
                    (when (string-equal (cdr (assoc "Connection" (headers request) :test #'string-equal))
                                        "close")
                      (setf (header response "Connection") "close"))
                    (emit (server peer) "request" request response))))

  (add-listener peer "error"
                (lambda (e)
                  (format t "Error: ~S~%" e)
                  (close (sock peer))))

  (add-listener (sock peer) "data"
                (lambda (data)
                  (format t "Request data: ~S~%" (babel:octets-to-string data))
                  (funcall (parser peer) data)
                  (format t "Parser state: ~S~%" (state (parser peer)))))

  (add-listener (sock peer) "close"
                (lambda (_)
                  (declare (ignore _))
                  (format t "Peer ~S left~%" peer))))
