;; Starts a server that accepts TCP connections on port 4545
;; and listens for commands "PING" and "DIE"
;; PING is replied to with PONG, and the connection is maintained
;; DIE disconnects everyone and should gracefully shut down the server
;; any other command causes the peer to disconnect
;;
;; Original application of this code was to aid in the development of the socket class
(ql:quickload :hinge)
(in-package :hinge)

(defparameter *server* (make-instance 'server))
(defparameter *client* (make-instance 'socket))

(add-listener *server* "connection"
              (lambda (peer)
                (format t "New client: ~A~%" peer)
                (add-listener peer "data"
                              (lambda (data)
                                (let ((data-str (string-right-trim '(#\return #\linefeed #\space)
                                                                   (babel:octets-to-string data))))
                                  (cond ((string= "DIE" data-str)
                                         (format t "Asked to die.~%")
                                         (close peer)
                                         (close *server*)
                                         (close *client*))
                                        ((string= "PING" data-str)
                                         (format t "Ponging ~A~%" peer)
                                         (send peer (babel:string-to-octets "PONG")))
                                        (t
                                         (format t "Unknown request, booting ~A: ~S~%" peer data-str)
                                         (send peer (babel:string-to-octets "Invalid request!")
                                               (lambda (sock)
                                                 (close sock))))))))

                (add-listener peer "close"
                              (lambda (peer)
                                (format t "~A Left.~%" peer)))))



(add-listener *client* "connect"
              (lambda (sock)
                (let ((pinger (set-interval (owner sock) 5
                                            (lambda ()
                                              (format t "Pinging!~%")
                                              (send sock (babel:string-to-octets "PING"))))))
                  (add-listener sock "close"
                                (lambda (s)
                                  (format t "Stopping the pinger.~%")
                                  (clear (owner sock) pinger))))))

(add-listener *client* "error"
              (lambda (c)
                (format t "Socket ~S error: ~S~%" *client* c)
                (describe c)))


;; Bind the server
(bind *server* 4545)
(format t "Bound ~S.~%" *server*)
;; Connect the client
(connect *client* 4545)

;; Run the event loop
(run :default)
