(ql:quickload :hinge)
(in-package :hinge)

(defparameter *ctx* (zmq:init 1))
(defparameter *command-addr* "ipc:///tmp/sock.command")
(defparameter *feed-addr* "ipc:///tmp/sock.feed")


(defparameter *command* (make-instance 'zmq-socket :type :pull :context *ctx*))
(bind *command* *command-addr*)

(defparameter *feed-reader* (make-instance 'zmq-socket :type :pull :context *ctx*))
;(zmq:setsockopt (sock *feed-reader*) :subscribe "")
(bind *feed-reader* *feed-addr*)

(defparameter *feed-producer* (make-instance 'zmq-socket :type :push :context *ctx*))
(connect *feed-producer* *feed-addr*)

;; Feed Producer interval
(set-interval (get-default-hinge) 1
              (lambda ()
                (format t "Writing data out at ~S.~%" (get-universal-time))
                (send *feed-producer* (princ-to-string (get-universal-time))
                      (lambda (sock)
                        (format t "Failed to write a feed number: ~S~%" (get-universal-time))))
                (format t "Submitted write at ~S~%" (get-universal-time))))

;; Feed consumer
(add-listener *feed-reader* "data"
              (lambda (data)
                (let ((s-data (babel:octets-to-string data)))
                  (format t "Got data ~S at ~S~%" s-data (get-universal-time)))))

;; Command socket
(add-listener *command* "data"
              (lambda (data)
                (let ((s-data (babel:octets-to-string data)))
                  (format t "Got command: ~S~%" s-data)
                  (when (string= s-data "pause")
                    (format t "=> Pausing.~%")
                    (pause *feed-reader*))
                  (when (string= s-data "resume")
                    (format t "=> Resuming.~%")
                    (resume *feed-reader*)))))
