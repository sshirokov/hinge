;; Pause/Resume sockets.
;; This server sets up a set of sockets in an event machine.
;; On the `*feed-addr*', one socket will bind and consider itself the consumer.
;; it will wait for messages to arrive on the wire, and echo them to the terminal
;; as strings with a timestamp.
;;
;; A second socket will connect as a producer, and every second will send the
;; universal time as a string to the `*feed-addr*' address, notifying the operator of any failures.
;;
;; A third socket will listen on the `*command-addr*' and expect one of two strings:
;; "pause" or "resume" and will invoked the same-named method on the feed-consuming socket.
;;
;; The pause and resume methods will pause the arrival of additional data events on the socket,
;; preventing it from doing any IO during the interval.
;;
;;; Server setup
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
                (send *feed-producer* (princ-to-string (get-universal-time))
                      (lambda (sock)
                        (format t "Failed to write a feed number: ~S~%" (get-universal-time))))))

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

;; Run the event loop
(run :default)

;;; Driver
;;;; This should be evaluated in a separate shell
;;;; or interactively while the above server is running
(ql:quickload :hinge)
(in-package :hinge)

(defun send-command (command)
  (zmq:with-context (ctx 1)
    (zmq:with-socket (s ctx :push)
      (zmq:connect s *command-addr*)
      (zmq:send! s command))))

;; Evaluating this should force the server to stop echoing back data
;; to the terminal, but it should continue to produce data for itself
;; without incident.
(send-command "pause")

;; Once this command is received, IO is resumed. You should see
;; the backlog of messages delivered earlier appear as data events on the wire.
;; in rapid succession, then again proceed lockstep with the sends.
(send-command "resume")
