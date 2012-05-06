;; This demonstrates the same server as the one in `examples/async.lisp'
;; but using external symbols in a separate package.
;;
;; The driver below should be evaluated once the server is running
;;
;; Server setup
;;;;;;;;;;;;;;;
(ql:quickload :hinge)
(defpackage :hinge-example
  (:use :cl :hinge))
(in-package :hinge-example)

(defparameter *ctx* (zmq:init 1))
(defparameter *addr* "ipc:///tmp/sock.command")

(let ((sock (make-instance 'zmq-socket :type :sub :context *ctx*)))
  (zmq:setsockopt (sock sock) :subscribe "")
  (bind sock *addr*)

  (add-listener sock "data"
                (lambda (data)
                  (let ((data (eval (read-from-string (babel:octets-to-string data)))))
                    (format t "Got some data: ~S~%" data)
                    (async (:success (lambda (result)
                                       (format t "Got: ~s in ~A~%" result (bt:current-thread)))
                            :failure (lambda (condition)
                                       (format t "Error: ~s in ~A~%" condition (bt:current-thread))))
                      (format t "Doing division in: ~A~%" (bt:current-thread))
                      (/ 1337.0 data))))))

(set-interval (get-default-hinge) 10
              (lambda () (format t "Still kicking: ~S~%" (get-universal-time))))

(run :default)


;;;;;;;;;;;;
;; Driver ;;
;;;;;;;;;;;;
(defun send-divisor (n)
  (zmq:with-context (ctx 1)
    (zmq:with-socket (pub ctx :pub)
      (zmq:connect pub *addr*)
      (zmq:send! pub (prin1-to-string n)))))

;; Perform a legal asynchronous division.
;;
;; The server should output a series of lines that look like the following,
;; potentially with some additional debug output as the `:success' callback
;; is invoked.
;;   Doing division in: #<THREAD "pool-worker[0]" RUNNING {10057F6AD3}>
;;   Got: 668.5 in #<THREAD "repl-thread" RUNNING {1003769163}>
(send-divisor 2)

;; Perform an illegal asynchronous division, resulting in an error.
;;
;; The server should output similar lines as before, except this time
;; you should note that the `:failure' callback is invoked with the
;; signaled condition:
;;   Doing division in: #<THREAD "pool-worker[1]" RUNNING {10058007C3}>
;;   Error: #<DIVISION-BY-ZERO {1006EE6553}> in #<THREAD "repl-thread" RUNNING {1003769163}>
(send-divisor 0)
