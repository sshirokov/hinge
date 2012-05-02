;; Setup
(ql:quickload :hinge)
(in-package :hinge)

(defparameter *ctx* (zmq:init 1))
(defparameter *sock* (make-instance 'zmq-socket :type :sub :context *ctx*))
(zmq:setsockopt (sock *sock*) :subscribe "")
(bind *sock* "ipc:///tmp/sock.command")

(add-listener *sock* "data"
              (lambda (data)
                (let* ((data-str (babel:octets-to-string data))
                       (data (eval (read-from-string data-str))))
                  (format t "Got some data: ~S~%" data)
                  (async (:success (lambda (result)
                                     (format t "Got: ~s in ~A~%" result (bt:current-thread)))
                          :failure (lambda (condition)
                                     (format t "Error: ~s in ~A~%" condition (bt:current-thread))))
                    (format t "Doing division in: ~A~%" (bt:current-thread))
                    (/ 1337.0 data)))))

(set-interval (get-default-hinge) 10
              (lambda () (format t "Still kicking: ~S~%" (get-universal-time))))

(run :default)


;; Command
;; -- Should be run in a separate thread or lisp process
;;    or interactively from a REPL while the above server is running
(ql:quickload :hinge)

(defun send-divisor (n)
  (zmq:with-context (ctx 1)
    (zmq:with-socket (pub ctx :pub)
      (zmq:connect pub "ipc:///tmp/sock.command")
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
