(ql:quickload :hinge)
(defpackage :hinge-example
  (:use :cl :hinge :hinge.http))
(in-package :hinge-example)

(let ((server (make-instance 'http-server)))
  (add-listener server "request"
                (lambda (request)
                (format t "Request: ~S~%" request)
                (describe request)

                (send (sock (peer request))
                      (babel:string-to-octets
                       (let ((reply "Hello world"))
                         (concatenate 'string
                                      (format nil "HTTP/1.1 200 OK~A~A" #\Return #\Newline)
                                      (format nil "Connection: close~A~A" #\Return #\Newline)
                                      (format nil "Content-Length: ~A~A~A" (length reply) #\Return #\Newline)
                                      (format nil "~A~A~A" #\Return #\Newline reply))))
                      (lambda (sock) (close sock)))))
  (bind server 4545))

(run :default)
