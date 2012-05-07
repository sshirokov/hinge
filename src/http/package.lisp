(defpackage :hinge.http
  (:use :cl :hinge)
  (:export :http-server))
(in-package :hinge.http)

(defclass http-server (server)
  ())

(defclass http-peer ()
  ((socket :initarg :socket
           :accessor sock)))

(defmethod initialize-instance :after ((server http-server) &key)
  (add-listener server "connection"
                (lambda (client)
                  (make-instance 'http-peer :socket client))))

(defmethod initialize-instance :after ((peer http-peer) &key)
  (add-listener (sock peer) "data"
                (lambda (data)
                  (format t "Data: ~S~%" (babel:octets-to-string data))))

  (add-listener (sock peer) "close"
                (lambda (_)
                  (format t "Peer ~S left~%" peer))))
