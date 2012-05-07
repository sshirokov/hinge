(ql:quickload :hinge)
(defpackage :hinge-example
  (:use :cl :hinge :hinge.http))
(in-package :hinge-example)

(let ((server (make-instance 'http-server)))
  (add-listener "request"
                (lambda (request)
                  (format t "Request: ~S~%" request)
                  (describe request)))
  (bind server 4545))

(run :default)
