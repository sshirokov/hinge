(ql:quickload :hinge)
(defpackage :hinge-example
  (:use :cl :hinge :hinge.http))

(in-package :hinge-example)

(let ((server (make-instance 'http-server)))
  (add-listener server "request"
                (lambda (request response)
                  (declare (ignorable request))
                  (write-head response 200 '(("Content-Type" . "text/html")))
                  (send response "Hello chunk one!")
                  (send response (format nil "~%"))

                  (send response "Hello chunk two!")
                  (send response (format nil "~%"))

                  (end response
                       (format nil "Hello world!~%"))))

  (bind server 4545))

(run :default)
