(ql:quickload :hinge)
(defpackage :hinge-example
  (:use :cl :hinge :hinge.http))

(in-package :hinge-example)

(let ((server (make-instance 'http-server)))
  (add-listener server "request"
                (lambda (request response)
                  (declare (ignorable request))
                  (async (:success (lambda (r)
                                     (write-head response 200 '(("Content-Type" . "text/html")))
                                     (end response (format nil "Hello world! [~A]" r))))
                    (random 1024))))

  (bind server 4545))

(run :default)
