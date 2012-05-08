(defpackage :hinge.http
  (:use :cl :log5 :hinge)

  (:import-from :alexandria
                :curry
                :if-let)

  (:export :http-server

           :http-request
           :http-method
           :resource
           :version
           :headers
           :body

           :http-response
           :request
           :status-code
           :status-reason
           :headers

           :write-head
           :set-headers
           :header
           :send
           :end))


(in-package :hinge.http)
