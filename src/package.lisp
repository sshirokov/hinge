(defpackage :hinge
  (:use :cl :log5)

  (:import-from :alexandria
                :curry
                :with-gensyms
                :flatten
                :when-let
                :if-let
                :appendf)

  (:import-from :arnesi
                :queue
                :queue-empty-p
                :enqueue
                :dequeue)

  (:export :output
           :hinge
           :*hinge*
           :get-default-hinge
           :run

           :set-interval
           :set-timeout
           :defer

           :pool
           :job
           :submit
           :async

           :emit
           :emitter
           :add-listener
           :listen-once
           :remove-listener

           :socket
           :sock
           :zmq-socket
           :server

           :connect
           :bind
           :send))



(in-package :hinge)
(defcategory output)
(defcategory debug)
