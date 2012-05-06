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
           :*hinge*))

(in-package :hinge)
(defcategory output)
