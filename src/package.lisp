(defpackage :hinge
  (:use :cl :log5)

  (:import-from :alexandria
                :with-gensyms
                :flatten
                :when-let
                :if-let
                :appendf)

  (:import-from :arnesi
                :queue
                :enqueue
                :dequeue)

  (:export :output
           :*hinge*))

(in-package :hinge)
(defcategory output)
