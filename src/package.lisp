(defpackage :hinge
  (:use :cl :log5)

  (:import-from :alexandria
                :appendf)

  (:export :output
           :*hinge*))

(in-package :hinge)
(defcategory output)

