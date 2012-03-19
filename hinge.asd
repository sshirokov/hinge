(defpackage hinge-system
  (:use :asdf))
(in-package :hinge-system)

(defsystem :hinge
  :description "A synonym for node. Something like an evented framework in and for CL."
  :version "0.0.0"
  :depends-on (:log5 :usocket :ev)

  :components ((:module "src" :components
                        ((:file "package")
                         (:file "helpers" :depends-on ("package"))

                         ;; Basic reactor
                         (:file "hinge" :depends-on ("package" "helpers"))
                         (:file "generics" :depends-on ("package"))
                         (:file "methods" :depends-on ("generics" "hinge"))

                         ;; Event emitter
                         (:file "emitter" :depends-on ("hinge" "generics"))

                         ;; Network
                         (:file "socket" :depends-on ("emitter"))
                         (:file "server" :depends-on ("emitter" "helpers" "socket"))))))
