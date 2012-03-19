(defpackage hinge-system
  (:use :asdf))
(in-package :hinge-system)

(defsystem :hinge
  :description "A synonym for node. Something like an evented framework in and for CL."
  :version "0.0.0"
  :depends-on (:log5 :usocket :ev)

  :components ((:module "src" :components
                        ((:file "package")
                         (:file "hinge" :depends-on ("package"))
                         (:file "generics" :depends-on ("package"))
                         (:file "methods" :depends-on ("generics" "hinge"))
                         (:file "emitter" :depends-on ("hinge" "generics"))

                         (:file "server" :depends-on ("emitter"))))))
