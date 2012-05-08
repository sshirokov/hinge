(in-package :hinge.http)

(defclass http-request (emitter)
  ((peer :initarg :peer :accessor peer)
   (http-method :initarg :http-method :accessor http-method)
   (resource :initarg :resource :accessor resource)
   (version :initarg :version :accessor version)
   (headers :initarg :headers :accessor headers)
   (body :initarg :body :accessor body)))

(defclass http-response (emitter)
  ((request :initarg :request :accessor request)))
