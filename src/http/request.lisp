(in-package :hinge.http)

(defclass http-request (emitter)
  ((peer :initarg :peer :accessor peer)
   (http-method :initarg :http-method :accessor http-method)
   (resource :initarg :resource :accessor resource)
   (version :initarg :version :accessor version)
   (headers :initarg :headers :accessor headers)
   (body :initarg :body :accessor body)))

(defclass http-response (emitter)
  ((request :initarg :request :accessor request)

   (status-code :initform 200 :accessor status-code)
   (status-reason :initform "OK" :accessor status-reason)
   (headers :initform (list) :accessor headers)

   (status-sent :initform nil :accessor status-sent :accessor status-sent-p)
   (headers-sent :initform nil :accessor headers-sent-p :accessor headers-sent)

   (body :initform (vector) :accessor body)))

;; Response API
(defgeneric write-head (response status &optional reason/headers headers)
  (:documentation "Write the status reply `status' with `readon/headers' as
the reason, if it is a string, otherwise a stock reason for the given code.
If `reason/headers' is a list or `headers' is given a call to `set-headers'
is made, adding them to the response headers."))

(defgeneric set-headers (response headers)
  (:documentation "Update the outgoing headers on `response'
by adding the headers in the `headers' alist, replacing any
with equal values."))

(defgeneric set-header (response header value)
  (:documentation "Set a header key `header' to `value'
in the outgoing response headers of `response'. Existing
values are overwritten."))

(defgeneric header (response header)
  (:documentation "Get a header from `response' named by the key
`header'"))

;; Bind header/set-header as a setfable pair
(defsetf header set-header)

(defgeneric end (response &optional data)
  (:documentation "Finish the response. If `data' is given
`send' is called with the data first.

This method checks the `Connection' header to determine if
the socket should be closed once the response is written,
or if the parser machinery of the peer needs to be reloaded."))


;; Response methods
(defmethod send ((response http-response) data &optional callback)
  (declare (ignore callback))
  "Send a chunk to the client. If this is the first chunk
and there is no content length set, or no chunked encoding
header in the headers of the response a content-length header
is stored before the write. If a chunked encoding header is found
in the headers of the response the `data' is written as
an HTTP chunk.

If the response code or headers haven't been written yet, they are
written first."
  :TODO)
