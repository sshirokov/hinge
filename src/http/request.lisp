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

(defgeneric write-headers (response &optional headers)
  (:documentation "Add any `headers' to the response headers
of `response' then write the header list to the client."))

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
(defmethod write-head ((response http-response) status &optional reason/headers headers)
  (let ((reason (typecase reason/headers
                  (string reason/headers)
                  (otherwise "OK")))
        (headers (typecase reason/headers
                   (list reason/headers)
                   (otherwise (or headers ())))))

    (send (sock (peer (request response)))
          (babel:string-to-octets
           (format nil "~A ~A ~A~A~A" (version (request response)) status reason #\Return #\Newline)))
    (setf (status-sent response) t)

    (when headers
      (set-headers response headers))))

(defmethod write-headers ((response http-response) &optional headers)
  (when headers
    (set-headers response headers))

  (mapc #'(lambda (header-pair)
            (destructuring-bind (key . value) header-pair
              (send (sock (peer (request response)))
                    (babel:string-to-octets
                     (format nil "~A: ~A~A~A" key value #\Return #\Newline)))))
        (headers response))
  (send (sock (peer (request response)))
                    (babel:string-to-octets
                     (format nil "~A~A" #\Return #\Newline)))
  (setf (headers-sent response) t))

(defmethod set-headers ((response http-response) headers)
  (mapc #'(lambda (header-pair)
            (destructuring-bind (key . value) header-pair
              (set-header response key value)))
        headers))

(defmethod set-header ((response http-response) (key symbol) value)
  (set-header response (symbol-name key) value))
(defmethod set-header ((response http-response) (key string) value)
  (if-let (header (assoc key (headers response) :test #'string-equal))
    (rplacd header value)
    (push (cons key value) (headers response))))

(defmethod header ((response http-response) (key symbol))
  (header response (symbol-name key)))
(defmethod header ((response http-response) (key string))
  (cdr (assoc key (headers response) :test #'string-equal)))

(defmethod begin ((response http-response))
  (unless (status-sent-p response)
    (write-head 200 "OK"))

  (unless (headers-sent-p response)
    (write-headers response)))

(defmethod send ((response http-response) data &optional callback)
  (declare (ignore callback))
  "Send a chunk to the client.

If this is the first chunk and there is no content length set,
or no chunked encoding set in the headers of the response a
content-length header is stored before the write.

If a chunked encoding header is found in the headers of the response
the `data' is written as an HTTP chunk.

No verification is made that the `data' does not exceed the advertised
content length if one is present.

If the response code or headers haven't been written yet, they are
written first."
  (let ((bindata (if (stringp data) (babel:string-to-octets data) data)))
    (unless (or (header response "Content-Length")
                (string-equal (header response "Transfer-Encoding") "chunked"))
      (setf (header response "Content-Length") (length bindata)))

    (begin response)

    (if (string-equal (header response "Transfer-Encoding") "chunked")
        (format t "TODO: Chunked encoding is not supported")
        (send (sock (peer (request response)))
              bindata))))

(defmethod end ((response http-response) &optional data)
  (if data
      (send response data)
      (begin response))

  (if (string-equal (header response "Connection") "close")
      (add-listener (sock (peer (request response))) "drain"
                    (lambda (sock)
                      (format t "~S Closing connection after body send.~%" (peer (request response)))
                      (close sock)))

      (setf (parser (peer (request response)))
            (make-instance 'request-parser :peer (peer (request response))))))
