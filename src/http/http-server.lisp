(in-package :hinge.http)

;; Server
(defclass http-server (server)
  ())

(defmethod initialize-instance :after ((server http-server) &key)
  (add-listener server "connection"
                (lambda (client)
                  (make-instance 'http-peer :server server :socket client))))

;; HTTP Parser
;;; Request line parser
(deffsm request-fsm ()
  ((http-method :initform (string "") :accessor http-method)
   (resource :initform (string "") :accessor resource)
   (version :initform (string "") :accessor version))
  (:default-initargs . (:state :read-http-method)))

(defun whitespace-p (code)
  "Is the `code' a code-char for a whitespace char?"
  (member (code-char code) `(#\Newline #\Linefeed #\Return #\Space #\Tab)))

(defstate request-fsm :read-http-method (fsm cc)
  (if (not (whitespace-p cc))
      (not (setf (http-method fsm) (concatenate 'string (http-method fsm) (list (code-char cc)))))

      (if (char-equal (code-char cc) #\Space)
          :read-resource
          :error)))

(defstate request-fsm :read-resource (fsm cc)
  (if (not (whitespace-p cc))
      (not (setf (resource fsm) (concatenate 'string (resource fsm) (list (code-char cc)))))

      (if (char-equal (code-char cc) #\Space)
          :read-version
          :error)))

(defstate request-fsm :read-version (fsm cc)
  (if (not (whitespace-p cc))
      (not (setf (version fsm) (concatenate 'string (version fsm) (list (code-char cc)))))

      (if (char-equal (code-char cc) #\Return)
          :seek-newline
          :error)))

(defstate request-fsm :seek-newline (fsm cc)
  (if (char-equal (code-char cc) #\Newline)
      :done
      :error))

;;; Header block parser
(deffsm header-fsm ()
  ((headers :initform (list) :accessor headers)
   (key-buffer :initform (string "") :accessor key-buffer)
   (value-buffer :initform (string "") :accessor value-buffer))
  (:default-initargs . (:state :key-or-done)))

(defstate header-fsm :read-key (fsm cc)
  (cond ((not (or (char-equal (code-char cc) #\:)
                  (whitespace-p cc)))
         (not (setf (key-buffer fsm) (concatenate 'string (key-buffer fsm) (list (code-char cc))))))

        ((char-equal (code-char cc) #\:)
         :read-space)

        (:otherwise
         :error)))

(defstate header-fsm :read-space (fsm cc)
  (if (char-equal (code-char cc) #\Space)
      :read-value
      :error))

(defstate header-fsm :read-value (fsm cc)
  (cond ((not (member (code-char cc) '(#\Newline #\Return)))
         (not (setf (value-buffer fsm) (concatenate 'string (value-buffer fsm) (list (code-char cc))))))

        ((char-equal (code-char cc) #\Return)
         (push (cons (key-buffer fsm) (value-buffer fsm)) (headers fsm))
         (setf (key-buffer fsm) (string "")
               (value-buffer fsm) (string ""))
         :read-newline)

        (:otherwise
         :error)))

(defstate header-fsm :read-newline (fsm cc)
  (if (char-equal (code-char cc) #\Newline)
      :key-or-done
      :error))

(defstate header-fsm :key-or-done (fsm cc)
  (if (char-equal (code-char cc) #\Return)
      :expect-finish
      (values :read-key t)))

(defstate header-fsm :expect-finish (fsm cc)
  (if (char-equal (code-char cc) #\Newline)
      :done
      :error))

;;; Body dumper
(deffsm body-fsm ()
  ((body :initform (vector) :accessor body)
   (remaining :initform 0 :accessor remaining)))

(defstate body-fsm :initial (fsm event)
  (format t "TODO: Collect body mass upto ~S~%" (remaining fsm)))

(defstate body-fsm :skip (fsm event)
  "Do nothing, we don't care"
  (declare (ignore fsm event)))

;;; Overall request parser
(deffsm request-parser ()
  ((peer :initarg :peer :accessor peer)

   (request-fsm :accessor request-fsm :initform (make-instance 'request-fsm))
   (headers-fsm :accessor headers-fsm :initform (make-instance 'header-fsm))
   (body-fsm :accessor body-fsm :initform (make-instance 'body-fsm))

   (buffer :initform nil :accessor buffer))
  (:default-initargs . (:state :read-request)))

(defstate request-parser :read-request (parser data)
  (flet ((finish (at)
           (if (not (equalp at :error))
               (progn
                 (unless (>= at (length data))
                   (setf (buffer parser) (subseq data at (length data))))

                 (values
                  (if (eql (state (request-fsm parser)) :done) :read-headers nil)
                  (buffer parser)))
               :error)))

    (finish
     (dotimes (i (length data) i)
       (funcall (request-fsm parser) (aref data i))
       (when (eql (state (request-fsm parser)) :error)
         (emit (peer parser) "error" "Request Line Parser Error")
         (return :error))
       (when (eql (state (request-fsm parser)) :done)
         (return (1+ i)))))))

(defstate request-parser :read-headers (parser data)
  (let ((data (or (buffer parser) data)))
    (setf (buffer parser) nil)

    (flet ((finish (at)
             (if (not (equalp at :error))
                 (progn
                   (unless (>= at (length data))
                     (setf (buffer parser)
                           (concatenate '(vector (unsigned-byte 8))
                                        (if (buffer parser) (buffer parser) #())
                                        (subseq data at (length data)))))

                   (values
                    (if (eql (state (headers-fsm parser)) :done)
                        (prog1 :read-body
                          :TODO-parse-headers)
                        nil)
                    (buffer parser)))
                 :error)))

      (finish
       (dotimes (i (length data) i)
         (funcall (headers-fsm parser) (aref data i))
         (when (eql (state (headers-fsm parser)) :error)
           (emit (peer parser) "error" "Header Parser Error")
           (return :error))
         (when (eql (state (headers-fsm parser)) :done)
           (let* ((con-len (cdr (assoc "content-length" (headers (headers-fsm parser)) :test #'string-equal)))
                  (con-len (parse-integer (or con-len "") :junk-allowed t))
                  (con-len (or con-len 0)))
             (format t "=> Content-Length: ~S~%" con-len)
             (when (zerop con-len)
               (setf (state (body-fsm parser)) :skip)
               (emit (peer parser) "request" parser)))
           (return (1+ i))))))))

(defstate request-parser :read-body (parser data)
  (let ((data (or (buffer parser) data)))
    (setf (buffer parser) nil)
    (format t "Body data: ~S~%" (babel:octets-to-string data))
    (values nil (buffer parser))))

;; HTTP Peer
(defclass http-peer (emitter)
  ((server :initarg :server
           :accessor server)
   (socket :initarg :socket
           :accessor sock)
   (parser :accessor parser)))

(defmethod initialize-instance :after ((peer http-peer) &key)
  (setf (parser peer) (make-instance 'request-parser :peer peer))

  (add-listener peer "request"
                (lambda (request)
                  (emit (server peer) "request" request)))

  (add-listener peer "error"
                (lambda (e)
                  (format t "Error: ~S~%" e)
                  (close (sock peer))))

  (add-listener (sock peer) "data"
                (lambda (data)
                  (format t "Request data: ~S~%" (babel:octets-to-string data))
                  (funcall (parser peer) data)
                  (format t "Parser state: ~S~%" (state (parser peer)))))

  (add-listener (sock peer) "close"
                (lambda (_)
                  (declare (ignore _))
                  (format t "Peer ~S left~%" peer))))
