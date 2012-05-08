(in-package :hinge.http)

;; Server
(defclass http-server (server)
  ())

(defmethod initialize-instance :after ((server http-server) &key)
  (add-listener server "connection"
                (lambda (client)
                  (make-instance 'http-peer :socket client))))

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
  (:default-initargs . (:state :read-key)))

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

(defstate header-fsm :seek-return-1 (fsm data)
  (format t "~S[~S] => ~S~%" fsm state (code-char data))
  (when (char-equal (code-char data) #\Return)
    :seek-newline-1))

(defstate header-fsm :seek-newline-1 (fsm data)
  (format t "~S[~S] => ~S~%" fsm state (code-char data))
  (if (char-equal (code-char data) #\Newline)
    :seek-return-2
    :error))

(defstate header-fsm :seek-return-2 (fsm data)
  (format t "~S[~S] => ~S~%" fsm state (code-char data))
  (if (char-equal (code-char data) #\Return)
    :seek-newline-2
    :seek-return-1))

(defstate header-fsm :seek-newline-2 (fsm data)
  (format t "~S[~S] => ~S~%" fsm state (code-char data))
  (if (char-equal (code-char data) #\Newline)
      :done
      :error))

;;; Body dumper
(deffsm body-fsm ()
  ())

(deffsm request-parser ()
  ((peer :initarg :peer :accessor peer)

   (request-fsm :initform (make-instance 'request-fsm) :accessor request-fsm)
   (headers-fsm :initform (make-instance 'header-fsm) :accessor headers-fsm)
   (body-fsm :initform (make-instance 'body-fsm) :accessor body-fsm)

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
    (format t "Headers data: ~S~%" (babel:octets-to-string data))

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
           (return (1+ i))))))))

;; HTTP Peer
(defclass http-peer (emitter)
  ((socket :initarg :socket
           :accessor sock)
   (parser :accessor parser)))

(defmethod initialize-instance :after ((peer http-peer) &key)
  (setf (parser peer) (make-instance 'request-parser :peer peer))

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
