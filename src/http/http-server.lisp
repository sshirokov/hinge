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
  ())

(defstate request-fsm :initial (fsm data)
  (format t "~S => ~S~%" fsm (code-char data))
  (cond ((char-equal (code-char data) #\Return)
         :seek-newline)
        ((char-equal (code-char data) #\Newline)
         :error)))

(defstate request-fsm :seek-newline (fsm data)
  (if (char-equal (code-char data) #\Newline)
      :done
      :error))

;;; Header block parser
(deffsm header-fsm ()
  ()
  (:default-initargs . (:state :seek-return-1)))

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
