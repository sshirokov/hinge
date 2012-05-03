(in-package :hinge)

;; Helpers
(defun make-safe-hash-table (&rest args &key &allow-other-keys)
  "Wrapper to return a hash table that is safe
to use concurrently. `args' are passed on as to `make-hash-table'"
  ;; TODO: Feature checks for other lisps
  (apply #'make-hash-table :synchronized t args))

;; Classes
;;; Pool Class
(defclass pool (emitter)
  ((context :initform (zmq:init 0)
            :reader context)

   (work-address :initform (format nil "inproc://threadpool-work-~A" (uuid:make-v4-uuid))
                 :reader work-address)
   (work-sock :accessor work-sock)

   (result-address :initform (format nil "inproc://threadpool-result-~A" (uuid:make-v4-uuid))
                   :reader result-address)
   (result-sock :accessor result-sock)

   (size :initform 2
         :initarg :size
         :reader size
         :documentation "The number of pool workers.")
   (workers :initform (list)
            :accessor workers
            :documentation "The collection of worker threads.")

   (work :initform (make-safe-hash-table :test 'equalp)
         :accessor work
         :documentation "A mapping of string job ids to `job' objects
scheduled or running in the thread pool.")))

;;;; Generics
(defgeneric make-worker (pool &optional id)
  (:documentation "Return a running thread ready to do work for the given pool."))

;;;; Lifecycle methods
(defmethod bind ((pool pool) sub-mask &optional host)
  "Create and bind the sockets for work distribution and collection."
  (declare (ignore  host))
  (format t "~A Building work and result sockets. ~%" pool)
  (setf (work-sock pool) (zmq:socket (context pool) :push)
        (result-sock pool) (make-instance 'zmq-socket :owner (owner pool)
                                          :context (context pool) :type :sub))

  (zmq:bind (work-sock pool) (work-address pool))
  (bind (result-sock pool) (result-address pool))

  ;; TODO: There should probably be an API for this, since it's useless without it..
  (format t "Subscribing to results: ~S.~%" sub-mask)
  (zmq:setsockopt (sock (result-sock pool)) :subscribe sub-mask)

  (add-listener (result-sock pool) "data"
                (lambda (data)
                  (let* ((job-id (babel:octets-to-string data))
                         (job (prog1 (gethash job-id (work pool))
                                (remhash job-id (work pool)))))
                    (if (not job)
                        (format t "WARNING: Receiver could not find job: ~S~%" job-id)
                        (progn
                          (case (status job)
                            (:done (funcall (finish job) (result job)))
                            (:error (funcall (fail job) (result job)))
                            (otherwise
                             (format t "WARNING: Job ~S returned in unknown status: ~S~%" job-id (status job))))))))))

(defmethod initialize-instance :after ((pool pool) &key)
  (bind pool "")

  (format t "Initializing pool: ~A~%" pool)
  (dotimes (worker-id (size pool))
    (let ((worker (make-worker pool worker-id)))
      (format t "Adding worker ~a: ~S~%" worker-id worker)
      (push worker (workers pool)))))

(defmethod close ((pool pool) &key &allow-other-keys)
  "Terminate all of the workers in the pool and
fire any leftover callbacks as failure."
  (dolist (worker (workers pool))
    (when (and (bt:threadp worker) (bt:thread-alive-p worker))
      (format t "Destroying worker: ~S~%" (bt:thread-name worker))
      (bt:destroy-thread worker)))
  (setf (workers pool) (list))

  (format t "Destroying worker socket.~%")
  (when-let (sock (work-sock pool))
    (setf (work-sock pool) nil)
    (zmq:close sock))
  (format t "Destroying result socket.~%")
  (when-let (sock (result-sock pool))
    (setf (result-sock pool) nil)
    (close sock))
  (format t "Destroying the job passing context.~%")
  (when-let (ctx (context pool))
    (setf (slot-value pool 'context) nil)
    (zmq:term ctx))

  (format t "TODO: Fail remaining callbacks in ~A~%" (work pool)))

;;;; Methods
(defmethod make-worker ((pool pool) &optional (id :somekind))
  (flet ((work-fn ()
           (zmq:with-sockets ((work (context pool) :pull)
                              (result (context pool) :pub))
             (zmq:connect work (work-address pool))
             (zmq:connect result (result-address pool))

             (format t "Worker: ~S waiting to work.~%" (bt:thread-name (bt:current-thread)))
             (do* ((job-id (zmq:recv! work :string) (zmq:recv! work :string))
                   (job (gethash job-id (work pool)) (gethash job-id (work pool))))
                  (nil)
               (if (not job)
                   (format t "WARNING: Worker ~S could not find job-id ~S~%"
                           (bt:thread-name (bt:current-thread)) job-id)
                   (progn
                     (perform job)
                     (zmq:send! result job-id)))))))

    (bt:make-thread #'work-fn :name (format nil "pool-worker[~S]" id))))


;;; Job Class
(defclass job ()
  ((id :initarg :id
       :reader id
       :documentation "String identifier of the background job. Should match the key in the work table.")
   (stamps :initform (list (cons :new (get-internal-real-time)))
           :accessor stamps
           :documentation "A list of events and time-stamps in the format of (:event . time) in
reverse-chronological order. Possible events are: `:new' `:active' `:done' `:error'")

   (thunk :initarg :thunk
          :accessor thunk
          :initform (lambda () :badjob)
          :documentation "Invoked when the job is scheduled to execute.")

   (result :accessor result
           :documentation "The result of evaluating `thunk'.")

   (finish :initarg :finish
           :reader finish
           :initform (lambda (result) (declare (ignore result)))
           :documentation "Invoked on successful run of `thunk'  with the return value in the original thread.")
   (fail :initarg :fail
         :reader fail
         :initform (lambda (condition) (declare (ignore condition)))
         :documentation "Invoked on a failed run of `thunk' with the condition signaled in the original thread.")))

(defgeneric perform (job)
  (:documentation "Perform the job and either store success or failure.
Returns three values: the job, the terminal status, and the result")
  (:method ((job job))
    (format t "Worker: ~S got job-id ~S => ~S~%"
            (bt:thread-name (bt:current-thread)) (id job) job)
    (stamp job :active)
    (setf (result job) (handler-case
                           (prog1 (funcall (thunk job))
                             (stamp job :done))

                         (t (c)
                           (prog1 c
                             (stamp job :error)))))
    (format t "Worker: ~S finished job-id ~S => ~S~%" (bt:thread-name (bt:current-thread)) (id job) (status job))
    (values job (status job) (result job))))

(defgeneric status (job)
  (:documentation "The current status of the job.")
  (:method ((job job))
    (caar (stamps job))))

(defgeneric stamp (job status)
  (:documentation "Add a status stamp to a job with the current internal time.")
  (:method ((job job) status)
    (push (cons status (get-internal-real-time)) (stamps job))))

;;; API
(defmacro async ((&key hinge pool success failure) &body forms)
  (with-gensyms (g!job-id g!job e!pool e!hinge e!success e!failure)
    `(let* ((,e!hinge ,hinge)
            (,e!pool (or ,pool
                         (bg-pool (if ,e!hinge ,e!hinge (get-default-hinge)))))
            (,e!success ,success)
            (,e!failure ,failure)

            (,g!job-id (princ-to-string (uuid:make-v4-uuid)))
            (,g!job (apply #'make-instance 'job :id ,g!job-id
                           :thunk (lambda () ,@forms)
                           (flatten
                            (remove nil (list
                                         (when ,e!success (list :finish ,e!success))
                                         (when ,e!failure (list :fail ,e!failure))))))))
       (setf (gethash ,g!job-id (work ,e!pool)) ,g!job)
       (format t "Sending job: ~S to pool ~S~%" ,g!job-id ,e!pool)
       (zmq:send! (work-sock ,e!pool) ,g!job-id)
       ,g!job-id)))
