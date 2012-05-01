(in-package :hinge)

;; Helpers
(defun make-safe-hash-table (&rest args &key)
  "Wrapper to return a hash table that is safe
to use concurrently. `args' are passed on as to `make-hash-table'"
  ;; TODO: Feature checks for other lisps
  (apply #'make-hash-table :synchronized t args))

;; Classes
;;; Pool Class
(defclass pool (emitter)
  ((size :initform 5
         :initarg :size
         :reader size
         :documentation "The number of pool workers.")
   (workers :initform (list)
            :accessor workers
            :documentation "The collection of worker threads.")

   (work :initform (make-safe-hash-table)
         :accessor work
         :documentation "A mapping of string job ids to `job' objects
scheduled or running in the thread pool.")))

;;;; Generics
(defgeneric make-worker (pool &optional id)
  (:documentation "Return a running thread ready to do work for the given pool."))

;;;; Lifecycle methods
(defmethod initialize-instance :after ((pool pool) &key)
  (format t "TODO: Initializing pool: ~A~%" pool)
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
  (format t "TODO: Fail remaining callbacks in ~A~%" (work pool)))

;;;; Methods
(defmethod make-worker ((pool pool) &optional (id :somekind))
  (flet ((work-fn ()
           ;; TODO: Uh, no
           (do () (nil) (sleep 1))))
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
           :initform (lambda (result) (declare (ignore result)))
           :documentation "Invoked on successful run of `thunk'  with the return value in the original thread.")
   (fail :initarg :fail
         :initform (lambda (condition) (declare (ignore condition)))
         :documentation "Invoked on a failed run of `thunk' with the condition signaled in the original thread.")))
