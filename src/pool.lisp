(in-package :hinge)

;; Helpers
(defun make-safe-hash-table (&rest args &key)
  "Wrapper to return a hash table that is safe
to use concurrently. `args' are passed on as to `make-hash-table'"
  (apply #'make-hash-table :synchronized t args))

;; Classes
(defclass pool ()
  ((size :initform 5
         :initarg :size)
   (workers :initform (list)
            :accessor workers)
   (work :initform (make-safe-hash-table)
         :accessor work)))

(defmethod initialize-instance :after ((pool pool) &key)
  (format t "TODO: Initializing pool: ~A~%" pool))

(defmethod close ((pool pool) &key &allow-other-keys)
  "Terminate all of the workers in the pool and
fire any leftover callbacks as failure."
  (format t "TODO: Terminate worker threads in: ~A~%" (workers pool))
  (format t "TODO: Fail remaining callbacks in ~A~%" (work pool)))

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


