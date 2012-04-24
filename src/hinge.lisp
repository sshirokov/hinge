(in-package :hinge)

;; Helpers
(defun make-safe-hash-table (&rest args &key)
  "Wrapper to return a hash table that is safe
to use concurrently. `args' are passed on as to `make-hash-table'"
  (apply #'make-hash-table :synchronized t args))

;; Classes
(defclass hinge (ev:ev-loop)
  ((workers :initform (list)
            :accessor workers
            :documentation "A list of thread handles.")
   (work :initform (make-safe-hash-table)
         :accessor work
         :documentation "A table holding jobs as `hinge-job' sent to the thread-pool keyed by unique string ID")))

(defclass hinge-job ()
  ((id :initarg :id
       :reader id
       :documentation "String identifier of the background job. Should match the key in the work table.")
   (stamps :initform (list (:new (get-internal-real-time)))
           :accessor stamps
           :documentation "A list of events and time-stamps in the format of (:event time) in
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


;; Package Dynamics
(defvar *hinge* (make-instance 'hinge) "The current instance of the event reactor.")
