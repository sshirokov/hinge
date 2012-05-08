(in-package :hinge.http)
(defcategory state-machine)

(defclass standard-state-machine (c2mop:funcallable-standard-object)
  ((state :initform :initial :initarg :state
          :accessor state
          :documentation "The current state of the state-machine.")
   (last-event :initform (get-internal-real-time)
               :accessor last-event
               :documentation "The timestamp of the last event."))

  (:metaclass c2mop:funcallable-standard-class)
  (:documentation "(funcall this-instance event-from-bus)
Every iteration of the event machine the `last-event' slot is updated with `get-internal-real-time' before
the funcallable instance application.

SUBCLASS NOTE: Make sure to include ```(:metaclass c2mop:funcallable-standard-class)``` in your
subclass definition, or else the funcallable instance will not function correctly."))

(defgeneric standard-state-machine-event (machine state event)
  (:documentation "Method specialized by `defstate' to handle the actual driving of
the state machine with events."))

(defmethod initialize-instance :before ((machine standard-state-machine) &key)
  "Bind a (funcallable machine event) driver to the event machine instance.
See `defstate' for the reasoning and function. This method is closure plumbing."
  (c2mop:set-funcallable-instance-function
   machine
   #'(lambda (event)
       (log-for (state-machine trace) "~A event ~A" machine event)
       (multiple-value-bind (next-state recur-p)
           (standard-state-machine-event machine (state machine) event)

         (log-for (state-machine trace) "Next state: ~A Recur?: ~A" next-state recur-p)
         (setf (last-event machine) (get-internal-real-time)
               (state machine) (or next-state (state machine)))

         (if recur-p
             (funcall machine event)
             (values machine (state machine)))))))

(defmethod initialize-instance :after ((machine standard-state-machine) &key))

(defmacro deffsm (name parents slots &rest options)
  "Define an fsm `name' as in (defclass name parents slots options)
This macro takes care of the inheritance chain of `standard-state-machine'
and the funcallable metaclass"
  `(defclass ,name ,(append (list 'standard-state-machine) parents)
     ,slots
     (:metaclass c2mop:funcallable-standard-class)
     ,@options))

(defmacro defstate (machine-type state-name (machine-sym event-sym) &body body)
  "Helper macro to define states for the machine of type `machine-type'.

The generated state methods will be specialized on `machine-type' and `state-name', and
subclasses of `standard-state-machine' should use this property to extend the state machine.

`state-name' is the identifier for this state, and names it. Event invocations will
use this name to determine which state the machine is in, and error out if one cannot be found.
The event will be bound to the symbol named `event-sym' declared as in a one-argument lambda list.
Each invocation of this state with the even bound to `event-sym' will evaluate `body' forms as
in a method invocation and the resulting value of the evaluation should return the next state
for the machine as a `:keyword', or `nil' to indicate the machine should remain in its current state.
The symbol `machine' will be bound to the currently executing state machine. The current state is
available in `state'

If the state produces two-value return, it is interpreted as (values next-state recur-event)
and if recur-event is non-nil the same event is sent into the machine again after performing
the transition into next-state. This is useful if simply performing a state transition would
result in event starvation."
  `(defmethod standard-state-machine-event
       ((,machine-sym ,machine-type) (state (eql ,state-name)) ,event-sym)
     ,@body))
