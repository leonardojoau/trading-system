(load "events.lisp")

(defclass AGENT ()
  ((name
     :accessor name
     :initarg :name)
   (timestamps
     :accessor timestamps
     :initform NIL)
   (revalprices
     :accessor revalprices
     :initform NIL)
   (orders
     :accessor orders
     :initform NIL)
   (positions
     :accessor positions
     :initform NIL)
   (pls
     :accessor pls
     :initform NIL)
   (fitnesses
     :accessor fitnesses
     :initform NIL)
   (trades
     :accessor trades
     :initform NIL)
   (tradestats
     :accessor tradestats
     :initform NIL)
   (incomingmessages
     :accessor incomingmessages
     :initform NIL)
   (outgoingmessages
     :accessor outgoingmessages
     :initform NIL)
   (recepientslist
     :accessor recipientslist
     :initarg :recipientslist
     :initform NIL)))

(defclass FSM ()
  ((currentstate
     :accessor currentstate
     :initarg :currentstate
     :initform NIL)
   (transitions
     :accessor transitions
     :initarg :transitions
     :initform NIL)))

(defclass FSMAGENT (FSM AGENT)
  ((states
     :accessor states
     :initarg :states
     :initform NIL)))

(defclass TRANSITION ()
  ((initialstate
     :accessor initialstate
     :initarg :initialstate)
   (finalstate
     :accessor finalstate
     :initarg :finalstate)
   (sensor
     :accessor sensor
     :initarg :sensor
     :initform #'(lambda (x) x))
   (predicate
     :accessor predicate
     :initarg :predicate
     :initform #'(lambda (x) NIL))
   (actuator
     :accessor actuator
     :initarg :actuator
     :initform #'(lambda (x) NIL))
   (effected
     :accessor effected
     :initform NIL)))

(defmethod perform ((tr TRANSITION) (e EVENT))
  (setf (effected tr) (funcall (predicate tr)
                               (funcall (sensor tr)
                                        e))))

(defmethod operatefsm ((fsm FSM) (e EVENT))
  (let* ((applicable-transitions
           (remove-if-not #'(lambda (x) (equal (initialstate x)
                                               (currentstate fsm)))
                          (transitions fsm)))
         (effected-transition
           (car (remove-if-not #'(lambda (x) (perform x e))
                               applicable-transitions))))
    (funcall (actuator effected-transition)
             (funcall (sensor effected-transition)
                      e))
    (setf (currentstate fsm) (finalstate effected-transition))
    (format T "Transition $ -> S~%"
            (initialstate effected-transition)
            (finalstate effected-transition))))
