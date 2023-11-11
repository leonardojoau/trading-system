(load "agents.lisp")

(defclass SIMPLEMODEL (FSMAGENT)
  ((L
     :accessor L
     :initarg :L)
   (COUNTER
     :accessor COUNTER
     :initform 0)
   (MA
     :accessor MA
     :initform 0)))

(defmethod initialize ((a SIMPLEMODEL))
  (with-slots (L states name) a
    (when (null states)
      (push :INIT states)
      (setf name (concatenate 'string
                              "SIMPLE_MODEL_"
                              (format NIL "A" L))))))

(defun avg-list (list)
  (/ (reduce #'+ list) (length list)))

(defun sub-list (list start end)
  (subseq list start (min end (length list))))

(defmethod preprocess ((a SIMPLEMODEL) (e MARKETUPDATE))
  (with-slots (L COUNTER MA revalprices) a
    (setf COUNTER (length revalprices))
    (setf MA (avg-list (sub-list revalprices 0 (- L 1))))))

(defmethod setfsm ((a SIMPLEMODEL))
  (with-slots (L COUNTER MA states currentstate
                 revalprices transitions positions name) a
    (setf currentstate (first states))
    (setf transitions (list
                        (make-instance
                          'TRANSITION
                          :initialstate :INIT
                          :finalstate :INIT
                          :sensor #'price
                          :predicate #'(lambda (p)
                                         (<= COUNTER L))
                          :actuator #'(lambda (p)
                                        (push 0 positions)
                                        (format T
                                                "~S INIT->INIT ~%"
                                                name)))
                        (make-instance
                          'TRANSITION
                          :initialstate :INIT
                          :finalstate :LONG
                          :sensor #'price
                          :predicate #'(lambda (p)
                                         (and (> COUNTER L)
                                              (> p MA)))
                          :actuator #'(lambda (p)
                                        (push 1 positions)
                                        (format T
                                                "~S INIT->LONG ~%"
                                                name)))
                        (make-instance
                          'TRANSITION
                          :initialstate :INIT
                          :finalstate :SHORT
                          :sensor #'price
                          :predicate #'(lambda (p)
                                         (and (> COUNTER L)
                                              (<= p MA)))
                          :actuator #'(lambda (p)
                                        (push -1 positions)
                                        (format T
                                                "~S INIT->SHORT ~%"
                                                name)))
                        (make-instance
                          'TRANSITION
                          :initialstate :LONG
                          :finalstate :INIT
                          :sensor #'price
                          :predicate #'(lambda (p)
                                         NIL)
                          :actuator #'(lambda (p)
                                        NIL))
                        (make-instance
                          'TRANSITION
                          :initialstate :LONG
                          :finalstate :LONG
                          :sensor #'price
                          :predicate #'(lambda (p)
                                         (and (> COUNTER L)
                                              (> p MA)))
                          :actuator #'(lambda (p)
                                        (push 1 positions)
                                        (format T
                                                "~S LONG->LONG ~%"
                                                name)))
                        (make-instance
                          'TRANSITION
                          :initialstate :LONG
                          :finalstate :SHORT
                          :sensor #'price
                          :predicate #'(lambda (p)
                                         (and (> COUNTER L)
                                              (<= p MA)))
                          :actuator #'(lambda (p)
                                        (push -1 positions)
                                        (format T
                                                "~S LONG->SHORT ~%"
                                                name)))
                        (make-instance
                          'TRANSITION
                          :initialstate :SHORT
                          :finalstate :INIT
                          :sensor #'price
                          :predicate #'(lambda (p)
                                         NIL)
                          :actuator #'(lambda (p)
                                        NIL))
                        (make-instance
                          'TRANSITION
                          :initialstate :SHORT
                          :finalstate :LONG
                          :sensor #'price
                          :predicate #'(lambda (p)
                                         (and (> COUNTER L)
                                              (> p MA)))
                          :actuator #'(lambda (p)
                                        (push 1 positions)
                                        (format T
                                                "~S SHORT->LONG ~%"
                                                name)))
                        (make-instance
                          'TRANSITION
                          :initialstate :SHORT
                          :finalstate :SHORT
                          :sensor #'price
                          :predicate #'(lambda (p)
                                         (and (> COUNTER L)
                                              (<= p MA)))
                          :actuator #'(lambda (p)
                                        (push -1 positions)
                                        (format T
                                                "~S SHORT->SHORT ~%"
                                                name)))))))

(defmethod postprocess ((a SIMPLEMODEL) (e MARKETUPDATE))
  (with-slots (name COUNTER MA states positions pls) a
    (format T "Event ~S S consumed for agent ~S :~%"
            (timestamp e) (price e) name)
    (format T "Output: COUNTER= ~S MA= ~S state= ~S
               position= ~S pl= ~S~%" COUNTER MA (first states)
               (first positions) (first pls))))

(defun consume (a e)
  (when (observe a e)
    (update a e)))

(defmethod observe ((a AGENT) (e EVENT))
  T)

(defmethod update :before ((a AGENT) (e MARKETUPDATE))
  (when (null (timestamps a))
    (push 0 (pls a)) 
    (push 0 (fitnesses a)))
  (push (timestamp e) (timestamps a))
  (push (price e) (revalprices a))
  (preprocess a e)
  (format T ":before completed for agent ~A and event ~A~%" a e))

(defmethod update :after ((a AGENT) (e EVENT))
  (let* ((L (length (timestamps a)))
         (lastposition (first (positions a)))
         (prevposition (if (< L 2) 0 (second (positions a))))
         (tradequantity (- lastposition prevposition))
         (lastprice (first (revalprices a)))
         (prevprice (if (< L 2) 0 (second (revalprices a))))
         (pl (if (< L 2)
                 0
                 (* prevposition (- lastprice prevprice)))))
    (push pl (pls a))
    (unless (zerop tradequantity)
;      (push (make-TRADE
;              :timestamp (timestamp e)
;              :price (+ (price e)
;                        (slippage a e tradequantity))
;              :quantity tradequantity)
;            (trades a))
;      (push (compute-tradestats (trades a)) (tradestats a))
      )
    (postprocess a e)
    (format T ":after completed for agent ~A and event ~A~%"
            a e)))

(defmethod update ((a FSMAGENT) (e MARKETUPDATE))
  (setfsm a)
  (format T "Set FSM completed for ~S~%" (name a))
  (operatefsm a e)
  (format T "Operate FSM completed for ~S~%" (name a))
  (push (currentstate a) (states a))
  (format T ":main completed for ~S and new state ~S added ~%"
          (name a) (currentstate a)))
