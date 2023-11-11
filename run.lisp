(load "simple_model.lisp")

; read prices from file

(defparameter *events* '())

(defparameter *modl* (make-instance 'SIMPLEMODEL
                                    :L 10
                                    :name "MA_10"
                                    :states '(:INIT)))

(with-open-file (stream "AAPL.txt")
  (loop for line = (read-line stream NIL)
        while line
        do
        (let* ((ts (subseq line 0 10))
              (px (read-from-string (subseq line 11 (length line))))
              (e (make-instance 'MARKETUPDATE
                                :security "AAPL"
                                :timestamp ts
                                :value px)))
          (setq *events* (append *events* (list e)))
          (format T "timestamp= ~S price= ~S~%" ts px))))

(dolist (e *events*)
  (format T "ts= ~S~%" (timestamp e))
  (consume *modl* e))
