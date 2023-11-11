(defclass EVENT ()
	((timestamp
	:initarg :timestamp
	:accessor timestamp)
	(value
	:initarg :value
	:accessor value))
)

(defclass MARKETUPDATE (EVENT)
	((security
	:initarg :security
	:accessor security))
)

(defmethod price ((e MARKETUPDATE))
  (value e))
