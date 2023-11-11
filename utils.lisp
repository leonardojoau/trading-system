(defun cumulative-pl (pls)
  "Generate a cumulative PnL list from a list
   of absolute returns in reverse chronological order."
  (let ((sum 0)
        (result '()))
    (dolist (pl (reverse pls) (nreverse result))
      (setq sum (+ sum pl))
      (push sum result))))
