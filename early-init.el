;; Disable package.el in favor of straight.el
(setq package-enable-at-startup nil)

(defmacro with-gc-cons-threshold (threshold &rest body)
  "Execute BODY with GC threshold temporarily set to THRESHOLD.
   Log the current GC threshold before and after the block is executed."
  (let ((orig-gc-threshold gc-cons-threshold))
    `(unwind-protect
         (progn
           (setq gc-cons-threshold ,threshold)
           (message "Current GC threshold: %s" gc-cons-threshold)
           ,@body)
       (setq gc-cons-threshold ,orig-gc-threshold)
       (message "Restored GC threshold: %s" gc-cons-threshold))))

