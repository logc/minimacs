;; Disable package.el in favor of straight.el
(setq package-enable-at-startup nil)

;; General early changes
(setq read-process-output-max (* 1024 1024)) ;; 1mb for LSP

(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)

;; Frame
(add-to-list 'default-frame-alist '(ns-transparent-titlebar . t))
(add-to-list 'default-frame-alist '(ns-appearance . dark))
(setq
 ns-use-proxy-icon nil
 frame-title-format nil)

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
