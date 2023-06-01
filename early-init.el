;; Disable package.el in favor of straight.el
(setq package-enable-at-startup nil)

;; HACK Work around native compilation on macOS failing with 'ld: library not
;; found for -lemutls_w'.
;; https://github.com/d12frosted/homebrew-emacs-plus/issues/554
(setenv "LIBRARY_PATH"
	(string-join
	 '("/opt/homebrew/opt/gcc/lib/gcc/13"
	   "/opt/homebrew/opt/libgccjit/lib/gcc/13"
	   "/opt/homebrew/opt/gcc/lib/gcc/13/gcc/aarch64-apple-darwin22/13")
	 ":"))

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

