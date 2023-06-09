;; Disable package.el in favor of straight.el
(setq package-enable-at-startup nil)

(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)

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

(defun set-exec-path-from-shell-PATH ()
  "Set up Emacs' `exec-path' and PATH environment variable to match
that used by the user's shell.

This is particularly useful under Mac OS X and macOS, where GUI
apps are not started from a shell."
  (interactive)
  (let ((path-from-shell (replace-regexp-in-string
			  "[ \t\n]*$" "" (shell-command-to-string
					  "$SHELL --login -c 'echo $PATH'"
						    ))))
    (setenv "PATH" path-from-shell)
    (setq exec-path (split-string path-from-shell path-separator))))

(set-exec-path-from-shell-PATH)

;; HACK Work around native compilation on macOS failing with 'ld: library not
;; found for -lemutls_w'.
;; https://github.com/d12frosted/homebrew-emacs-plus/issues/554
(setenv "LIBRARY_PATH"
	(string-join
	 '("/opt/homebrew/opt/gcc/lib/gcc/13"
	   "/opt/homebrew/opt/libgccjit/lib/gcc/13"
	   "/opt/homebrew/opt/gcc/lib/gcc/13/gcc/aarch64-apple-darwin22/13")
	 ":"))
