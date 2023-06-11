;;; preliminaries.el --- -*- lexical-binding: t; -*-

;;; Commentary:
;;;   preliminary installation like straight and use-package; should go BEFORE
;;;   anything else except early-init.el

;;; Code:
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
	(url-retrieve-synchronously
	 "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
	 'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

;; Install use-package
(straight-use-package 'use-package)

;; Supress compilation warnings
(setq warning-suppress-log-types '((comp)))

;;; preliminaries.el ends here
