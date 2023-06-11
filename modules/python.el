;;; python.el --- -*- lexical-binding: t; -*-

;;; Commentary:
;;;   configure pacakges for Python development

;;; Code:
(use-package python-mode
  :interpreter
  ("python3" . python-mode))

(use-package pip-requirements)

(use-package pyvenv)

;;; python.el ends here
