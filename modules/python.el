;;; python.el --- -*- lexical-binding: t; -*-

;;; Commentary:
;;;   configure pacakges for Python development

;;; Code:
(use-package python-mode
  :interpreter
  ("python3" . python-mode)
  :hook
  ((python-mode . lsp))
  :config
  (setq python-shell-interpreter "hatch"
      python-shell-interpreter-args "run python"
      python-shell-prompt-detect-failure-warning nil)
  )

(use-package pip-requirements)

(use-package pyvenv)

(use-package hy-mode)

(use-package jupyter)
;;; python.el ends here
