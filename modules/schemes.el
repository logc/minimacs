;;; schemes.el --- -*- lexical-binding: t; -*-

;;; Commentary:
;;;   configuration of Scheme modes

;;; Code:
(use-package scheme
  :ensure nil
  :mode ("\\.scm\\'" . scheme-mode)
  :config
  (add-hook 'scheme-mode-hook 'geiser-mode)) ;; Enable Geiser automatically in Scheme buffers

(use-package geiser-kawa :ensure t
  :config
  (setq geiser-scheme-implementation 'kawa))

(use-package racket-mode)

;;; schemes.el ends here
