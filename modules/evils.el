;;; evils.el --- -*- lexical-binding: t; -*-

;;; Commentary:
;;;  configuration of evil-mode and evil-related packages like evil-collection

;;; Code:
(use-package evil
  :hook (after-init . evil-mode)
  :init
  (setq evil-want-integration t)
  (setq evil-want-keybinding nil)
  :config
  (evil-mode 1))

(use-package evil-leader
  :commands (evil-leader-mode)
  :ensure evil-leader
  :demand evil-leader
  :after evil
  :init (global-evil-leader-mode)
  :config
  (progn
    (evil-leader/set-leader "<SPC>")
    (evil-leader/set-key "<SPC>" 'execute-extended-command)
    (evil-leader/set-key "/" 'consult-ripgrep)
    (evil-leader/set-key "bb" 'consult-buffer)
    (evil-leader/set-key "bd" 'kill-current-buffer)
    (evil-leader/set-key "ff" 'find-file)
    (evil-leader/set-key "fs" 'save-buffer)
    (evil-leader/set-key "gg" 'magit-status)
    (evil-leader/set-key "pp" 'projectile-switch-project)
    (evil-leader/set-key "qq" 'save-buffers-kill-emacs)
    (evil-leader/set-key "qr" 'restart-emacs)
    (evil-leader/set-key "tF" 'toggle-frame-fullscreen)
    (evil-leader/set-key "w" evil-window-map)
    (evil-leader/set-key "wd" 'delete-window)
    (evil-leader/set-key "wa" 'ace-window)

    ;; TODO: only in scala-mode
    (evil-leader/set-key "bb" 'run-sbt)))

(use-package evil-collection
  :after evil
  :demand t
  :config
  (evil-collection-init 'eww)
  (evil-collection-init))

(use-package evil-org
  :ensure t
  :after org
  :hook (org-mode . evil-org-mode)
  :config
  (add-to-list 'evil-emacs-state-modes 'org-agenda-mode)
  (require 'evil-org-agenda)
  (evil-org-agenda-set-keys))

(use-package evil-surround
  :demand t
  :after evil
  :config
  (global-evil-surround-mode 1))

;;; evils.el ends here
