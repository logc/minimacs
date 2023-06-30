;;; general.el --- -*- lexical-binding: t; -*-

;;; Commentary:
;;;  general configuration settings that either involve the `emacs` package, or affect the whole experience like theming

;;; Code:
(use-package straight
  :custom (straight-use-package-by-default t))

(use-package emacs
  :init
  (setq read-process-output-max (* 1024 1024)) ;; 1mb for LSP
  (setq
   column-number-mode       t
   explicit-shell-file-name "/opt/homebrew/bin/fish"
   multi-term-program       explicit-shell-file-name
   shell-file-name          explicit-shell-file-name
   projectile-project-search-path '("~/Documents/code/work/"
				    "~/Documents/code/personal"))

  ;; Do not allow the cursor in the minibuffer prompt
  (setq minibuffer-prompt-properties
	'(read-only t cursor-intangible t face minibuffer-prompt))
  (add-hook 'minibuffer-setup-hook #'cursor-intangible-mode)

  ;; Enable recursive minibuffers
  (setq enable-recursive-minibuffers t)

  :config
  (set-frame-font "Berkeley Mono 14" nil t)
  (setq use-package-always-defer t)
  (setq confirm-kill-emacs #'y-or-n-p)
  
  ;; Use coreutils gls for dired
  (setq insert-directory-program "/opt/homebrew/bin/gls")

  ;; GPG settings
  (custom-set-variables '(epg-gpg-program  "/opt/homebrew/bin/gpg")))

(use-package acme-theme
  :demand t
  :init (load-theme 'acme :no-confirm))

(use-package smartparens
  :diminish smartparens-mode ;; Do not show in modeline
  :init
  (require 'smartparens-config)
  :config
  (smartparens-global-mode t))

(use-package dashboard
  :demand t
  :config
  (dashboard-setup-startup-hook)
  (setq dashboard-startup-banner (concat user-emacs-directory "minimacs.txt")
	dashboard-center-content t
	dashboard-set-heading-icons t
	dashboard-show-shortcuts nil
	dashboard-items '((projects . 3)
			  (agenda . 5))))

(use-package which-key
  :demand t
  :config
  (which-key-mode))

(use-package minions
  :defer 0.1
  :config
  (setq minions-mode-line-lighter "[+]")
  (minions-mode 1))

(use-package mood-line
  :demand t
  :defer 0.1
  :after minions
  :config
  (defun mood-line-segment-major-mode ()
    "Displays the current major mode in the mode-line."
    (concat (format-mode-line minions-mode-line-modes 'mood-line-major-mode) "  "))
   (mood-line-mode))

(use-package hide-mode-line
  :demand t
  :hook (dashboard-after-initialize-hook . hide-mode-line-mode))


(use-package direnv :config (direnv-mode))

(use-package projectile
  :init (projectile-mode +1)
  :bind (:map projectile-mode-map
	      ("s-p" . projectile-command-map)
	      ("C-c p" . projectile-command-map)))

(use-package vertico
  :straight (vertico :files (:defaults "extensions/*")
                     :includes (vertico-indexed
                                vertico-flat
                                vertico-grid
                                vertico-mouse
                                vertico-quick
                                vertico-buffer
                                vertico-repeat
                                vertico-reverse
                                vertico-directory
                                vertico-multiform
                                vertico-unobtrusive))
  :init
  (vertico-mode))

(use-package vertico-posframe
  :hook (vertico-mode . vertico-posframe-mode)
  :init (vertico-posframe-mode 1))

(use-package orderless
  :init
  (setq completion-styles '(orderless basic)
	completion-category-defaults nil
	completion-category-overrides '((file (styles partial-completion)))))

(use-package marginalia
  :custom
  (marginalia-max-relative-age 0)
  (marginalia-align 'right)
  :init
  (marginalia-mode))

(use-package savehist
  :init
  (savehist-mode))

(use-package flycheck
  :init (global-flycheck-mode))

(use-package lsp-mode
  :hook  (prog-mode . lsp-deferred)
  (lsp-mode . lsp-lens-mode)
  :config
  (setq lsp-idle-delay 0.500)
  (setq lsp-log-io nil)
  (setq lsp-completion-provider :capf)
  (setq lsp-prefer-flymake nil)
  ;; Disable warning when mode derives from prog-mode but does not have LSP e.g. elisp
  (setq lsp-warn-no-matched-clients nil)
  (setq lsp-enable-snippet nil)
  )

(use-package corfu
  :hook (lsp-completion-mode . kb/corfu-setup-lsp) ; Use corfu for lsp completion
  :custom
  ;; Works with `indent-for-tab-command'. Make sure tab doesn't indent when you
  ;; want to perform completion
  (tab-always-indent 'complete)
  (completion-cycle-threshold nil)      ; Always show candidates in menu

  (corfu-auto nil)
  (corfu-auto-prefix 2)
  (corfu-auto-delay 0.25)

  (corfu-min-width 80)
  (corfu-max-width corfu-min-width)     ; Always have the same width
  (corfu-count 14)
  (corfu-scroll-margin 4)
  (corfu-cycle nil)

  ;; `nil' means to ignore `corfu-separator' behavior, that is, use the older
  ;; `corfu-quit-at-boundary' = nil behavior. Set this to separator if using
  ;; `corfu-auto' = `t' workflow (in that case, make sure you also set up
  ;; `corfu-separator' and a keybind for `corfu-insert-separator', which my
  ;; configuration already has pre-prepared). Necessary for manual corfu usage with
  ;; orderless, otherwise first component is ignored, unless `corfu-separator'
  ;; is inserted.
  (corfu-quit-at-boundary nil)
  (corfu-separator ?\s)            ; Use space
  (corfu-quit-no-match 'separator) ; Don't quit if there is `corfu-separator' inserted
  (corfu-preview-current 'insert)  ; Preview first candidate. Insert on input if only one
  (corfu-preselect-first t)        ; Preselect first candidate?

  ;; Other
  (corfu-echo-documentation nil)        ; Already use corfu-doc
  (lsp-completion-provider :none)       ; Use corfu instead for lsp completions
  :init
  (global-corfu-mode)
  :config
  ;; NOTE 2022-03-01: This allows for a more evil-esque way to have
  ;; `corfu-insert-separator' work with space in insert mode without resorting to
  ;; Enable Corfu more generally for every minibuffer, as long as no other
  ;; completion UI is active. If you use Mct or Vertico as your main minibuffer
  ;; completion UI. From
  ;; https://github.com/minad/corfu#completing-with-corfu-in-the-minibuffer
  (defun corfu-enable-always-in-minibuffer ()
    "Enable Corfu in the minibuffer if Vertico/Mct are not active."
    (unless (or (bound-and-true-p mct--active) ; Useful if I ever use MCT
                (bound-and-true-p vertico--input))
      (setq-local corfu-auto nil)       ; Ensure auto completion is disabled
      (corfu-mode 1)))
  (add-hook 'minibuffer-setup-hook #'corfu-enable-always-in-minibuffer 1)

  ;; Setup lsp to use corfu for lsp completion
  (defun kb/corfu-setup-lsp ()
    "Use orderless completion style with lsp-capf instead of the
default lsp-passthrough."
    (setf (alist-get 'styles (alist-get 'lsp-capf completion-category-defaults))
          '(orderless))))

;; Restart
(use-package restart-emacs)

(use-package dired-sidebar
  ;:bind (("SPC o n" . dired-sidebar-toggle-sidebar))
  :ensure t
  :commands (dired-sidebar-toggle-sidebar)
  :init
  (add-hook 'dired-sidebar-mode-hook
            (lambda ()
              (unless (file-remote-p default-directory)
                (auto-revert-mode))))
  :config
  (push 'toggle-window-split dired-sidebar-toggle-hidden-commands)
  (push 'rotate-windows dired-sidebar-toggle-hidden-commands)

  (setq dired-sidebar-subtree-line-prefix "__")
  (setq dired-sidebar-use-term-integration t)
  (setq dired-sidebar-use-custom-font t))

(use-package nerd-icons-dired
  :hook
  (dired-mode . nerd-icons-dired-mode))

(use-package nerd-icons
  :custom
  (nerd-icons-font-family "Symbols Nerd Font Mono"))

;; Here 'completion' means 'vertico'
(use-package nerd-icons-completion
  :after (marginalia nerd-icons)
  :demand t
  :hook (marginalia-mode . nerd-icons-completion-marginalia-setup)
  :config
  (nerd-icons-completion-mode))

(use-package vterm
    :ensure t)

(use-package helpful)

;;; general.el ends here
