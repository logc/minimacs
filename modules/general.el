;;; general.el --- -*- lexical-binding: t; -*-

;;; Commentary:
;;;  general configuration settings that either involve the `emacs` package,
;;;  or affect the whole experience like theming

;;; Code:
(use-package straight
  :custom (straight-use-package-by-default t))

(use-package emacs
  :init
  ;; Mark all themes as safe
  (setq custom-safe-themes t)

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
  ;; set faces
  (set-face-attribute 'default nil :family "Berkeley Mono" :height 140)
  (set-face-attribute 'variable-pitch nil :family "Berkeley Mono Variable" :height 140)
  (copy-face 'default 'fixed-pitch)

  (setq use-package-always-defer t)
  (setq confirm-kill-emacs #'y-or-n-p)
  
  ;; Use coreutils gls for dired
  (setq insert-directory-program "/opt/homebrew/bin/gls")

  ;; GPG settings
  (setq epg-gpg-program "/opt/homebrew/bin/gpg")

  ;; Settings for backup files
  (setq backup-directory-alist '(("." . "~/.config/emacs/backups"))
        backup-by-copying t    ; Don't delink hardlinks
        version-control t      ; Use version numbers on backups
        delete-old-versions t  ; Automatically delete excess backups
        kept-new-versions 20   ; how many of the newest versions to keep
        kept-old-versions 5    ; and how many of the old
	)

  ;; Make other-frame work on MacOS
  (setq mac-frame-tabbing nil)

  ;; Keep track of recent files
  (recentf-mode 1)
  (setq recentf-max-menu-items 25)
  (setq recentf-max-saved-items 25)

  ;; Tree-sitter grammars
  (setq treesit-language-source-alist
   '((bash "https://github.com/tree-sitter/tree-sitter-bash")
     (css "https://github.com/tree-sitter/tree-sitter-css")
     (elisp "https://github.com/Wilfred/tree-sitter-elisp")
     (html "https://github.com/tree-sitter/tree-sitter-html")
     (json "https://github.com/tree-sitter/tree-sitter-json")
     (markdown "https://github.com/ikatyang/tree-sitter-markdown")
     (python "https://github.com/tree-sitter/tree-sitter-python")
     (scala "https://github.com/tree-sitter/tree-sitter-scala")
     (toml "https://github.com/tree-sitter/tree-sitter-toml")
     (yaml "https://github.com/ikatyang/tree-sitter-yaml"))) 

  ;; Line numbers in programming modes
  (add-hook 'prog-mode-hook 'display-line-numbers-mode)
)

;; Theme
(use-package doom-themes
  :ensure t
  :config
  ;; Global settings (defaults)
  (setq doom-themes-enable-bold t    ; if nil, bold is universally disabled
        doom-themes-enable-italic t) ; if nil, italics is universally disabled

  ;; Enable flashing mode-line on errors
  (doom-themes-visual-bell-config)

  ;; Corrects (and improves) org-mode's native fontification.
  (doom-themes-org-config)

  :init
  (load-theme 'doom-oksolar-dark))

(use-package solaire-mode
  :ensure t
  :config
  (solaire-global-mode +1))

(use-package smartparens
  :diminish smartparens-mode ;; Do not show in modeline
  :init
  (require 'smartparens-config)
  :config
  (smartparens-global-mode t))

(use-package dashboard
  :demand t
  :custom
  (dashboard-banner-logo-title (shell-command-to-string "fortune -s"))
  :config
  (dashboard-setup-startup-hook)
  (setq dashboard-startup-banner (concat user-emacs-directory "minimacs.txt")
	dashboard-center-content t
	dashboard-display-icons-p t
	dashboard-icon-type 'nerd-icons
	dashboard-set-footer nil
	dashboard-set-init-info nil
	dashboard-projects-backend 'projectile
	dashboard-items '((projects . 3)
			  (bookmarks . 3)
			  (agenda . 10))))

(use-package which-key
  :demand t
  :config
  (which-key-mode))

(use-package doom-modeline
  :ensure t
  :init (doom-modeline-mode 1))

(use-package hide-mode-line
  :demand t
  :hook (dashboard-after-initialize-hook . hide-mode-line-mode))

(use-package direnv
  :demand t
  :config
  (direnv-mode))

(use-package projectile
  :init (projectile-mode +1)
  :bind (:map projectile-mode-map
	      ("s-p" . projectile-command-map)
	      ("C-c p" . projectile-command-map))

  :config
  (setq projectile-globally-ignored-directories
  '(".idea"
    ".vscode"
    ".ensime_cache"
    ".eunit"
    ".git"
    ".hg"
    ".fslckout"
    "_FOSSIL_"
    ".bzr"
    "_darcs"
    ".tox"
    ".svn"
    ".stack-work"
    ".ccls-cache"
    ".cache"
    ".clangd")))

(use-package consult)

;(use-package ripgrep)

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
  :if (window-system)
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
  (setq lsp-enable-snippet nil))

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

(use-package treemacs
  :ensure t
  :defer t
  :config
  (progn
    (setq treemacs-collapse-dirs                   (if treemacs-python-executable 3 0)
          treemacs-deferred-git-apply-delay        0.5
          treemacs-directory-name-transformer      #'identity
          treemacs-display-in-side-window          t
          treemacs-eldoc-display                   'simple
          treemacs-file-event-delay                2000
          treemacs-file-extension-regex            treemacs-last-period-regex-value
          treemacs-file-follow-delay               0.2
          treemacs-file-name-transformer           #'identity
          treemacs-follow-after-init               t
          treemacs-expand-after-init               t
          treemacs-find-workspace-method           'find-for-file-or-pick-first
          treemacs-git-command-pipe                ""
          treemacs-goto-tag-strategy               'refetch-index
          treemacs-header-scroll-indicators        '(nil . "^^^^^^")
          treemacs-hide-dot-git-directory          t
          treemacs-indentation                     2
          treemacs-indentation-string              " "
          treemacs-is-never-other-window           nil
          treemacs-max-git-entries                 5000
          treemacs-missing-project-action          'ask
          treemacs-move-forward-on-expand          nil
          treemacs-no-png-images                   nil
          treemacs-no-delete-other-windows         t
          treemacs-project-follow-cleanup          nil
          treemacs-persist-file                    (expand-file-name ".cache/treemacs-persist" user-emacs-directory)
          treemacs-position                        'left
          treemacs-read-string-input               'from-child-frame
          treemacs-recenter-distance               0.1
          treemacs-recenter-after-file-follow      nil
          treemacs-recenter-after-tag-follow       nil
          treemacs-recenter-after-project-jump     'always
          treemacs-recenter-after-project-expand   'on-distance
          treemacs-litter-directories              '("/node_modules" "/.venv" "/.cask")
          treemacs-project-follow-into-home        nil
          treemacs-show-cursor                     nil
          treemacs-show-hidden-files               t
          treemacs-silent-filewatch                nil
          treemacs-silent-refresh                  nil
          treemacs-sorting                         'alphabetic-asc
          treemacs-select-when-already-in-treemacs 'move-back
          treemacs-space-between-root-nodes        t
          treemacs-tag-follow-cleanup              t
          treemacs-tag-follow-delay                1.5
          treemacs-text-scale                      nil
          treemacs-user-mode-line-format           nil
          treemacs-user-header-line-format         nil
          treemacs-wide-toggle-width               70
          treemacs-width                           35
          treemacs-width-increment                 1
          treemacs-width-is-initially-locked       t
          treemacs-workspace-switch-cleanup        nil)

    ;; The default width and height of the icons is 22 pixels. If you are
    ;; using a Hi-DPI display, uncomment this to double the icon size.
    ;;(treemacs-resize-icons 44)

    (treemacs-follow-mode t)
    (treemacs-filewatch-mode t)
    (treemacs-fringe-indicator-mode 'always)
    (when treemacs-python-executable
      (treemacs-git-commit-diff-mode t))

    (pcase (cons (not (null (executable-find "git")))
                 (not (null treemacs-python-executable)))
      (`(t . t)
       (treemacs-git-mode 'deferred))
      (`(t . _)
       (treemacs-git-mode 'simple)))

    (treemacs-hide-gitignored-files-mode nil))
  :bind
  (:map global-map
        ("M-0"       . treemacs-select-window)
        ("C-x t 1"   . treemacs-delete-other-windows)
        ("C-x t t"   . treemacs)
        ("C-x t d"   . treemacs-select-directory)
        ("C-x t B"   . treemacs-bookmark)
        ("C-x t C-t" . treemacs-find-file)
        ("C-x t M-t" . treemacs-find-tag)))

(use-package treemacs-evil
  :after (treemacs evil)
  :ensure t)

(use-package treemacs-projectile
  :after (treemacs projectile)
  :ensure t)

(use-package treemacs-magit
  :after (treemacs magit)
  :ensure t)

(use-package treemacs-nerd-icons
  :demand t
  :config
  (treemacs-load-theme "nerd-icons"))

(use-package nerd-icons-dired
  :hook
  (dired-mode . nerd-icons-dired-mode))

(use-package nerd-icons
  :custom
  (nerd-icons-font-family "Hack Nerd Font"))

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

(use-package ansi-color
    :hook (compilation-filter . ansi-color-compilation-filter)) 

(use-package tabspaces)

(use-package yaml-ts-mode)

;; Use the Debug Adapter Protocol for running tests and debugging
(use-package dap-mode
  :hook
  (lsp-mode . dap-mode)
  (lsp-mode . dap-ui-mode))

(use-package format-all
  :hook (prog-mode . format-all-ensure-formatter))
;;; general.el ends here

