;;;; minimacs --- an Emacs distribution focused on minimalism

;;;; Commentary:
;;;;   inspired by Doom Emacs

;;;; Code:

(setq gc-cons-threshold (* 50 1000 1000))

;;;; Preliminaries:

;; Install straight.el
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

;;;; Configurations:

;; Configure use-package to use straight.el by default
(use-package straight
             :custom (straight-use-package-by-default t))

;; Configurations

(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)
(set-frame-font "Hack 14" nil t)
(setq use-package-always-defer t)
;; confirm quit
(setq confirm-kill-emacs #'y-or-n-p)

;; Use packages
(use-package dashboard
  :demand t
  :config
  (dashboard-setup-startup-hook)
  (setq dashboard-startup-banner "~/.emacs.d/minimacs.txt"
        dashboard-center-content t
        dashboard-set-heading-icons t
	dashboard-show-shortcuts nil
	dashboard-items '((projects . 3)
			  (agenda . 5))
	dashboard-item-names '(("Agenda for the coming week:" . "Agenda:"))))

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
    (evil-leader/set-key "ff" 'find-file)
    (evil-leader/set-key "fs" 'save-buffer)
    (evil-leader/set-key "bb" 'ibuffer)
    (evil-leader/set-key "bd" 'kill-current-buffer)
    (evil-leader/set-key "gg" 'magit-status)
    (evil-leader/set-key "pp" 'projectile-switch-project)
    (evil-leader/set-key "w" evil-window-map)
    (evil-leader/set-key "wd" 'delete-window)
    (evil-leader/set-key "qq" 'save-buffers-kill-emacs)
    (evil-leader/set-key "qr" 'restart-emacs)
    (evil-leader/set-key "tF" 'toggle-frame-fullscreen)

    ;; TODO: only in scala-mode
    (evil-leader/set-key "bb" 'run-sbt)))

(use-package evil-collection
  :after evil
  :demand t
  :config
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

(use-package which-key
  :demand t
  :config
  (which-key-mode))

(use-package all-the-icons
  :if (display-graphic-p))

(use-package doom-modeline
  :hook (after-init . doom-modeline-mode)
  :config
  (setq doom-modeline-major-mode-color-icon nil))

(use-package plan9-theme
  :demand t
  :config
  (load-theme 'plan9 t))

(use-package projectile
  :init
  (projectile-mode +1)
  :bind (:map projectile-mode-map
              ("s-p" . projectile-command-map)
              ("C-c p" . projectile-command-map)))

(use-package vertico
  :init
  (vertico-mode)

  ;; Different scroll margin
  ;; (setq vertico-scroll-margin 0)

  ;; Show more candidates
  ;; (setq vertico-count 20)

  ;; Grow and shrink the Vertico minibuffer
  ;; (setq vertico-resize t)

  ;; Optionally enable cycling for `vertico-next' and `vertico-previous'.
  ;; (setq vertico-cycle t)
  )

;; Optionally use the `orderless' completion style. See
;; `+orderless-dispatch' in the Consult wiki for an advanced Orderless style
;; dispatcher. Additionally enable `partial-completion' for file path
;; expansion. `partial-completion' is important for wildcard support.
;; Multiple files can be opened at once with `find-file' if you enter a
;; wildcard. You may also give the `initials' completion style a try.
(use-package orderless
  :init
  ;; Configure a custom style dispatcher (see the Consult wiki)
  ;; (setq orderless-style-dispatchers '(+orderless-dispatch)
  ;;       orderless-component-separator #'orderless-escapable-split-on-space)
  (setq completion-styles '(orderless)
        completion-category-defaults nil
        completion-category-overrides '((file (styles partial-completion)))))

;; Persist history over Emacs restarts. Vertico sorts by history position.
(use-package savehist
  :init
  (savehist-mode))

;; A few more useful configurations...
(use-package emacs
  :init
  ;; Add prompt indicator to `completing-read-multiple'.
  ;; Alternatively try `consult-completing-read-multiple'.
  (defun crm-indicator (args)
    (cons (concat "[CRM] " (car args)) (cdr args)))
  (advice-add #'completing-read-multiple :filter-args #'crm-indicator)

  ;; Do not allow the cursor in the minibuffer prompt
  (setq minibuffer-prompt-properties
        '(read-only t cursor-intangible t face minibuffer-prompt))
  (add-hook 'minibuffer-setup-hook #'cursor-intangible-mode)

  ;; Emacs 28: Hide commands in M-x which do not work in the current mode.
  ;; Vertico commands are hidden in normal buffers.
  ;; (setq read-extended-command-predicate
  ;;       #'command-completion-default-include-p)

  ;; Enable recursive minibuffers
  (setq enable-recursive-minibuffers t))


;; Scala
;; Enable scala-mode for highlighting, indentation and motion commands
(use-package scala-mode
  :interpreter
    ("scala" . scala-mode))

;; Enable sbt mode for executing sbt commands
(use-package sbt-mode
  :after scala-mode
  :commands sbt-start sbt-command
  :config
  ;; WORKAROUND: https://github.com/ensime/emacs-sbt-mode/issues/31
  ;; allows using SPACE when in the minibuffer
  (substitute-key-definition
   'minibuffer-complete-word
   'self-insert-command
   minibuffer-local-completion-map)
   ;; sbt-supershell kills sbt-mode:  https://github.com/hvesalai/emacs-sbt-mode/issues/152
  (setq
   sbt:ansi-support t
   sbt:program-options '("-Dsbt.supershell=false")))

(defun run-sbt ()
  "Run an SBT shell in a dedicated buffer."
  (interactive)
  (pop-to-buffer "*sbt*")
  (sbt-start))

;; Enable nice rendering of diagnostics like compile errors.
(use-package flycheck
  :init (global-flycheck-mode))

(use-package lsp-mode
  ;; Optional - enable lsp-mode automatically in scala files
  :hook  (scala-mode . lsp)
         (lsp-mode . lsp-lens-mode)
  :config
  ;; Uncomment following section if you would like to tune lsp-mode performance according to
  ;; https://emacs-lsp.github.io/lsp-mode/page/performance/
  (setq gc-cons-threshold 100000000) ;; 100mb
  (setq read-process-output-max (* 1024 1024)) ;; 1mb
  (setq lsp-idle-delay 0.500)
  (setq lsp-log-io nil)
  (setq lsp-completion-provider :capf)
  (setq lsp-prefer-flymake nil))

;; Add metals backend for lsp-mode
(use-package lsp-metals)

;; Enable nice rendering of documentation on hover
;;   Warning: on some systems this package can reduce your emacs responsiveness significally.
;;   (See: https://emacs-lsp.github.io/lsp-mode/page/performance/)
;;   In that case you have to not only disable this but also remove from the packages since
;;   lsp-mode can activate it automatically.
(use-package lsp-ui)

;; lsp-mode supports snippets, but in order for them to work you need to use yasnippet
;; If you don't want to use snippets set lsp-enable-snippet to nil in your lsp-mode settings
;;   to avoid odd behavior with snippets and indentation
(use-package yasnippet)

;; Use company-capf as a completion provider.
;;
;; To Company-lsp users:
;;   Company-lsp is no longer maintained and has been removed from MELPA.
;;   Please migrate to company-capf.
(use-package company
  :hook (after-init . company-mode)
  :config
  (setq lsp-completion-provider :capf)
  (global-company-mode)
  (setq company-idle-delay 0
        company-minimum-prefix-length 1
	company-selection-wrap-around t)
  (company-tng-configure-default))

;; Use the Debug Adapter Protocol for running tests and debugging
;; Posframe is a pop-up tool that must be manually installed for dap-mode
(use-package posframe)
(use-package dap-mode
  :hook
  (lsp-mode . dap-mode)
  (lsp-mode . dap-ui-mode))

;; Java

;; switch java

(setq JAVA_BASE "/Library/Java/JavaVirtualMachines")

(defun switch-java--versions ()
  "Return the list of installed JDK."
  (seq-remove
   (lambda (a) (or (equal a ".") (equal a "..")))
   (directory-files JAVA_BASE)))

(defun switch-java--save-env ()
  "Store original PATH and JAVA_HOME."
  (when (not (boundp 'SW_JAVA_PATH))
    (setq SW_JAVA_PATH (getenv "PATH")))
  (when (not (boundp 'SW_JAVA_HOME))
    (setq SW_JAVA_HOME (getenv "JAVA_HOME"))))

(defun switch-java ()
  "List the installed JDKs and enable to switch the JDK in use."
  (interactive)
  ;; store original PATH and JAVA_HOME
  (switch-java--save-env)

  (let ((ver (completing-read
              "Which Java: "
              (seq-map-indexed
               (lambda (e i) (list e i)) (switch-java--versions))
              nil t "")))
    ;; switch java version
    (setenv "JAVA_HOME" (concat JAVA_BASE "/" ver "/Contents/Home"))
    (setenv "PATH" (concat (concat (getenv "JAVA_HOME") "/bin/java")
                           ":" SW_JAVA_PATH)))
  ;; show version after switching
  (switch-java-which-version?))

(defun switch-java-default ()
  "Restore the default Java version."
  (interactive)
  ;; store original PATH and JAVA_HOME
  (switch-java--save-env)

  ;; switch java version
  (setenv "JAVA_HOME" SW_JAVA_HOME)
  (setenv "PATH" SW_JAVA_PATH)
  ;; show version
  (switch-java-which-version?))

(defun switch-java-which-version? ()
  "Display the current version selected Java version."
  (interactive)
  ;; displays current java version
  (message (concat "Java HOME: " (getenv "JAVA_HOME"))))

;; Magit
(use-package magit)

(use-package forge
 :after magit
 :defer t
 :config
 (add-to-list 'forge-alist
              '("ghe.spotify.net" "ghe.spotify.net/api/v3"
                "ghe.spotify.net" forge-github-repository)))

;; Restart
(use-package restart-emacs)

;; org
(use-package org
  :bind
  ;; from: https://github.com/hlissner/doom-emacs/blob/develop/modules/lang/org/config.el
  ;; Recently, a [tab] keybind in `outline-mode-cycle-map' has begun
  ;; overriding org's [tab] keybind in GUI Emacs. This is needed to undo
  ;; that, and should probably be PRed to org.
  (:map org-mode-map
	("<tab>" . org-cycle))
  :config
  (setq org-agenda-files '("~/Documents/org/agendas"))
  (setq org-todo-keywords
	'((sequence
	   "TODO(t)"  ; A task that needs doing & is ready to do
	   "PROJ(p)"  ; A project, which usually contains other tasks
	   "LOOP(r)"  ; A recurring task
	   "STRT(s)"  ; A task that is in progress
	   "WAIT(w)"  ; Something external is holding up this task
	   "HOLD(h)"  ; This task is paused/on hold because of me
	   "IDEA(i)"  ; An unconfirmed and unapproved task or notion
	   "|"
	   "DONE(d)"  ; Task successfully completed
	   "KILL(k)") ; Task was cancelled, aborted or is no longer applicable
	  (sequence
	   "[ ](T)"   ; A task that needs doing
	   "[-](S)"   ; Task is in progress
	   "[?](W)"   ; Task is being held up or paused
	   "|"
	   "[X](D)")  ; Task was completed
	  (sequence
	   "|"
	   "OKAY(o)"
	   "YES(y)"
	   "NO(n)"))))

(use-package org-bullets
  :hook (org-mode . org-bullets-mode)
  :custom
  (org-bullets-bullet-list '("◉" "○" "●" "○" "●" "○" "●")))

;; Treemacs
(use-package treemacs
  :ensure t
  :defer t
  :init
  (with-eval-after-load 'winum
    (define-key winum-keymap (kbd "M-0") #'treemacs-select-window))
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

(use-package treemacs-icons-dired
  :hook (dired-mode . treemacs-icons-dired-enable-once)
  :ensure t)

(use-package treemacs-magit
  :after (treemacs magit)
  :ensure t)

(use-package treemacs-persp ;;treemacs-perspective if you use perspective.el vs. persp-mode
  :after (treemacs persp-mode) ;;or perspective vs. persp-mode
  :ensure t
  :config (treemacs-set-scope-type 'Perspectives))

(use-package treemacs-tab-bar ;;treemacs-tab-bar if you use tab-bar-mode
  :after (treemacs)
  :ensure t
  :config (treemacs-set-scope-type 'Tabs))

;; BigQuery
;; inspired by https://github.com/isamert/dotfiles/blob/master/emacs/index.org
(define-derived-mode bqsql-mode sql-mode "bqsql-mode")
(add-to-list 'auto-mode-alist (cons (rx ".bqsql" string-end) #'bqsql-mode))

(defun logc/inner-back-quote-at-point ()
  "Return text inside the back quotes at point."
  (let ((bounds (evil-inner-back-quote)))
    (buffer-substring-no-properties
     (nth 0 bounds)
     (nth 1 bounds))))

(defun logc/region-or (what)
  "Returns currently selected string or WHAT-at-point string. WHAT
can be 'symbol 'word or a function that returns string etc."
  (if (use-region-p)
      (buffer-substring-no-properties (region-beginning) (region-end))
    (if (functionp what)
        (funcall what)
      (thing-at-point what t))))

(defun logc/uuid ()
  "Generate a UUID."
  (s-trim (shell-command-to-string "uuidgen")))

(defvar-local isamert/shell-command-mode-command nil
  "Current shell command that belongs to the buffer.")

(define-minor-mode logc/shell-command-mode
  "Shell command mode."
  :lighter "iscm"
  :keymap (make-sparse-keymap))

(defun logc/zsh-smart-history ()
  (->>
   (with-temp-buffer
     (insert-file-contents "~/.zsh_history")
     (buffer-string))
   s-trim
   (s-split "\n")
   (--map (if (s-prefix? ":" it)
              (s-replace-regexp "^: [0-9:]+;" "" it)
            it))
   (--filter (and (not (s-blank? it))
                  (> (length it) 5)
                  (not (s-matches? "^\\(ls\\|pwd\\|exit\\|cd\\|echo\\)" it))))))

(cl-defun logc/shell-command
    (&key
     command
     (switch t)
     (buffer-name (concat "*" command "*"))
     (on-start (lambda (&rest _)))
     (on-finish (lambda (&rest _)))
     (on-fail (lambda (&rest _))))
  "Run given shell COMMAND and redirect output to given BUFFER-NAME.
This is a wrapper around `start-process-shell-command' that adds
support for ANSI term colors and some syntactic convenience.

When called interactively, asks for a command to run (with eshell
completion)."
  (interactive
   (let ((command (completing-read "Command: " (logc/zsh-smart-history))))
     (list
      :command command
      :on-finish
      (lambda (&rest _)
        (let ((msg (format ">> \"%s\" finished successfully." command)))
          (message msg)
          (when (not (frame-focus-state))
            (alert msg))))
      :on-fail
      (lambda (&rest _)
        (let ((msg (format ">> \"%s\" FAILED." command)))
          (message msg)
          (when (not (frame-focus-state))
            (alert msg)))))))
  (let* ((proc (start-process-shell-command command buffer-name command))
         (proc-out ""))
    (set-process-sentinel
     proc
     (lambda (p e)
       (with-current-buffer buffer-name
         (read-only-mode -1))
       (if (= 0 (process-exit-status p))
           (funcall on-finish proc-out)
         (funcall on-fail))))
    (set-process-filter
     proc
     (lambda (proc str)
       (with-current-buffer buffer-name
         (setq proc-out (concat proc-out str))
         (let ((inhibit-read-only t))
           (goto-char (point-max))
           (insert (ansi-color-apply str))))))
    (with-current-buffer buffer-name
      (read-only-mode)
      (font-lock-mode)
      (logc/shell-command-mode 1)
      (evil-normal-state)
      (setq-local
       logc/shell-command-mode-command
       (list
        :command command
        :buffer-name buffer-name
        :on-start on-start
        :on-finish on-finish
        :on-fail on-fail))
      (funcall on-start))
    (when switch
      (switch-to-buffer buffer-name))))

(defun org-babel-execute:bqsql (query params)
  "Execute bqsql (BigQuery) scripts."
  (let* ((job-id (logc/uuid))
         (format (or (alist-get :format params) "pretty"))
         (json-out? (s-matches? "json" format))
         (buf (get-buffer-create "*logc/bqsql*"))
         (org-buffer (current-buffer))
         (start-time (float-time))
         process)
    (debug)
    (with-current-buffer buf (erase-buffer))
    (setq process (start-process "query" buf "bq" "query" "--quiet" "--nouse_legacy_sql" "--format" format "--job_id" job-id query))
    (set-process-sentinel
     process
     (lambda (p m)
       (let ((end-time (float-time))
             (result (with-current-buffer buf
                       (string-trim (buffer-string)))))
         (message "=> Query finished, time elapsed: %s" (format-seconds "%Y %D %H %M %z%S" (- end-time start-time)))
         (with-current-buffer org-buffer
           (save-excursion
             (goto-char (point-max))
             (unless (re-search-backward job-id nil t)
               (let ((bname (format "*bqsql:%s" job-id)))
                 (with-current-buffer (get-buffer-create bname)
                   (insert result)
                   (org-mode))
                 (user-error "org-block is gone. Result inserted to the buffer %s" bname)))
             (forward-line -4)
             (org-babel-insert-result
              result
              (list "replace" (cond
                               ((s-prefix? "Error" result) "drawer")
                               (json-out? "lang")
                               (t "raw")))
              nil
              nil
              (when json-out? "json")))))))
    job-id))

(defun logc/big-query-job-status (job-id)
  "Get status for given job id."
  (interactive
   (list (read-string "Job ID: " (logc/region-or 'symbol))))
  (let ((buf (get-buffer-create (format "*logc/bigquery: %s*" job-id))))
    (switch-to-buffer buf)
    (insert (shell-command-to-string (format "bq show %s -j '%s'" (if current-prefix-arg "--format=prettyjson" "") job-id)))))

(defun logc/big-query-table-info (table-name)
  "Get summary information for TABLE-NAME.
This information includes schema summary, last modified date,
total {rows,bytes} etc. and first 10 rows of the table."
  (interactive
   (list (read-string
          "Table: "
          ;; Replace first `.' with `:'
          (s-replace-regexp
           "^\\([A-Za-z0-9_-]+\\)\\." "\\1:"
           (logc/region-or
            (lambda () (logc/inner-back-quote-at-point)))))))
  (let ((buffer-name (format "*bq table info: %s*" table-name)))
    (logc/shell-command
     :buffer-name buffer-name
     :command (format "bq show '%s'" table-name)
     :on-start
     (lambda (&rest _)
       (toggle-truncate-lines +1)
       (logc/shell-command
        :buffer-name buffer-name
        :command (format "bq head -n 10 '%s'" table-name))))))

;;;; Finish:
(setq gc-cons-threshold (* 2 1000 1000))

;;; init.el ends here
