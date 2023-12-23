;;; orgs.el --- -*- lexical-binding: t; -*-

;;; Commentary:
;;;   configuration of org-mode

;;; Code:
(use-package org
  :bind
  ;; from: https://github.com/hlissner/doom-emacs/blob/develop/modules/lang/org/config.el
  ;; Recently, a [tab] keybind in `outline-mode-cycle-map' has begun
  ;; overriding org's [tab] keybind in GUI Emacs. This is needed to undo
  ;; that, and should probably be PRed to org.
  (:map org-mode-map
	("<tab>" . org-cycle))

  :config
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((latex . t)
     (plantuml . t)))
  ; NOTE: this is not portable
  (setq org-plantuml-jar-path "/opt/homebrew/opt/plantuml/libexec/plantuml.jar")
  ; NOTE: this might be unsafe
  (setq org-confirm-babel-evaluate nil)

  (require 'org-tempo)

  (setq org-agenda-files '("~/Dropbox/Documents/org/agendas/"))
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
	   "NO(n)")))
  (with-eval-after-load "ox-latex"
    (add-to-list 'org-latex-classes
		 '("memoir"
                 "\\documentclass{memoir}"
                 ;; ("\\part{%s}"          . "\\part*{%s}")
                 ("\\chapter{%s}"       . "\\chapter*{%s}")
                 ("\\section{%s}"       . "\\section*{%s}")
                 ("\\subsection{%s}"    . "\\subsection*{%s}")
                 ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
                 ("\\paragraph{%s}"     . "\\paragraph*{%s}")
                 ("\\subparagraph{%s}"  . "\\subparagraph*{%s}"))))

  ;; Resize headings
  (set-face-attribute 'org-level-1 nil :height 1.8)
  (set-face-attribute 'org-document-title nil :height 2.0))

(use-package htmlize)

(use-package org-re-reveal
  :defer t
  :after org
  :init
  (add-hook 'org-mode-hook (lambda () (require 'org-re-reveal)))
  :config
  (add-to-list 'org-export-backends 're-reveal)
  (setq org-re-reveal-revealjs-version "4"))

(use-package writeroom-mode
  :ensure t
  :hook ((org-mode . writeroom-mode)
	 (org-mode . visual-line-mode))
  :after org
  :config
  (setq writeroom-width 120))

(use-package mixed-pitch
  :hook
  (writeroom-mode . mixed-pitch-mode)
  :config
  (setq mixed-pitch-set-height t)
  (set-face-attribute 'variable-pitch nil :font "Charter" :height 180)
  (set-face-attribute 'fixed-pitch nil :height 180)
)

(use-package org-superstar              ; supersedes `org-bullets'
  :ensure
  :after org
  :hook (org-mode . org-superstar-mode)
  :config
  (setq org-superstar-headline-bullets-list '("◈" "◉" "○" "▷")) ;; '(" ")
  (setq org-superstar-remove-leading-stars t)
  (setq org-superstar-item-bullet-alist
        '((?+ . ?•)
          (?* . ?➤)
          (?- . ?–))))

;; This function is here because I intend to use it for org-capture ...
(defun copy-file-link-to-clipboard ()
  "Copy current line in file to clipboard as 'file:</path/to/file>::<line-number>'."
  (interactive)
  (let ((path-with-line-number
         (concat "file:" (buffer-file-name) "::" (number-to-string (line-number-at-pos)))))
    (kill-new path-with-line-number)
    (message (concat path-with-line-number " copied to clipboard"))))
;;; orgs.el ends here
