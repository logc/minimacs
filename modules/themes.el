;;; themes.el --- -*- lexical-binding: t; -*-

;;; Commentary:
;;;  general configuration settings for theming

;;; Code:
(use-package doom-themes
  :ensure t
  :config
  ;; Global settings (defaults)
  (setq doom-themes-enable-bold t    ; if nil, bold is universally disabled
        doom-themes-enable-italic t) ; if nil, italics is universally disabled

  ;; Enable flashing mode-line on errors
  (doom-themes-visual-bell-config)

  ;; Corrects (and improves) org-mode's native fontification.
  ;; (doom-themes-org-config)
  ;; :init
  ;; (load-theme 'doom-oksolar-dark)
  )

(use-package color-theme-sanityinc-tomorrow)

(use-package solaire-mode
  :ensure t
  :config
  (solaire-global-mode +1))

(use-package modus-themes
  :custom
  (modus-themes-custom-auto-reload t)
  (modus-themes-italic-constructs t)
  (modus-themes-bold-constructs nil)
  (modus-themes-mixed-fonts t)
  (modus-themes-variable-pitch-ui t)
  (modus-themes-custom-auto-reload t)
  (modus-themes-disable-other-themes t)
  (modus-themes-prompts '(italic bold))
  ( modus-themes-completions
    '((matches . (extrabold))
      (selection . (semibold italic text-also))))
  (modus-themes-org-blocks 'gray-background) ; {nil,'gray-background,'tinted-background}
  (modus-themes-headings
   '((1 . (variable-pitch 1.5))
     (2 . (1.1))
     (agenda-date . (1.3))
     (agenda-structure . (variable-pitch light 1.8))
     (t . (1.1))))
  (modus-themes-to-toggle '(modus-operandi-tinted modus-vivendi-tinted)))

(use-package ef-themes
  :defer t
  :ensure t
  :config
  (setq ef-themes-mixed-fonts t)
  :init
  (ef-themes-load-random 'dark))
;;; themes.el ends here
