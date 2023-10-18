;;;; minimacs --- an Emacs distribution focused on minimalism

;;;; Commentary:
;;;;   inspired by Doom Emacs

;;;; Code:

(defun load-from-modules (filename)
  "Load FILENAME from the modules subdirectory."
  (load (concat user-emacs-directory "modules/" filename)))

;(with-gc-cons-threshold
(setq gc-cons-threshold (* 100 1024 1024))
(load-from-modules "preliminaries.el")
(load-from-modules "general.el")
(load-from-modules "evils.el")
(load-from-modules "orgs.el")
(load-from-modules "java.el")
(load-from-modules "magit.el")
(load-from-modules "python.el")
(load-from-modules "scala.el")
(load-from-modules "bigquery.el")
(load-from-modules "schemes.el")
(load-from-modules "zig.el")
;)

(provide 'init)
;;; init.el ends here
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(safe-local-variable-values
   '((org-re-reveal-totaltime . 1800)
     (org-re-reveal-defaulttiming . 120))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
