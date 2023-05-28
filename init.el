;;;; minimacs --- an Emacs distribution focused on minimalism

;;;; Commentary:
;;;;   inspired by Doom Emacs

;;;; Code:

(defun load-from-modules (filename)
  "Load FILENAME from the modules subdirectory."
  (load (concat user-emacs-directory "modules/" filename)))

(with-gc-cons-threshold
 (* 50 1024 1024) ;; set gc threshold temporarily to 50 MB
 (load-from-modules "preliminaries.el")
 (load-from-modules "general.el")
 (load-from-modules "evils.el")
 (load-from-modules "java.el")
 (load-from-modules "magit.el")
 (load-from-modules "orgs.el")
 (load-from-modules "python.el")
 (load-from-modules "scala.el")
 (load-from-modules "bigquery.el"))

(provide 'init)
;;; init.el ends here

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("8701800a33d6afe1588af6383c8deb7847b7fdfb8c7b134753a0b0f26524c967" "0a1b7b2d4e8931c14e40d87ac58d1e20624307d9a90643e6c96d7aaeb597762b" "34af44a659b79c9f92db13ac7776b875a8d7e1773448a8301f97c18437a822b6" "d43860349c9f7a5b96a090ecf5f698ff23a8eb49cd1e5c8a83bb2068f24ea563" default)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
