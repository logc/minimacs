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
(load-from-modules "java.el")
(load-from-modules "magit.el")
(load-from-modules "orgs.el")
(load-from-modules "python.el")
(load-from-modules "scala.el")
(load-from-modules "bigquery.el")
;)

(provide 'init)
;;; init.el ends here

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("4c56af497ddf0e30f65a7232a8ee21b3d62a8c332c6b268c81e9ea99b11da0d3" "3c451787cee570710bff441154a7db8b644cdbb7d270189b2724c6041a262381" "2bf927247813d3fd7f3020eed3a411204822128c045d6cee5eb2f12b19e64f6c" "d537a9d42c6f5349d1716ae9be9a0645cc168f7aff2a8353819d570e5d02c0b3" "835d934a930142d408a50b27ed371ba3a9c5a30286297743b0d488e94b225c5f" "4c7228157ba3a48c288ad8ef83c490b94cb29ef01236205e360c2c4db200bb18" "8701800a33d6afe1588af6383c8deb7847b7fdfb8c7b134753a0b0f26524c967" "0a1b7b2d4e8931c14e40d87ac58d1e20624307d9a90643e6c96d7aaeb597762b" "34af44a659b79c9f92db13ac7776b875a8d7e1773448a8301f97c18437a822b6" "d43860349c9f7a5b96a090ecf5f698ff23a8eb49cd1e5c8a83bb2068f24ea563" default))
 '(epg-gpg-program "/opt/homebrew/bin/gpg"))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
