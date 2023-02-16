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
 (load-from-modules "theme.el")
 (load-from-modules "java.el")
 (load-from-modules "magit.el")
 (load-from-modules "orgs.el")
 (load-from-modules "bigquery.el"))

