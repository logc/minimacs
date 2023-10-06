;;; magit.el --- -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:
(use-package magit)

(use-package forge
  :after magit
  :defer t
  :config
  (add-to-list 'forge-alist
	       '("ghe.spotify.net" "ghe.spotify.net/api/v3"
		 "ghe.spotify.net" forge-github-repository)))

(use-package git-timemachine)
;;; magit.el ends here
