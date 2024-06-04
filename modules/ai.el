;;; general.el --- -*- lexical-binding: t; -*-
;;; Commentary:

					;(use-package copilot
					;  :straight (:host github :repo "copilot-emacs/copilot.el" :files ("*.el"))
					;  :ensure t
					;  :bind
;;  (:map copilot-mode-map
;;	      ("<tab>" . logc/copilot-tab)
;;	      ("s-j" . copilot-next-completion)
;;	      ("s-k" . copilot-previous-completion)
;;	      ("s-l" . copilot-accept-completion-by-word))
					;  (:map copilot-completion-map
					;         ("<right>" . copilot-accept-completion)
					;         ("C-f" . copilot-accept-completion)
					;	 ("C-g" . copilot-clear-overlay)
					;         ("M-<right>" . copilot-accept-completion-by-word)
					;         ("M-f" . copilot-accept-completion-by-word)
					;         ("C-e" . copilot-accept-completion-by-line)
					;         ("<end>" . copilot-accept-completion-by-line)
					;         ("M-n" . copilot-next-completion)
					;         ("M-p" . copilot-previous-completion))
					;  :hook (prog-mode . copilot-mode)
					;  :config
					;  (defun logc/copilot-tab ()
					;    (interactive)
					;    (or (copilot-accept-completion)
					;	(indent-for-tab-command))))

					;(use-package chatgpt-shell
					;  :ensure t
					;  :config
					;  (let ((api-key (plist-get
					;                  (car (auth-source-search :host "api.openai.com" :user "luis.osa.gdc@gmail.com" :require '(:user :secret)))
					;                  :secret)))
					;    (setq chatgpt-shell-openai-key (if (functionp api-key)
					;                                       (funcall api-key)
					;                                     api-key)))
					;  :bind (("C-c g" . chatgpt-shell)))

(use-package shell-maker
  :ensure t
  :straight (:host github :repo "xenodium/chatgpt-shell" :files ("shell-maker.el")))

(require 'shell-maker)

(defvar aika-shell--config
  (make-shell-maker-config
   :name "AiKA"
   :execute-command
   (lambda (command _history callback error-callback)
     (shell-command-to-string
      (format "echo \"%s\" | nc 127.0.0.1 8080" command)))))

(defun aika-shell ()
  "Start an AIKA shell."
  (interactive)
  (shell-maker-start aika-shell--config))

(defun aika-shell--extract-response (response)
					; TODO: pass through function, for now
  response)

(provide 'ai)
;;; ai.el ends here
