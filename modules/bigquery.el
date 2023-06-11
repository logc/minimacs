;;; bigquery.el --- -*- lexical-binding: t; -*-

;;; Commentary:
;;;  configure packages and functions to work with BigQuery
;;;  inspired by https://github.com/isamert/dotfiles/blob/master/emacs/index.org

;;; Code:
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

;;; bigquery.el ends here
