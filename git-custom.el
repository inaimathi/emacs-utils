;;Addendum to the built-in GIT library for Emacs
(add-hook 'git-status-mode-hook
	  (lambda () 
	    (define-key git-status-mode-map "\C-p" 'git-pull)
	    (define-key git-status-mode-map "\S-p" 'git-push)
	    (define-key git-status-mode-map "\M-f" 'git-svn-fetch)
	    (define-key git-status-mode-map "\M-d" 'git-svn-dcommit)
	    (define-key git-status-mode-map "\C-d" 'git-diff-with-revisions)
	    (define-key git-status-mode-map "\C-l" 'git-log)
	    (define-key git-status-mode-map "\S-l" 'git-log-full)
	    (define-key git-status-mode-map "\C-b" 'git-branch)
	    (define-key git-status-mode-map "\S-b" 'git-branch-d)
	    (define-key git-status-mode-map "e" 'git-merge)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; group and customizations
(defgroup git-custom nil
  "Additions to Emacs git mode"
  :group 'convenience)

(defcustom project-directory "~/" 
  "The directory to scan for git-pull-completions"
  :type 'string :group 'git-custom)
(defcustom additional-directories '("/home/inaimathi" "home") 
  "Directories to add apart from the ones in project-directory"
  :type '(repeat string) :group 'git-custom)
(defcustom machine-addresses '("inaimathi@192.168.1.2" "inaimathi@192.168.1.3" "git@orphan" "repos@jupiter") 
  "Standard targets to complete to."
  :type '(repeat string) :group 'git-custom)
(defcustom github-addresses '("git@github.com:Inaimathi/") 
  "GitHub repos to complete to"
  :type '(repeat string) :group 'git-custom)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; set git-pull-completion
(defun combine (hosts dirs &optional sep)
  (apply #'append (mapcar (lambda (a-host) 
			    (mapcar (lambda (a-dir) (concat a-host (or sep ":") a-dir)) 
				    dirs)) 
			  hosts)))

(defun filter (predicate lst &optional acc)
  (cond ((not lst) (reverse acc))
	((funcall predicate (car lst)) (filter predicate (cdr lst) (cons (car lst) acc)))
	(t (filter predicate (cdr lst) acc))))

(defvar git-pull-completion 
  (let ((local-dirs (append additional-directories (filter (lambda (n) (file-directory-p (concat project-directory n))) (directory-files project-directory nil "^[^.#%A-Z]")))))
    (append (list "master" "backup")
	    (combine machine-addresses local-dirs)
	    (combine github-addresses (mapcar (lambda (d) (concat d ".git")) local-dirs) ""))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; additional function definitions
;;;;;;;;;; branching/merging
(defun git-branch (branch)
  "Create a branch from the current HEAD and switch to it."
  (interactive (list (git-read-commit-name "Branch: ")))
  (unless git-status (error "Not in git-status buffer."))
  (if (git-rev-parse (concat "refs/heads/" branch))
      (git-call-process-display-error "checkout" branch)
    (and (git-call-process-display-error "branch" branch)
	 (git-call-process-display-error "checkout" branch)))
    (git-refresh-ewoc-hf git-status))

(defun git-branch-d (branch)
  "Delete specified branch"
  (interactive (list (git-read-commit-name "Delete branch: ")))
  (unless git-status (error "Not in git-status buffer."))
  (git-call-process-display-error "branch" "-D" branch))

(defun git-merge (branch)
  "Merge the given branch into the current branch (optionally delete the given branch)"
  (interactive (list (git-read-commit-name "Merge with: ")))
  (unless git-status (error "Not in git-status buffer."))
  (when (git-call-process-display-error "merge" branch)
    (when (y-or-n-p (concat "Delete " branch "?: ")) (git-call-process-display-error "branch" "-d" branch))))

;;;;;;;;;; git-svn interactions
(defun git-svn-fetch ()
  (interactive)
  (unless git-status (error "Not in git-status buffer"))
  (git-run-command-buffer "*git-svn-fetch*" "svn" "fetch")
  (display-buffer "*git-svn-fetch*"))

(defun git-svn-dcommit ()
  (interactive)
  (unless git-status (error "Not in git-status buffer"))
  (git-run-command-buffer "*git-svn-dcommit*" "svn" "dcommit")
  (display-buffer "*git-svn-dcommit*"))

;;;;;;;;;; additional log options
(defun git-log-full ()
  "Display full log for the current repo (the main point is the timestamps)"
  (interactive)
  (let ((buffer (apply #'git-run-command-buffer "*git-log*" '("log"))))
    (with-current-buffer buffer
      (goto-char (point-min))
      (setq buffer-read-only t))
    (display-buffer buffer)))

(defun git-log ()
  "Display log for marked files in oneline format. If no files are marked, it displays the full log for this repo instead of the log for the file near point."
  (interactive)
  (let* ((files (just-marked-files))
         (coding-system-for-read git-commits-coding-system)
         (buffer (apply #'git-run-command-buffer "*git-log*" "log" "--pretty=oneline" (when files (cons "--" (git-get-filenames files))))))
    (with-current-buffer buffer
      (goto-char (point-min))
      (setq buffer-read-only t))
    (display-buffer buffer)))

(defun just-marked-files ()
  "Returns marked files. Returns NIL if no files are marked (instead of returning the file at point)"
  (ewoc-collect git-status (lambda (info) (git-fileinfo->marked info))))

;;;;;;;;;; diff changes
(defun git-diff-with-revisions (revisions)
  (interactive (list (completing-read-multiple "Revision number: " (git-log-completions))))
  (let* ((files (just-marked-files))
	 (f-names (when files (cons "--" (git-get-filenames files))))
	 (buffer (cond ((or (not (car revisions)) (string= (car revisions) ""))
			(apply #'git-run-command-buffer "*git-diff*" "diff" "HEAD" f-names))
		       ((not (cadr revisions))
			(apply #'git-run-command-buffer "*git-diff*" "diff" (car revisions) (car (git-log-completions)) f-names))
		       (t 
			(apply #'git-run-command-buffer "*git-diff*" "diff"  (car revisions) (cadr revisions) f-names)))))
    (switch-to-buffer "*git-diff*")
    (goto-char (point-min))
    (diff-mode)
    (display-buffer "*git-diff*")))

(defun git-log-completions ()
  "Generates hash completions from the current log"
  (git-log)
  (with-current-buffer "*git-log*"
    (mapcar (lambda (x) (when (>= (string-width x) 40) (substring x 0 40)))
	    (split-string (buffer-string) "\n"))))

;;;;;;;;;; pull/push additions (incorporating git-pull-completions)
(defun git-pull (remote-dir)
  "Pulls from a directory. Typical input is 'user@remote-machine:directory,master'"
  (interactive 
   (list (let ((input (completing-read-multiple "Pull from: " git-pull-completion)))
	   (if (= 1 (length input))
	       (append input '("master"))
	     input))))
  (unless git-status (error "Not in git-status buffer"))
  (apply 'git-call-process-display-error "pull" remote-dir)
  (display-buffer "*Git Command Output*")
  (git-refresh-files)
  (git-refresh-status))

(defun git-push (remote-dir)
  "Pushes to a foreign repo"
  (interactive 
   (list (let ((input (completing-read-multiple "Push to: " git-pull-completion)))
	   (if (= 1 (length input))
	       (append input '("master"))
	     input))))
  (unless git-status (error "Not in git-status buffer"))
  (apply 'git-call-process-display-error "push" remote-dir))

;;;;;;;;;; basics (rarely used)
(defun git-add-file ()
  "Add marked file(s) to the index cache."
  (interactive)
  (let ((files (git-get-filenames (git-marked-files-state 'unknown 'ignored 'unmerged))))
    ;; now works on directories too
    (unless files
      (push (file-relative-name (read-file-name "File to add: " nil nil t)) files))
    (when (apply 'git-call-process-display-error "add" "-f" files)
      (git-update-status-files files)
      (git-success-message "Added" files)
      (git-refresh-status))))

(defun git-init (directory)
  "Initializes the given directory as a GIT repo, then runs git-status on it"
  (interactive "GSelect directory: ")
  (unless (file-directory-p directory)
    (make-directory directory))
  (let ((default-directory directory))
    (when (git-call-process-display-error "init")
      (git-status directory))))

(defun git-clone (remote-dir)
  "Clones remote directory in the local directory"
  (interactive 
   (list (let ((input (completing-read-multiple "Clone from: " git-pull-completion)))
	     input)))
  (apply 'git-call-process-display-error "clone" remote-dir)
  (git-status (cadr (split-string (car remote-dir) ":"))))

(provide 'git-custom)