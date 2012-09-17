(require 'compile)

(defgroup ha-custom nil
  "A mode for running some IDE-like features for Haskell development."
  :group 'editing)

(defcustom ha-custom-lint-command "hlint"
  "The command to run your haskell linter"
  :type 'string
  :group 'ha-custom)

(defcustom ha-custom-hoogle-command "~/.cabal/bin/hoogle" 
  "The command to run your hoogle search (you need to `cabal install hoogle; hoogle data` before using this)"
  :type 'string
  :group 'ha-custom)

(defvar ha-custom-lint-regex
  "^\\(.*?\\):\\([0-9]+\\):\\([0-9]+\\): .*[\n\C-m]Found:[\n\C-m]\\s +\\(.*\\)[\n\C-m]Why not:[\n\C-m]\\s +\\(.*\\)[\n\C-m]"
  "Regex for HLint messages")

(defun ha-custom-lint-process-setup ()
  "Setup compilation variables and buffer for `hlint'."
  (run-hooks 'hs-lint-setup-hook))

(defun ha-custom-lint-finish-hook (buf msg)
  "Function, that is executed at the end of HLint execution"
  (if hs-lint-replace-with-suggestions
      (hs-lint-replace-suggestions)
    (next-error 1 t)))

(defun ha-custom-hoogle-doc (search-term)
  (interactive "MHoogle Search: ")
  (ha-custom-hoogle-search search-term t))

(defun ha-custom-hoogle-search (search-term &optional info)
  (interactive "MHoogle Search: ")
  (let ((b (get-buffer-create "*hoogle-search*")))
    (with-current-buffer b
      (erase-buffer)
      (insert (shell-command-to-string
	       (concat ha-custom-hoogle-command 
		       (when info " --info") 
		       " \"" search-term "\"")))
      (point-min))
    (display-buffer b)))

(defun ha-custom-flash-region (start end &optional timeout)
  "Temporarily highlight region from START to END."
  (let ((overlay (make-overlay start end))) 
    (overlay-put overlay 'face 'secondary-selection)
    (run-with-timer (or timeout 0.2) nil 'delete-overlay overlay)))

(defun ha-custom-do-next-replacement ()
  (interactive)
  (with-current-buffer (get-buffer "*ha-custom-lint*")
    (apply #'ha-custom-do-replacement (ha-custom-next-replacement))))

(defun ha-custom-do-replacement (link-position file-name old-code new-code)
  (save-excursion
    (goto-char link-position)
    (compile-goto-error)
    (with-current-buffer (get-file-buffer file-name)
      (let ((start (point)))
	(kill-region start (+ (length old-code) start))
	(insert new-code)
	(ha-custom-flash-region start (+ (length new-code) start) 1)))
    (switch-to-buffer-other-frame "*ha-custom-lint*")))

(defun ha-custom-next-replacement ()
  (interactive)
  (re-search-forward ha-custom-lint-regex nil t)
  (let ((link-position (match-beginning 0))
	(fname (match-string 1))
	(old-code (match-string 4))
	(new-code (match-string 5)))
    (goto-char (match-end 0))
    (list link-position fname old-code new-code)))

(define-compilation-mode ha-custom-lint-mode "hlint"
  "Mode for check Haskell source code."
  (set (make-local-variable 'compilation-process-setup-function)
       'ha-custom-lint-process-setup)
  (set (make-local-variable 'compilation-disable-input) t)
  (set (make-local-variable 'compilation-scroll-output) nil)
  (set (make-local-variable 'compilation-finish-functions)
       (list 'ha-custom-lint-finish-hook)))

(add-hook
 'ha-custom-lint-mode-hook
 (lambda () 
   (local-set-key (kbd "C-c n") 'ha-custom-do-next-replacement)))

(defun ha-custom-lint ()
  "Run HLint for current buffer with haskell source"
  (interactive)
  (compilation-start 
   (concat ha-custom-lint-command " " buffer-file-name)
   'ha-custom-lint-mode))

(provide 'ha-custom)