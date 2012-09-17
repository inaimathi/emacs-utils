(require 'cl)
(require 'htmlize)
(require 'convenience)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; special character related
(defcustom blog-special-chars
  (list :copyright "©" :trademark "™" :registered "®" :interrobang "‽" :irony "؟")
  "Commonly used (for blogging purposes) spechial characters."
  :group 'blog-mode)

(defvar blog-mode-special-char-map nil
  "Character insertion submap for Blog Mode")

(defun insert-special (char)
  `(lambda ()
     (interactive)
     (insert ,(getf blog-special-chars char))))

(let ((map (make-sparse-keymap)))
  (define-key map (kbd "c") (insert-special :copyright))
  (define-key map (kbd "t") (insert-special :trademark))
  (setf blog-mode-special-char-map map))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; keymap and other customs

(defvar blog-mode-map nil
  "Keymap for blog minor mode")
(let ((map (make-sparse-keymap))) 
  (define-key map (kbd "C-c s") blog-mode-special-char-map)
  (define-key map (kbd "C-c l") 'insert-link)
  (define-key map (kbd "C-c p") 'insert-code-block)
  (define-key map (kbd "C-c c") 'insert-inline-code)
  (define-key map (kbd "C-c b") 'insert-bold)
  (define-key map (kbd "C-c i") 'insert-italic)
  (define-key map (kbd "C-c q") 'insert-complete-quote)
  (define-key map (kbd "C-c f") 'insert-footnote)
  (define-key map (kbd "C-c e") 'insert-edit)
  (define-key map (kbd "C-c n") 'insert-note)  
  
  (define-key map (kbd "C-c C-l") 'region-to-link)
  (define-key map (kbd "C-c C-p") 'region-to-code-block)
  (define-key map (kbd "C-c C-c") 'region-to-inline-code)
  (define-key map (kbd "C-c C-b") 'region-to-bold)
  (define-key map (kbd "C-c C-i") 'region-to-italic)
  (define-key map (kbd "C-c C-q") 'region-to-complete-quote)
  (define-key map (kbd "C-c C-f") 'region-to-footnote)
  (define-key map (kbd "C-c C-e") 'region-to-edit)
  (define-key map (kbd "C-c C-n") 'region-to-note)
  
  (define-key map (kbd "C-c RET") 'blog-html-paragraph)
  (define-key map (kbd "C-c g") 'html-escape-region)
  (define-key map (kbd "/") 'smart-backslash)
  (define-key map (kbd ">") 'smart-brace)
  (setq blog-mode-map map))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Temp function until I put together a proper footnote system

(defun sanitize-footnotes ()
  (interactive)
  (query-replace-regexp " (\\(.\\)\\(.*\\))" (upcase "\\1\\2.")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(define-minor-mode blog-mode
  "This is a collection of useful keyboard macros for editing Langnostic"
  nil
  " Blog"
  (use-local-map blog-mode-map))

(defgroup blog-mode nil
  "Custom extensions to HTML mode geared towards blogging"
  :group 'editing)

(defcustom blog-footnote-header "<hr />\n<h5>Footnotes</h5>"
  "The string used to delimit footnotes from the rest of the blog post. 
   If you change this, make it something fairly unique or you'll run into obvious trouble."
  :group 'blog-mode)

(defcustom blog-note-types '("beginner" "platform" "editor")
  "Completions used for the region-to-div and insert-div shortcut functions"
  :group 'blog-mode)

(defcustom blog-default-blogger-title nil
  "the default blog to post to using interactive invocation of `blog-post-to-blogger`"
  :group 'blog-mode)

(defcustom blog-default-blogger-user nil
  "The default blog to post to using interactive invocation of `blog-post-to-blogger`"
  :group 'blog-mode)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; googlecl-related
(defun blog-post-to-blogger (file-name post-title blogger-user blogger-blog-title)
  "Posts the current article to Blogger.
  Uses blog-default-blogger-title and blog-default-blogger-user to specify the user account and blog.
  This assumes that you've given googlecl permission to use 
your google account manually (that is, through terminal)"
  (interactive (list (buffer-file-name) (read-string "Post Title: ")
		     blog-default-blogger-user blog-default-blogger-title))
  (cond ((not file-name) 
	 (message "You need to save your post before I can post it."))
	((not (and blogger-user blogger-blog-title)) 
	 (message "Please specify the blogger-user and blogger-title (or customize the appropriate variables in blog-mode)"))
	((and file-name post-title blogger-user blogger-blog-title)
	 (shell-command
	  (format "google -u '%s' blogger --blog='%s' post --title '%s' %s" 
		  blogger-user blogger-blog-title post-title 
		  (car (last (split-string file-name "/"))))))
	(t (message "Error posting for some weird reason."))))

(defun blog-delete-blog-post (post-title blogger-user blogger-blog-title)
  "Deletes the specified blog post from the specified blog."
  (interactive (list (read-string "Post Title: ") ;; doesn't use the list as completions because `google blogger list` is slower than a fucking glacier 
		     blog-default-blogger-user blog-default-blogger-title))
  (shell-command (format "google -u '%s' blogger --blog='%s' delete --title '%s'"
			 blogger-user blogger-blog-title post-title)))

(defun blog-list-blog-posts (blogger-blog-title blogger-user)
  "Returns a list of posts published under the given blog.
Warning: SLOW AS FUCK"
  (interactive (list blog-default-blogger-title blog-default-blogger-user))
  (shell-command (format "google -u '%s' blogger --blog='%s' list" blogger-user blogger-blog-title)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; simple definitions
(defmacro definsert (tag-name start-tag end-tag)
  "Defines function to insert tag surrounding point."
  `(defun ,(make-symbol (concat "insert-" (symbol-name tag-name))) ()
     (interactive)
     (insert ,start-tag)
     (save-excursion (insert ,end-tag))))

(defmacro defregion (tag-name start-tag end-tag)
  "Defines region wrapper function."
  `(defun ,(make-symbol (concat "region-to-" (symbol-name tag-name))) (start end)
     (interactive "r")
     (goto-char end) (insert ,end-tag)
     (goto-char start) (insert ,start-tag)))

(defmacro deftag (tag-name start-tag end-tag)
  "Shortcut for tags that have standard defregion and definsert definitions"
  `(progn
     (defregion ,tag-name ,start-tag ,end-tag)
     (definsert ,tag-name ,start-tag ,end-tag)))

(deftag link (concat "<a href=\"" (x-get-clipboard) "\">") "</a>")
(deftag bold "<b>" "</b>")
(deftag italic "<i>" "</i>")
(deftag quote "<blockquote>" "</blockquote>")
(deftag sig "<span class=\"sig\">" "</span>")
(deftag edit "<span class=\"edit\">EDIT:\n\n" (concat "\n" (format-time-string "%a, %d %b, %Y" (current-time)) "</span>"))
(deftag note 
  (let ((n-type (completing-read "Note Type: " blog-note-types)))
    (concat "<div class=\"note " n-type "\">\n"
	    "<h3>" (capitalize n-type) " Note</h3>\n"
	    "<span class=\"note-body\">\n"))
  "\n</span>\n</div>")

(defun blog-html-paragraph ()
  "Custom definition for html-modes' html-paragraph (the default doesn't auto-close the tag)"
  (interactive)
  (insert "<p>")
  (save-excursion (insert "</p>")))

(defun insert-complete-quote (sig)
  (interactive "sSig: ")
  (insert-quote)
  (save-excursion (insert-sig)
		  (insert sig)))

(defun region-to-complete-quote (signature)
  (interactive "sSig: ")
  (let ((start (region-beginning))
	(end (region-end)))
    (save-excursion
      (goto-char end)
      (save-excursion (insert "</blockquote>"))
      (insert-sig)
      (insert "-")
      (insert signature))
    (insert "<blockquote>")))
 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; <pre> and <code> definitions
(definsert code-block "<pre>" "</pre>")
(definsert inline-code "<code>" "</code>")

;; region versions are more complicated to accomodate htmlize
(defun region-to-inline-code (code-mode)
  "HTMLize just the current region and wrap it in a <code> block"
  (interactive "CMode name: ")
  (htmlized-region code-mode #'insert-inline-code))

(defun region-to-code-block (code-mode)
  "HTMLize the current region and wrap it in a <pre> block"
  (interactive "CMode name: ")
  (htmlized-region code-mode #'insert-code-block))

(defun htmlized-region (code-mode insert-fn)
  (let* ((start (region-beginning))
	 (end (region-end))
	 (result (get-htmlified-region start end code-mode)))
    (delete-region start end)
    (funcall insert-fn)
    (insert result)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; footnote definitions
(defun insert-footnote ()
  "Inserts footnote, and a return link at the bottom of the file. 
   Moves point to footnote location."
  (interactive)
  (progn (footnotes-header)
	 (let ((footnote-name (format-time-string "%a-%b-%d-%H%M%S%Z-%Y" (current-time)))
	       (num (number-to-string (+ 1 (count-footnotes)))))
	   (insert "<a href=\"#foot-" footnote-name "\" name=\"note-" footnote-name "\">[" num "]</a>")
	   (goto-char (+ 1 (buffer-size)))
	   (insert "\n")
	   (blog-html-paragraph)
	   (insert num " - <a href=\"#note-" footnote-name "\" name=\"foot-" footnote-name "\">[back]</a> - "))))

(defun region-to-footnote ()
  "Inserts a footnote at point and return link at the bottom. Moves the current region to the end of the file. 
   Leaves point where it is."
  (interactive)
  (save-excursion (kill-region (region-beginning) (region-end))
		  (insert-footnote)
		  (yank)))

(defun footnotes-header ()
  "Inserts footnote header if not already present"
  (unless (save-excursion (goto-char 1) (search-forward blog-footnote-header nil t))
    (save-excursion 
      (goto-char (point-max))
      (insert "\n\n" blog-footnote-header))))

(defun count-footnotes ()
  "Returns the number of footnotes in the current file. Used for human-readable note labels"
  (interactive)
  (save-excursion
    (if (not (progn (goto-char 1) (search-forward blog-footnote-header nil t)))
	0
      (let ((count -1))
	(while (progn (setq count (1+ count))
		      (search-forward "<a href=\"#note-" nil t)))
	count))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; utility and general primitives
(defregion strikethru "<span style=\"text-decoration: line-through;\">" "</span>")

(defun smart-backslash ()
  "Backslash closes previous tag when used in the combination </. Self-inserts otherwise."
  (interactive)
  (if (equal (save-excursion (backward-char) (thing-at-point 'char)) "<")
      (progn (backward-delete-char 1)
	     (sgml-close-tag))
    (insert "/")))

(defun smart-brace ()
  "Closing pointy brace closes SGML tag."
  (interactive)
  (insert ">")
  (save-excursion (sgml-close-tag)))

;;; line converters (these are specific enough that I don't assign hotkeys, just use the M-x command)
(defun region-to-paragraphs ()
  (interactive)
  (region-to-tag "p"))

(defun region-to-ul ()
  (interactive)
  (region-to-tag "li" "ul"))

(defun region-to-ol ()
  (interactive)
  (region-to-tag "li" "ol"))

(defun region-to-tag (line-tag-name &optional wrapper-tag-name)
  (interactive "sTag: ")
  (let* ((start (region-beginning))
	 (end (region-end))
	 (line-count (count-lines start end)))
    (goto-char start)
    (when wrapper-tag-name (insert "<" wrapper-tag-name ">\n"))
    (loop for i from 1 to line-count
    	  do (progn (end-of-line)
    		    (insert "</" line-tag-name ">")
    		    (beginning-of-line)
    		    (insert "<" line-tag-name ">")
    		    (forward-line)))
    (when wrapper-tag-name (insert "</" wrapper-tag-name ">\n"))))

;;; for editing docs from sales
(defun html-escape-region ()
  "Function mostly used for escaping .DOCs from marketing for use in HTML"
  (interactive)
  (htmlized-region nil #'insert))

(defun get-htmlified-region (start end &optional code-mode)
  "Returns a string of the current region HTMLized with highlighting according to code-mode"
  (let ((htmlified nil))
    (clipboard-kill-ring-save start end)
    (get-buffer-create "*blog-mode-temp*") ;;using 'with-temp-buffer here doesn't apply correct higlighting
    (with-current-buffer "*blog-mode-temp*"
      (when (fboundp code-mode) (funcall code-mode))
      (clipboard-yank)
      (unless code-mode 
	(fundamental-mode)
	(font-lock-fontify-buffer))
      (setq htmlified (substring (htmlize-region-for-paste (point-min) (point-max)) 6 -6)))
    (kill-buffer "*blog-mode-temp*")
    htmlified))

(provide 'blog-mode)