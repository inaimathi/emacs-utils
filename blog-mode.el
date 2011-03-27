(require 'htmlize)

(defvar blog-mode-map nil
  "Keymap for blog minor mode")

(unless blog-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "\C-cl" 'insert-link)
    (define-key map "\C-cp" 'insert-code-block)
    (define-key map "\C-cc" 'insert-inline-code)
    (define-key map "\C-cb" 'insert-bold)
    (define-key map "\C-cq" 'insert-quote)
    (define-key map "\C-cs" 'insert-sig)
    (define-key map "\C-cf" 'insert-footnote)
    (define-key map "\C-ce" 'insert-edit)
    
    (define-key map "\C-c\C-l" 'region-to-link)
    (define-key map "\C-c\C-p" 'region-to-code-block)
    (define-key map "\C-c\C-c" 'region-to-inline-code)
    (define-key map "\C-c\C-b" 'region-to-bold)
    (define-key map "\C-c\C-q" 'region-to-quote)
    (define-key map "\C-c\C-s" 'region-to-sig)
    (define-key map "\C-c\C-f" 'region-to-footnote)
    (define-key map "\C-c\C-e" 'region-to-edit)

    (define-key map "/" 'smart-backslash)
    (setq blog-mode-map map)))

(define-minor-mode blog-mode
  "This is a collection of useful keyboard macros for editing Langnostic"
  nil
  " Blog"
  (use-local-map blog-mode-map))

(defgroup blog-mode nil
  "Additions to Emacs git mode"
  :group 'editing)

(defcustom blog-footnote-header "<hr />\n<h5>Footnotes</h5>"
  "The string used to delimit footnotes from the rest of the blog post. 
   If you change this, make it something fairly unique or you'll run into obvious trouble."
  :group 'blog-mode)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; simple definitions
(defmacro definsert (tag-name start-tag end-tag)
  "Defines function to insert tag surrounding point."
  `(defun ,(make-symbol (concat "insert-" (symbol-name tag-name))) ()
     (interactive)
     (insert ,start-tag)
     (save-excursion (insert ,end-tag))))

(defmacro defregion (tag-name start-tag end-tag)
  "Defines region wrapper function."
  `(defun ,(make-symbol (concat "region-to-" (symbol-name tag-name))) ()
     (interactive)
     (goto-char (region-end))
     (insert ,end-tag)
     (goto-char (region-beginning))
     (insert ,start-tag)))

(defmacro deftag (tag-name start-tag end-tag)
  "Shortcut for tags that have standard defregion and definsert definitions"
  `(progn
     (defregion ,tag-name ,start-tag ,end-tag)
     (definsert ,tag-name ,start-tag ,end-tag)))

(deftag link (concat "<a href=\"" (x-get-clipboard) "\">") "</a>")
(deftag bold "<b>" "</b>")
(deftag quote "<blockquote>" "</blockquote>")
(deftag sig "<span class=\"sig\">" "</span>")
(deftag edit "<span class=\"edit\">EDIT:\n\n" (concat "\n" (format-time-string "%a, %d %b, %Y" (current-time)) "</span>"))

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

(defun get-htmlified-region (start end &optional code-mode)
  "Returns a string of the current region HTMLized with highlighting according to code-mode"
  (let ((htmlified nil))
    (clipboard-kill-ring-save start end)
    (get-buffer-create "*blog-mode-temp*") ;;using 'with-temp-buffer here doesn't apply correct higlighting
    (with-current-buffer "*blog-mode-temp*"
      (if (fboundp code-mode) (funcall code-mode))
      (clipboard-yank)
      (setq htmlified (substring (htmlize-region-for-paste (point-min) (point-max)) 6 -6)))
    (kill-buffer "*blog-mode-temp*")
    htmlified))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; footnote definitions
(defun insert-footnote ()
  "Inserts footnote, and a return link at the bottom of the file. 
   Moves point to footnote location."
  (interactive)
  (progn (footnotes-header)
	 (let ((footnote-name (format-time-string "%a-%b-%d-%H%M%S%Z-%Y" (current-time)))
	       (num (number-to-string (+ 1 (count-footnotes)))))
	   (insert "<a href=\"#foot-" footnote-name "\" name=\"note-" footnote-name "\">[" num "]</a>")
	   (goto-char (point-max))
	   (insert "\n\n" num " - <a href=\"#note-" footnote-name "\" name=\"foot-" footnote-name "\">[back]</a> - "))))

(defun region-to-footnote ()
  "Inserts a footnote at point and return link at the bottom. Moves the current region to the end of the file. 
   Leaves point where it is."
  (interactive)
  (save-excursion (kill-region (region-beginning) (region-end))
	 (insert-footnote)
	 (yank)))

(defun footnotes-header ()
  "Inserts footnote header if not already present"
  (unless (save-excursion (search-forward blog-footnote-header nil t))
    (save-excursion 
      (goto-char (point-max))
      (insert "\n\n" blog-footnote-header))))

(defun count-footnotes ()
  "Returns the number of footnotes in the current file. Used for human-readable note labels"
  (interactive)
  (save-excursion
    (if (not (search-forward blog-footnote-header nil t))
	0
      (let ((count -1))
	(while (progn (setq count (1+ count))
		      (search-forward "<a href=\"#note-" nil t)))
	count))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; utility and general primitives
(defun smart-backslash ()
  "Backslash closes previous tag when used in the combination </. Self-inserts otherwise."
  (interactive)
  (if (equal (save-excursion (backward-char) (thing-at-point 'char)) "<")
      (progn (backward-delete-char 1)
	     (sgml-close-tag))
    (insert "/")))

(provide 'blog-mode)