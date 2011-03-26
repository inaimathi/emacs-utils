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

    (define-key map "\C-c\C-l" 'region-to-link)
    (define-key map "\C-c\C-p" 'region-to-code-block)
    (define-key map "\C-c\C-c" 'region-to-inline-code)
    (define-key map "\C-c\C-b" 'region-to-bold)
    (define-key map "\C-c\C-q" 'region-to-quote)
    (define-key map "\C-c\C-s" 'region-to-sig)
    (define-key map "\C-c\C-f" 'region-to-footnote)

    (define-key map "/" 'smart-backslash)
    (setq blog-mode-map map)))

(define-minor-mode blog-mode
  "This is a collection of useful keyboard macros for editing Langnostic"
  nil
  " Blog"
  (use-local-map blog-mode-map))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; simple definitions
(defun insert-tag (start-tag &optional end-tag)
  "Inserts a tag at point"
  (interactive)
  (insert start-tag)
  (save-excursion
    (insert (or end-tag ""))))

(defun wrap-region (start end start-tag &optional end-tag)
  "Inserts end tag at the end of the region, and start tag at point"
  (goto-char end)
  (insert (or end-tag ""))
  (goto-char start)
  (insert start-tag))

(defmacro definsert (tag-name start-tag end-tag)
  "Defines insert function."
  `(defun ,(make-symbol (concat "insert-" (symbol-name tag-name))) ()
     (interactive)
     (insert-tag ,start-tag ,end-tag)))

(defmacro defregion (tag-name start-tag end-tag)
  "Defines region wrapper function."
  `(defun ,(make-symbol (concat "region-to-" (symbol-name tag-name))) ()
     (interactive)
     (wrap-region (region-beginning) (region-end) ,start-tag ,end-tag)))

(definsert link (concat "<a href=\"" (x-get-clipboard) "\">") "</a>")
(defregion link (concat "<a href=\"" (x-get-clipboard) "\">") "</a>")
(definsert bold "<b>" "</b>")
(defregion bold "<b>" "</b>")
(definsert quote "<blockquote>" "</blockquote>")
(defregion quote "<blockquote>" "</blockquote>")
(definsert sig "<span class=\"sig\">" "</span>")
(defregion sig "<span class=\"sig\">" "</span>")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; <pre> and <code> definitions
(definsert code-block "<pre>" "</pre>")
(definsert inline-code "<code>" "</code>")

;; region versions are more complicated to accomodate htmlize
(defun region-to-inline-code ()
  "HTMLize just the current region and wrap it in a <code> block"
  (interactive)
  (let((htmlified (substring (htmlize-region-for-paste (region-beginning) (region-end)) 6 -6)))
    (delete-region (region-beginning) (region-end))
    (insert-inline-code)
    (insert htmlified)))

(defun region-to-code-block ()
  "HTMLize the current region and wrap it in a <pre> block"
  (interactive)
  (let ((htmlified (htmlize-region-for-paste (region-beginning) (region-end))))
    (delete-region (region-beginning) (region-end))
    (insert (concat "<pre>" (substring htmlified 6)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; footnote definitions
(defun insert-footnote ()
  "Inserts footnote, and a return link at the bottom of the file. 
   Moves point to footnote location."
  (interactive)
  (progn (footnotes-header)
	 (let ((footnote-name (format-time-string "%a-%b-%d-%H%M%S%Z-%Y" (current-time))))
	   (insert "<a href=\"#foot-" footnote-name "\" name=\"note-" footnote-name "\">[note]</a>")
	   (goto-char (point-max))
	   (insert "\n\n<a href=\"#note-" footnote-name "\" name=\"foot-" footnote-name "\">[back]</a> - "))))

(defun region-to-footnote ()
  "Inserts a footnote at point and return link at the bottom. Moves the current region to the end of the file. 
   Leaves point where it is."
  (interactive)
  (save-excursion (kill-region (region-beginning) (region-end))
	 (insert-footnote)
	 (yank)))

(defun footnotes-header ()
  "Inserts footnote header if not already present"
  (unless (save-excursion (search-forward "<hr />\n<h5>Footnotes</h5>" nil t))
    (save-excursion 
      (goto-char (point-max))
      (insert "\n\n<hr />\n<h5>Footnotes</h5>"))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; utility
(defun smart-backslash ()
  "Backslash closes previous tag when used in the combination </. Self-inserts otherwise."
  (interactive)
  (if (equal (save-excursion (backward-char) (thing-at-point 'char)) "<")
      (progn (backward-delete-char 1)
	     (sgml-close-tag))
    (insert "/")))

(provide 'blog-mode)