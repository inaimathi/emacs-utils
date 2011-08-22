(require 'cl)
(require 'htmlize)
(require 'convenience)

(defvar blog-mode-map nil
  "Keymap for blog minor mode")

(def-sparse-map blog-mode-map
  "C-c l" insert-link
  "C-c p" insert-code-block
  "C-c c" insert-inline-code
  "C-c b" insert-bold
  "C-c i" insert-italic
  "C-c q" insert-quote
  "C-c s" insert-sig
  "C-c f" insert-footnote
  "C-c e" insert-edit
  "C-c n" insert-note
  
  "C-c C-l" region-to-link
  "C-c C-p" region-to-code-block
  "C-c C-c" region-to-inline-code
  "C-c C-b" region-to-bold
  "C-c C-i" region-to-italic
  "C-c C-q" region-to-quote
  "C-c C-s" region-to-sig
  "C-c C-f" region-to-footnote
  "C-c C-e" region-to-edit
  "C-c C-n" region-to-note
  
  "C-c g" html-escape-region
  "/" smart-backslash)


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

(defcustom blog-note-types '("beginner" "platform" "editor")
  "Completions used for the region-to-div and insert-div shortcut functions"
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
     (let ((start (region-beginning))
	   (end (region-end)))
       (goto-char end)
       (insert ,end-tag)
       (goto-char start)
       (insert ,start-tag))))

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
  (let ((n-type (completing-read "Note Type: " blog-div-classes)))
    (concat "<div class=\"note " n-type "\">\n"
	    "<h3>" (capitalize n-type) " Note</h3>\n"
	    "<span class=\"note-body\">\n"))
  "\n</span>\n</div>")

(defun insert-complete-quote (sig)
  (interactive "sSig: ")
  (insert-quote)
  (save-excursion (insert-sig)
		  (insert sig)))

(defun region-to-complete-quote (sig)
  (interactive "sSig: ")
  (region-to-quote)
  (save-excursion (insert-sig)
		  (insert sig)))
 
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