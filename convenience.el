(require 'cl)

;;; lisp basics
(defun macroexpand-point (sexp)
  (interactive (list (sexp-at-point)))
  (with-output-to-temp-buffer "*el-macroexpansion*"
    (pp (macroexpand sexp)))
  (with-current-buffer "*el-macroexpansion*" (emacs-lisp-mode)))

;;; key and mode declaration shortcuts
(defmacro def-sparse-map (name/doc &rest key/fn-list)
  (assert (and (listp name/doc)
	       (symbolp (first name/doc))
	       (or (not (second name/doc))
		   (stringp (second name/doc)))))
  `(defvar ,(first name/doc)
     (keys (make-sparse-keymap) ,@key/fn-list)
     ,@(cdr name/doc)))

(defmacro keys (keymap &rest key/fn-list)
  `(let ((map ,keymap))
     ,@(loop for (key fn) on key/fn-list by #'cddr
	     collect `(define-key map (kbd ,key) ,fn))
     map))

(defmacro hooks (mode-name/s function)
  (assert (or (symbolp mode-name/s) (listp mode-name/s)))
  (flet ((to-hook (name) (intern (format "%s-mode-hook" name))))
    (if (symbolp mode-name/s)
	`(add-hook ',(to-hook mode-name/s) ,function)
      (let ((fn (gensym)))
	`(let ((,fn ,function))
	   ,@(loop for m in mode-name/s
		   collect `(add-hook ',(to-hook m) ,fn)))))))

(defmacro by-ext (extension/s mode)
  (assert (or (stringp extension/s) (listp extension/s)))
  (flet ((to-reg (ext) (format "\\.%s$" ext)))
    (if (stringp extension/s)
	`(add-to-list 'auto-mode-alist '(,(to-reg extension/s) . ,mode))
      `(progn ,@(loop for ext in extension/s
		      collect `(add-to-list 'auto-mode-alist 
					    '(,(to-reg ext) . ,mode)))))))

(defmacro global-mode (mode-name) ;;shortcut for globalizing a minor mode (since I do it more than once)
  (let ((g-name (make-symbol (concat "global-" (symbol-name mode-name)))))
    `(progn 
       (define-globalized-minor-mode ,g-name 
	   ,mode-name
	 (lambda () (,mode-name t)))
       (,g-name t))))

;;; basic word/char count
(defun word-count () ;; note that Emacs doesn't count hyphenated words as a single word
  (interactive)
  (count-text 'forward-word "words"))

(defun char-count ()
  (interactive)
  (count-text 'forward-char "characters"))

(defun count-text (inc-function items)
  (save-excursion
    (beginning-of-buffer)
    (message "%d %s"
	     (loop for count from 0 do (funcall inc-function) if (eobp) return count)
	     items)))

;;; Other
(defun file-nameify-string (a-string)
  (replace-regexp-in-string
   " +" "-"
   (replace-regexp-in-string 
    "[',`~_\"]" ""
    (replace-regexp-in-string 
     "part[ 	]\\([0-9]+\\)" "p\\1" 
     (downcase a-string))))) 

(provide 'convenience)

