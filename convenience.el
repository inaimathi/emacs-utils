(require 'cl)

;;; lisp basics
(defun macroexpand-point (sexp)
  (interactive (list (sexp-at-point)))
  (with-output-to-temp-buffer "*el-macroexpansion*"
    (pp (macroexpand sexp)))
  (with-current-buffer "*el-macroexpansion*" (emacs-lisp-mode)))

;;; key and mode declaration shortcuts
(defmacro def-sparse-map (name &rest key/fn-list)
  `(when (not ,name)
     (let ((map (make-sparse-keymap)))
       ,@(loop for (key fn) on key/fn-list by #'cddr
	       collecting `(define-key map (kbd ,key) ',fn))
       (setq ,name map))))

(defmacro add-global-keys (&rest key/fn-list)
  (progn ,@(loop for (key fn) on key/fn-list by #'cddr
		 collecting `(global-set-key (kbd ,key) ',fn))))

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
 
(provide 'convenience)

