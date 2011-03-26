(defun create-tag-table ()
  "This will recursively tag all files of a given type starting with the current buffers' directory. 
   It overwrites the old TAGS file, if one exists. 
   Haskell files assume you've installed `hasktags`."
  (interactive)
  (let ((file-type (get-taggable-extension))
	(cur-dir default-directory)
	(tags-file (find-parent-tags default-directory)))
    (if (equalp file-type "hs")
    	(shell-command "hasktags --ignore-close-implementation --etags `find . -type f -name \"*.*hs\"`")
      (shell-command (concat "find -name \"*." file-type "\" -print | etags -")))))

(defun find-parent-tags (dir)
  "Traverses the directory tree up to /home/[user]/ or / whichever comes first. 
   Returns either nil or the directory containing the first TAGS file it finds."
  (interactive (list default-directory))
  (find-parent-tags-rec (build-tag-paths dir)))

(defun find-parent-tags-rec (list-of-filepath)
  (cond ((null list-of-filepath) nil)
	((file-exists-p (car list-of-filepath)) (car list-of-filepath))
	(t (find-parent-tags-rec (cdr list-of-filepath)))))

(defun build-tag-paths (dir-string)
  (build-tag-paths-rec (remove-if #'blank-string? (split-string dir-string "/")) (list "/")))

(defun build-tag-paths-rec (steps acc)
  (if (null steps) 
      (mapcar (lambda (p) (concat p "TAGS")) acc)
    (build-tag-paths-rec (cdr steps)
			 (cons (concat (car acc) (car steps) "/") acc))))

(defun blank-string? (s) (equalp s ""))

(defun get-taggable-extension ()
  "Either returns the current file's extension (if it's appropriate) or asks the user to pick one with completion"
  (let ((b-name (buffer-file-name (current-buffer)))
	(valid-exts (list "lisp" "py" "c" "hs" "rb" "ss" "scm" "js" "erl" "el")))
    (string-match "\\.\\(.*?\\)$" b-name)
    (let ((current-filetype (match-string 1 b-name)))
      (if (member current-filetype valid-exts)
	  current-filetype
	(completing-read "File type: " valid-exts nil 'confirm)))))

(provide 'tagariffic)