(require 'aes)

;;;;; Config/storage
(defvar pw-store-password-file-path "~/.passwowrd.pw"
  "Location of the password file")

(defvar pw-store-key-cache nil
  "Temporary storage of the AES key to use.")

(run-with-idle-timer
 (* 60 5) nil
 (lambda ()
   (setf pw-store-key-cache nil)))

(push '("\\.pw" . password-mode) auto-mode-alist)

(defun password-mode)

(defgroup blog-mode nil
  "Custom extensions to HTML mode geared towards blogging"
  :group 'editing)

;;;;; Keymap

(defvar pw-store-mode-map nil
  "Keymap for password storage mode.")

(let ((map (make-sparse-keymap)))
  (define-key map (kbd "C-c C-i") 'pw-store-fresh-password)
  (define-key map (kbd "C-c C-a"))
  (define-key map (kbd "C-x C-s") 'pw-store-save-encrypted))

(provide 'password-mode)

