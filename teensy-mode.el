(require 'convenience)

(def-sparse-map (teensy-mode-map "Keymap for compiling/programming Teensy boards")
  "C-c C-c" 'teensy-compile-current-file
  "C-c C-v" 'teensy-compile-project
  "C-c C-l" 'teensy-load-hex-file
  "C-c C-k" 'teensy-compile-project-load-file)

(define-minor-mode teensy-mode
  "A collection of keyboard shortcuts geared for the Teensy USB development board"
  nil
  " Teensy"
  (use-local-map teensy-mode-map))

(defgroup teensy-mode nil
  "Additions"
  :group 'editing)

(defcustom teensy-compile-file-command "gcc"
  "The program used to compile individual files"
  :group 'teensy-mode)

(defcustom teensy-compile-project-command "make"
  "The command used to compile an entire project (the default requires you to have gnu-make and a makefile)"
  :group 'teensy-mode)

(defcustom teensy-loader-program "teensy_loader_cli"
  "The command used to load a teensy hex file onto the chip"
  :group 'teensy-mode)

(defcustom teensy-processor-type nil
  "The type of processor you are using varies depending on which Teensy chip you have. It's important that you choose the correct one, which is why no default is provided here.
atmega32u4  = Teensy 2.0
at90usb1286 = Teensy++ 2.0
at90usb162  = Teensy 1.0
at90usb646  = Teensy++ 1.0"
  :group 'teensy-mode
  :options '("atmega32u4" "at90usb1286" "at90usb162" "at90usb646"))

(defun teensy-compile-current-file ()
  (interactive)
  (shell-command (format "%s %s" teensy-compile-file-command buffer-file-name)))

(defun teensy-compile-project ()
  (interactive)
  (shell-command teensy-compile-project-command))

(defun teensy-load-hex-file ()
  (interactive)
  (let ((hex-files (directory-files (file-name-directory buffer-file-name) nil "hex$"))
	(command (format "%s -mmcu=%s -wv " teensy-loader-program teensy-processor-type)))
    (cond ((not hex-files) (message "No hex file found"))
	  ((= 1 (length hex-files)) (shell-command (concat command (car hex-files))))
	  (t (shell-command (concat command (completing-read "Hex File: " hex-files)))))))

(defun teensy-compile-project-load-file ()
  (interactive)
  (progn (teensy-compile-project)
	 (teensy-load-hex-file)))

(provide 'teensy-mode)