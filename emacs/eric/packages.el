(defvar eric-excluded-packages '())
(defvar eric-packages '(rust-mode
                        skewer-mode))

(defun eric/init-rust-mode()   (use-package rust-mode))
(defun eric/init-skewer-mode() (use-package skewer-mode))
