(defvar eric-excluded-packages '())
(defvar eric-packages '(company-ghc
                        company-go
                        emmet-mode
                        go-mode
                        golint
                        magit 
                        rainbow-mode))

(defun eric/init-company-ghc()    (use-package company-ghc))
(defun eric/init-emmet-mode()     (use-package emmet-mode))
(defun eric/init-magit()          (use-package magit))
(defun eric/init-rainbow-mode()   (use-package rainbow-mode))

;; go packages
(defun eric/init-go-mode() (use-package go-mode))
(defun eric/init-golint()  (use-package golint))
(defun eric/init-company-go()
  ; company-go by default will load before go-mode is loaded.
  (eval-after-load 'go-mode '(require 'company-go)))
