(defvar eric-excluded-packages '())
(defvar eric-packages '(company-ghc
                        company-go
                        emmet
                        go-mode
                        golint
                        magit 
                        rainbow-mode
                        zencoding-mode))

(defun eric/init-company-ghc()    (use-package company-ghc))
(defun eric/init-company-go()     (use-package company-go))
(defun eric/init-emmet()          (use-package emmet))
(defun eric/init-go-mode()        (use-package go-mode))
(defun eric/init-golint()         (use-package golint))
(defun eric/init-magit()          (use-package magit))
(defun eric/init-rainbow-mode()   (use-package rainbow-mode))
(defun eric/init-zencoding-mode() (use-package zencoding-mode))
