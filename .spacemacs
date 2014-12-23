(defun dotspacemacs/init ()
    ; Set font
    (spacemacs/set-font 'Monaco' 18)

    ; Disable auto-complete and dependencies so that we can use company for completion
    (setq-default dotspacemacs-excluded-packages
      '(auto-complete ac-ispell tern-auto-complete auto-complete-clang enslime edts))

    ; List of contribution to load."
    (setq dotspacemacs-configuration-layers
       '(company-mode haskell javascript python themes-megapack eric))

    ; Don't use slow OSX fullscreen
    (setq ns-use-native-fullscreen nil)
)

; Runs after dotspacemacs is loaded.
(defun dotspacemacs/config ()
    ; THEMES
    (load-theme 'solarized-dark' true)

    ; ignore files when viewing directories.
    ; (setq-default dired-omit-files-p t)
    ; (setq dired-omit-files "^\\.[^.]\\|\\.pyc$\\")
    
    ; SNIPPETS
    (setq yas-snippet-dirs
        '("~/.emacs.d/spacemacs/extensions/yasnippet-snippets"
          "/Users/eric/.emacs.d/elpa/haskell-mode-20141206.1533/snippets"
          "/Users/eric/code/yasnippet-snippets"))

    ; COMPANY-MODE
    (setq company-idle-delay nil)   ;; only auto-complete on key binding
    (setq company-tooltip-limit 20) ;; use a bigger popup window
    (setq company-mod )

    ; snippet or completion expansion
    (defun company-yasnippet-or-completion ()
      (interactive)
      (if (yas/expansion-at-point)
          (progn (company-abort)
                 (yas/expand))
        (company-complete-common)))

    (defun yas/expansion-at-point ()
      (first (yas--templates-for-key-at-point)))

    (define-key company-active-map (kbd "TAB") 'company-yasnippet-or-completion)
    (define-key company-active-map (kbd "<tab>") 'company-yasnippet-or-completion)
    (define-key company-active-map "\t" 'company-yasnippet-or-completion)
)

;;;;;;;;;;; Autogenerated variables ;;;;;;;;;;
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ac-ispell-requires 4)
 '(ahs-case-fold-search nil)
 '(ahs-default-range (quote ahs-range-whole-buffer))
 '(ahs-idle-interval 0.25)
 '(ahs-idle-timer 0 t)
 '(ahs-inhibit-face-list nil)
 '(custom-safe-themes
   (quote
    ("fc5fcb6f1f1c1bc01305694c59a1a861b008c534cae8d0e48e4d5e81ad718bc6" "97a2b10275e3e5c67f46ddaac0ec7969aeb35068c03ec4157cf4887c401e74b1" default)))
 '(paradox-github-token t)
 '(ring-bell-function (quote ignore) t)
 '(truncate-lines t))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:background nil))))
 '(company-tooltip-common ((t (:inherit company-tooltip :weight bold :underline nil))))
 '(company-tooltip-common-selection ((t (:inherit company-tooltip-selection :weight bold :underline nil)))))
