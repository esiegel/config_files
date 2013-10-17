;;; personal --- personal emacs config

;;; Commentary:
;; Using the prelude init scripts, https://github.com/bbatsov/prelude,
;; this will be loaded.

;;; Code:

;; set up repos
(require 'package)
(add-to-list 'package-archives
             '("marmalade" .  "http://marmalade-repo.org/packages/")
             '("melpa" .  "http://melpa.milkbox.net/"))
(package-initialize)

;; enable evil mode
(require 'evil)
(evil-mode 1)

;; remap org-mode meta keys for convenience
(mapcar (lambda (state)
          (evil-declare-key state org-mode-map
            (kbd "M-l") 'org-metaright
            (kbd "M-h") 'org-metaleft
            (kbd "M-k") 'org-metaup
            (kbd "M-j") 'org-metadown
            (kbd "M-L") 'org-shiftmetaright
            (kbd "M-H") 'org-shiftmetaleft
            (kbd "M-K") 'org-shiftmetaup
            (kbd "M-J") 'org-shiftmetadown))
        '(normal insert))

;; No wrap lines
(setq-default truncate-lines t)

;; custom code
(custom-set-variables
 '(custom-safe-themes
   (quote ("d677ef584c6dfc0697901a44b885cc18e206f05114c8a3b7fde674fce6180879"
           "e16a771a13a202ee6e276d06098bc77f008b73bbac4d526f160faa2d76c1dd0e"
           "8aebf25556399b58091e533e455dd50a6a9cba958cc4ebb0aab175863c25b9a4"
           default))))

(custom-set-faces)

;; solaraized theme
(load-theme 'solarized-dark)

;; Font size
(set-face-attribute 'default nil :height 160)

;; disallow keychords as jj is mapped to ace search
(key-chord-mode 0)

;; No tabs
(setq tab-width 4)
(setq-default indent-tabs-mode nil)

(provide 'personal)
;;; personal.el ends here
