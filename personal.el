;;; personal --- personal Emacs configuration


; set up package repositories
(require 'package)
(add-to-list 'package-archives
             '("marmalade" . "http://marmalade-repo.org/packages/")
             '("melpa" . "http://melpa.milkbox.net/"))
(package-initialize)

; enable evil mode
(require 'evil)
(evil-mode 1)

; remap org-mode meta keys for convenience
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

; EVIL escape as C-j
(defun my-esc (prompt)
  (cond
    ((or (evil-insert-state-p)
         (evil-normal-state-p)
         (evil-replace-state-p)
         (evil-visual-state-p)) [escape])
    (t (kbd "C-g"))))

(define-key key-translation-map (kbd "C-j") 'my-esc)
(define-key evil-operator-state-map (kbd "C-j") 'keyboard-quit)
(set-quit-char "C-j")

;; No wrap lines
(setq-default truncate-lines t)

;; solaraized theme
(load-theme 'solarized-dark)

;; Font size
(set-face-attribute 'default nil :height 160)

;; disallow keychords as jj is mapped to ace search
(key-chord-mode 0)

;; No tabs
(setq tab-width 4)
(setq-default indent-tabs-mode nil)

;; No whitespace mode
;; (add-hook 'prog-mode-hook 'prelude-turn-off-whitespace t)

(add-hook 'neotree-mode-hook
          (lambda ()
            (define-key evil-normal-state-local-map (kbd "TAB") 'neotree-enter)
            (define-key evil-normal-state-local-map (kbd "SPC") 'neotree-enter)
            (define-key evil-normal-state-local-map (kbd "q") 'neotree-hide)
            (define-key evil-normal-state-local-map (kbd "RET") 'neotree-enter)))


(provide 'personal)
;;; personal.el ends here
