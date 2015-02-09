;; Helpers
;; --------------------
(defvar-local _is_mac   (string-equal system-type "darwin"))
(defvar-local _is_linux (string-equal system-type "gnu/linux"))
(defvar-local _is_term  (not window-system))
(defvar-local _is_gui   (not _is_term))

;; Configuration Layers
;; --------------------

(setq-default
   ; Disable auto-complete and dependencies so that we can use company for completion
   dotspacemacs-excluded-packages '(auto-complete
                                    ac-ispell
                                    auto-complete-clang
                                    enslime
                                    edts
                                    tern-auto-complete)

    ; List of contribution to load."
    dotspacemacs-configuration-layers '(colors 
                                        company-mode
                                        fasd
                                        git
                                        go
                                        haskell
                                        html
                                        javascript
                                        python
                                        restclient
                                        scala
                                        themes-megapack
                                        eric)
)

; OSX custom config
(if _is_mac
    (progn
      (add-to-list 'dotspacemacs-configuration-layers 'dash)
      (add-to-list 'dotspacemacs-configuration-layers 'osx))
  )

;; Spacemacs Settings
;; Configuration for spacemacs that must run before init and config
;; --------------------

(defun eric/dotspacemacs-settings ()
    (if _is_mac (eric/dotspacemacs-settings-mac))
    (if _is_linux (eric/dotspacemacs-settings-linux))
)

(defun eric/dotspacemacs-settings-mac ()
    (setq-default dotspacemacs-default-font '("Monaco" :size 16))
)

(defun eric/dotspacemacs-settings-linux ()
    (setq-default dotspacemacs-default-font '("DejaVu Sans Mono" :size 11))
)

; Actual do the set the settigs.
(eric/dotspacemacs-settings)


;; Spacemacs Init
;; --------------------

(defun dotspacemacs/init ()
    (if _is_mac (eric/init-mac))
    (if _is_linux (eric/init-linux))

    (eric/init-go)
    (eric/init-haskell)
)

(defun eric/init-mac ()
    ; Don't use slow OSX fullscreen
    (setq ns-use-native-fullscreen nil)
)

(defun eric/init-linux ()
)

(defun eric/init-go ()
    ; add GOPATH env var.
    (setenv "GOPATH" "/Users/eric/code/go")
)

(defun eric/init-haskell ()
    ; add cabal to executable path
    (add-to-list 'exec-path "~/.cabal/bin")
)

;; Spacemacs Config
;; --------------------

(defun dotspacemacs/config ()
    (eric/config-variables)
    (eric/config-mappings)

    (eric/config-buffers)
    (eric/config-completion)
    (eric/config-dired)
    (eric/config-emmet)
    (eric/config-flymake)
    (eric/config-markdown)
    (eric/config-rainbow-identifiers)
    (eric/config-repls)
    (eric/config-scrolling)
    (eric/config-snippets)
    (eric/config-ssh)
    (eric/config-theme)
)

(defun eric/config-variables()
    ; always follow symlinks in git repos, don't ask
    (setq vc-follow-symlinks t)
)

(defun eric/config-mappings()
    ; show functions and variables
    (define-key evil-normal-state-map (kbd ", RET") 'helm-semantic-or-imenu)

    ; remove highlights
    (define-key evil-normal-state-map (kbd ", SPC") 'evil-search-highlight-persist-remove-all)

    ; show shell
    (define-key evil-normal-state-map (kbd ",s") 'eric/show-or-create-shell)
)

(defun eric/config-buffers ()
    ; ctrl h,l for fast buffer changing.
    (define-key evil-normal-state-map (kbd "C-h") 'evil-prev-buffer)
    (define-key evil-normal-state-map (kbd "C-l") 'evil-next-buffer)

    ; open up buffer list faster
    (define-key evil-normal-state-map (kbd ",b")    'helm-buffers-list)
    
    ; next-buffer should skip all *BUFFERS*
    (defadvice next-buffer (after avoid-messages-buffer-in-next-buffer)
      "Advice around `next-buffer' to avoid going into the *Messages* buffer."
      (when (eric/should-ignore-buffers (buffer-name))
        (next-buffer)))

    ; prev-buffer should skip all *BUFFERS*
    (defadvice previous-buffer (after avoid-messages-buffer-in-previous-buffer)
      "Advice around `previous-buffer' to avoid going into the *Messages* buffer."
      (when (eric/should-ignore-buffers (buffer-name))
        (previous-buffer)))

    ; activate our advice
    (ad-activate 'next-buffer)
    (ad-activate 'previous-buffer)
)

(defun eric/config-completion ()
    ; snippet or completion expansion
    (defun company-yasnippet-or-completion ()
      (interactive)
      (print "doing stuff")
      (if (yas/expansion-at-point)
          (progn (company-abort)
                 (yas/expand))
        (company-complete-common)))

    (defun yas/expansion-at-point ()
      (first (yas--templates-for-key-at-point)))

    ; config
    (setq company-idle-delay nil)   ;; only auto-complete on key binding
    (setq company-tooltip-limit 20) ;; use a bigger popup window
    (setq company-selection-wrap-around t) ;; wrap selection

    ; mappings
    (define-key evil-insert-state-map (kbd "C-M-i")     'company-complete)
    (define-key evil-insert-state-map (kbd "<tab>")     'company-yasnippet-or-completion)
    (define-key company-active-map    (kbd "<tab>")     'company-select-next)
    (define-key company-active-map    (kbd "<backtab>") 'company-select-previous)
)

(defun eric/config-dired()
    ; omit unintersting files in directory mode
    (setq dired-omit-mode t)

    ; change uninteresting regexp match.  I like seeing the ".." directory.
    ; see also dired-omit-extensions
    (setq dired-omit-files "^\\.?#\\|^\\.$")
)

(defun eric/config-emmet ()
  (add-hook 'sgml-mode-hook 'emmet-mode) ;; Auto-start on any markup modes
  (add-hook 'css-mode-hook  'emmet-mode) ;; enable Emmet's css abbreviation.
  (setq emmet-preview-default t)         ;; show preview by default
)

(defun eric/config-flymake ()
  ; E203 - whitespace before ':'
  ; E221 - multiple spaces before operator.  Nice to lineup =.
  ; E241 - multiple spaces after :.  Nice to lineup dicts.
  ; E272 - multiple spaces before keyword.  Nice to lineup import.
  ; W404 - import *, unable to detected undefined names.
  ; W801 - redefinition of unused import, try/except import fails.
  (setq flycheck-flake8rc "~/.flake8rc")
  (setq flymake-python-pyflakes-extra-arguments
        '(
          "--ignore=E203,E221,E241,E272,W404,W801"
          "--max-line-length=99"
         ))
)

(defun eric/config-markdown ()
  ; use pandoc to create markdown html
  (setq markdown-command "pandoc --smart --standalone -f markdown_github -t html5")
)

(defun eric/config-rainbow-identifiers ()

    ;; Customized filter: don't mark *all* identifiers
    ;; http://amitp.blogspot.com/2014/09/emacs-rainbow-identifiers-customized.html
    (defun rainbow-identifiers-filter (beg end)
      "Only highlight standalone words or those following 'this.' or 'self.'"
      (let ((curr-char (char-after beg))
            (prev-char (char-before beg))
            (prev-self (buffer-substring-no-properties
                        (max (point-min) (- beg 5)) beg)))
        (and (not (member curr-char 
                        '(?0 ?1 ?2 ?3 ?4 ?5 ?6 ?7 ?8 ?9 ??)))
             (or (not (equal prev-char ?\.))
                 (equal prev-self "self.")
                 (equal prev-self "this.")))))

    ;; Filter: don't mark identifiers inside comments or strings
    (setq rainbow-identifiers-faces-to-override
          '(font-lock-type-face
            font-lock-variable-name-face
            font-lock-function-name-face))

    ;; Set the filter
    (add-hook 'rainbow-identifiers-filter-functions 'rainbow-identifiers-filter)

    ;; Use a wider set of colors
    (setq rainbow-identifiers-choose-face-function 'rainbow-identifiers-cie-l*a*b*-choose-face)
    (setq rainbow-identifiers-cie-l*a*b*-lightness 45)
    (setq rainbow-identifiers-cie-l*a*b*-saturation 45)
)

(defun eric/config-repls()
    ; Have the up and down arrows get previous history in shell
    (define-key comint-mode-map (kbd "<up>") 'comint-previous-input)
    (define-key comint-mode-map (kbd "<down>") 'comint-next-input)
)

(defun eric/config-scrolling ()
  ; smooth-scrolling is enabled by default
  ; change margins to something smaller
  (setq smooth-scroll-margin 3)
)

(defun eric/config-snippets ()
    (setq yas-snippet-dirs
      '("~/.emacs.d/spacemacs/extensions/yasnippet-snippets"))
)

(defun eric/config-ssh()
    ; use ssh by default
    (setq tramp-default-method "ssh")
    (setq tramp-shell-prompt-pattern
          "\\(?:^\\|\\)[^]#$%>\n]*#?[]#$%>].* *\\(\\[[0-9;]*[a-zA-Z] *\\)*")
)

(defun eric/config-theme()
    (if _is_term
        (spacemacs/load-theme 'monokai)
        (spacemacs/load-theme 'solarized-dark))
)

;; Spacemacs Util
;; --------------------

(defun eric/should-ignore-buffers (buffer)
  ; ignore buffers that start and end with *.
  (string-match-p "\\*.*\\*" buffer))

(defun eric/get-buffer-index-by-name (name)
  (-find-index (lambda (b) (equal name (buffer-name b))) (buffer-list)))

(defun eric/get-buffer-by-name (name)
  (let ((shell-index (eric/get-buffer-index-by-name name)))
    (if shell-index (nth shell-index (buffer-list)) nil)))

(defun eric/show-or-create-shell ()
  (interactive)
  (let* ((shell-buffer (eric/get-buffer-by-name "*shell*"))
         (shell-window (get-buffer-window shell-buffer))
         (current-window (get-buffer-window)))
    (cond ((not shell-buffer) (shell))
          ((not shell-window) (set-buffer shell-buffer))
          (t (select-window shell-window) shell-buffer))
  ))




























;;;;;;;;;;;;;;;;;;;;;;;;;;;;; AUTOGENERATED
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
 '(company-tooltip-common ((t (:inherit company-tooltip :weight bold :underline nil))))
 '(company-tooltip-common-selection ((t (:inherit company-tooltip-selection :weight bold :underline nil)))))

