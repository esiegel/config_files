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
    dotspacemacs-excluded-packages '()

    ; List of contribution to load."
    dotspacemacs-configuration-layers '(colors
                                        auto-completion
                                        clojure
                                        emacs-lisp
                                        fasd
                                        floobits
                                        git
                                        go
                                        haskell
                                        html
                                        javascript
                                        markdown
                                        python
                                        restclient
                                        ruby
                                        scala
                                        shell
                                        syntax-checking
                                        themes-megapack)
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
    (eric/init-node)
    (eric/init-ruby)
    (eric/init-scala)
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

(defun eric/init-node ()
    ; add tern to the path
    (add-to-list 'exec-path "~/code/tern/bin")
)

(defun eric/init-ruby ()
    (setq-default ruby-version-manager 'rbenv)
    (setq-default ruby-enable-ruby-on-rails-support t)
)

(defun eric/init-scala ()
  (setq-default flycheck-scala-executable "/usr/local/bin/scalastyle")
  (setq-default flycheck-scalastyle-jar "/usr/local/Cellar/scalastyle/0.7.0/libexec/scalastyle_2.11-0.7.0-batch.jar")

  ; ensime
  (setq-default ensime-sem-high-enabled-p nil) ; Don't add highlighting on save
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
    (eric/config-git)
    (eric/config-markdown)
    (eric/config-neotree)
    (eric/config-org-mode)
    (eric/config-projectile-mode)
    (eric/config-rainbow-identifiers)
    (eric/config-repls)
    (eric/config-ruby)
    (eric/config-scrolling)
    (eric/config-searching)
    (eric/config-snippets)
    (eric/config-ssh)
    (eric/config-theme)
)

(defun eric/config-variables()
    ; always follow symlinks in git repos, don't ask
    (setq vc-follow-symlinks t)
)

(defun eric/config-mappings()
    ; fullscreen
    (define-key evil-normal-state-map (kbd "<s-return>") 'spacemacs/toggle-frame-fullscreen)

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
    (define-key evil-normal-state-map (kbd ",b")  'helm-buffers-list)
    
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

    ; emulate vim's splitright behavior.
    (setq evil-vsplit-window-right t)
)

(defun eric/config-completion ()
    (setq company-idle-delay nil)          ;; only auto-complete on key binding
    (setq company-tooltip-limit 20)        ;; use a bigger popup window
    (setq company-selection-wrap-around t) ;; wrap selection
    (setq ac-auto-start nil)               ;; only auto-complete on key binding
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

(defun eric/config-git ()
  (setq git-enable-github-support)
)

(defun eric/config-markdown ()
  ; use pandoc to create markdown html
  (setq markdown-command "pandoc --smart --standalone -f markdown_github -t html5")
)

(defun eric/config-neotree ()
  ; allow closing neotree with ':q' when only one other window
  (setq neo-dont-be-alone nil)
  (setq git-gutter-use-fringe nil)
)

(defun eric/config-org-mode ()
  (setq org-startup-indented t)
  (setq org-hide-leading-stars nil)
  (setq org-fontify-done-headline nil)
  (setq org-fontify-emphasized-text nil)
  (setq org-fontify-entities nil)
  (setq org-fontify-like-in-org-mode nil)
  (setq org-fontify-meta-lines-and-blocks nil)
  (setq org-fontify-meta-lines-and-blocks-1 nil)
  (setq org-fontify-quote-and-verse-blocks nil)
  (setq org-fontify-whole-heading-line nil)
)

(defun eric/config-projectile-mode ()
  ; load projectile mode
  ;; (if (not (bound-and-true-p projectile-mode))
  ;;   (projectile-mode))

  ; use helm for projectile completions
  (setq projectile-completion-system 'helm-comp-read)

  ; projectile considers current directory as root
  (setq projectile-require-project-root nil)
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

(defun eric/config-ruby ()
  ;; ruby mode, and extended ruby mode no magic encoding comment
  (setq ruby-insert-encoding-magic-comment nil)
  (setq enh-ruby-add-encoding-comment-on-save nil)
)

(defun eric/config-scrolling ()
  ; smooth-scrolling is enabled by default
  ; change margins to something smaller
  (setq smooth-scroll-margin 3)
)

(defun eric/config-searching ()
  ; TODO, ag-arguements hasn't loaded yet.
  ; search all file types
  ;(add-to-list 'ag-arguments "-a")
)

(defun eric/config-snippets ()
    (setq yas-snippet-dirs
      '("~/.emacs.d/spacemacs/extensions/yasnippet-snippets"))

    ; use the html-mode when in web-mode
    (add-hook 'web-mode-hook #'(lambda () (yas-activate-extra-mode 'html-mode)))
)

(defun eric/config-ssh()
    ; use ssh by default
    (setq tramp-default-method "ssh")
    (setq tramp-shell-prompt-pattern
          "\\(?:^\\|\\)[^]#$%>\n]*#?[]#$%>].* *\\(\\[[0-9;]*[a-zA-Z] *\\)*")
)

(defun eric/config-theme()
    (if _is_term
        (spacemacs/load-theme 'sanityinc-tomorrow-night)
        (spacemacs/load-theme 'sanityinc-tomorrow-night))
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
          ((not shell-window) (set-window-buffer nil shell-buffer))
          (t (select-window shell-window) shell-buffer))
  ))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;; AUTOGENERATED
;;;;;;;;;;; Autogenerated variables ;;;;;;;;;;
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ac-ispell-requires 4 t)
 '(ahs-case-fold-search nil t)
 '(ahs-default-range (quote ahs-range-whole-buffer) t)
 '(ahs-idle-interval 0.25 t)
 '(ahs-idle-timer 0 t)
 '(ahs-inhibit-face-list nil t)
 '(custom-safe-themes
   (quote
    ("fc5fcb6f1f1c1bc01305694c59a1a861b008c534cae8d0e48e4d5e81ad718bc6" "97a2b10275e3e5c67f46ddaac0ec7969aeb35068c03ec4157cf4887c401e74b1" default)))
 '(package-selected-packages
   (quote
    (clojure-mode ghc sbt-mode haskell-mode helm-core osx-trash jbeans-theme helm-flx helm-company evil-mc auto-compile avy anzu smartparens projectile yasnippet json-reformat zonokai-theme zenburn-theme zen-and-art-theme window-numbering web-mode web-beautify volatile-highlights vi-tilde-fringe underwater-theme ujelly-theme twilight-theme twilight-bright-theme twilight-anti-bright-theme tronesque-theme toxi-theme tao-theme tangotango-theme tango-plus-theme tango-2-theme tagedit sunny-day-theme sublime-themes subatomic256-theme subatomic-theme stekene-theme spray spaceline spacegray-theme soothe-theme soft-stone-theme soft-morning-theme soft-charcoal-theme smyx-theme smooth-scrolling smeargle slim-mode shm shell-pop seti-theme scss-mode sass-mode ruby-tools ruby-test-mode robe reverse-theme reveal-in-osx-finder restclient rbenv rainbow-mode rainbow-identifiers rainbow-delimiters pyvenv pytest pyenv-mode purple-haze-theme professional-theme popwin planet-theme pip-requirements phoenix-dark-pink-theme phoenix-dark-mono-theme pcre2el pbcopy pastels-on-dark-theme paradox page-break-lines organic-green-theme open-junk-file oldlace-theme occidental-theme obsidian-theme noflet noctilux-theme niflheim-theme neotree naquadah-theme mustang-theme multi-term move-text monochrome-theme molokai-theme moe-theme mmm-mode minimal-theme material-theme markdown-toc markdown-mode magit-gitflow magit macrostep lush-theme linum-relative light-soap-theme leuven-theme less-css-mode launchctl json-mode js2-refactor js2-mode js-doc jazz-theme jade-mode ir-black-theme inkpot-theme info+ indent-guide ido-vertical-mode hy-mode hungry-delete hindent highlight-parentheses highlight-numbers highlight-indentation heroku-theme hemisu-theme helm-themes helm-swoop helm-pydoc helm-projectile helm-mode-manager helm-make helm-gitignore helm-descbinds helm-dash helm-css-scss helm-c-yasnippet helm-ag helm hc-zenburn-theme haskell-snippets haml-mode gruvbox-theme gruber-darker-theme grandshell-theme gotham-theme google-translate golden-ratio go-eldoc gitignore-mode gitconfig-mode gitattributes-mode git-timemachine git-messenger git-commit gh-md gandalf-theme flycheck-pos-tip flycheck-haskell flycheck flx-ido floobits flatui-theme flatland-theme firebelly-theme fill-column-indicator fasd fancy-battery expand-region exec-path-from-shell evil-visualstar evil-tutor evil-surround evil-search-highlight-persist evil-numbers evil-nerd-commenter evil-matchit evil-lisp-state evil-jumper evil-indent-textobject evil-iedit-state evil-exchange evil-escape evil-args evil-anzu espresso-theme eshell-prompt-extras esh-help ensime enh-ruby-mode emmet-mode elisp-slime-nav django-theme define-word dash-at-point darktooth-theme darkmine-theme darkburn-theme dakrone-theme cython-mode cyberpunk-theme company-web company-tern company-statistics company-quickhelp company-go company-ghc company-cabal company-anaconda company colorsarenice-theme color-theme-sanityinc-tomorrow color-theme-sanityinc-solarized coffee-mode cmm-mode clues-theme clj-refactor clean-aindent-mode cider-eval-sexp-fu cider cherry-blossom-theme busybee-theme bundler buffer-move bubbleberry-theme birds-of-paradise-plus-theme auto-yasnippet auto-highlight-symbol auto-dictionary apropospriate-theme anti-zenburn-theme anaconda-mode ample-zen-theme ample-theme align-cljlet alect-themes aggressive-indent afternoon-theme adaptive-wrap ace-window ace-link ac-ispell evil-leader evil which-key quelpa package-build use-package bind-key spacemacs-theme s dash)))
 '(paradox-github-token t)
 '(ring-bell-function (quote ignore))
 '(truncate-lines t))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

