;; -*- mode: emacs-lisp -*-


;; Helpers
;; --------------------
(defvar-local _is_mac   (string-equal system-type "darwin"))
(defvar-local _is_linux (string-equal system-type "gnu/linux"))
(defvar-local _is_term  (not window-system))
(defvar-local _is_gui   (not _is_term))

;; Spacemacs config
;; --------------------

(defun dotspacemacs/layers ()
  "Configuration Layers declaration.
You should not put any user code in this function besides modifying the variable
values."
  (setq-default

   dotspacemacs-distribution 'spacemacs

   ;; Ask to lazy install packages that aren't explicitly defined.
   dotspacemacs-enable-lazy-installation 'unused
   dotspacemacs-ask-for-lazy-installation t

   ;; If non-nil layers with lazy install support are lazy installed.
   ;; List of additional paths where to look for configuration layers.
   ;; Paths must have a trailing slash (i.e. `~/.mycontribs/')
   dotspacemacs-configuration-layer-path '()

   dotspacemacs-configuration-layers
   '(
     (auto-completion :variables
                      auto-completion-enable-help-tooltip t
                      auto-completion-enable-snippets-in-popup t)
     better-defaults
     c-c++
     clojure
     colors
     command-log
     elm
     emacs-lisp
     fasd
     floobits
     (git :variables
          git-magit-status-fullscreen t
          git-enable-github-support t
          git-gutter-use-fringe t)
     (go :variables
         go-tab-width 2
         go-use-gometalinter t)
     haskell
     helm
     html
     imenu-list
     javascript
     markdown
     osx
     python
     ranger
     react
     restclient
     ruby
     ruby-on-rails
     rust
     rusteric
     (scala :variables
            scala-enable-eldoc nil
            )
     search-engine
     (shell :variables
            shell-default-height 30
            shell-default-shell 'eshell
            shell-default-term-shell "/bin/zsh"
            shell-default-position 'bottom
            )
     spell-checking
     sql
     (syntax-checking :variables
                      syntax-checking-enable-tooltips t)
     themes-megapack
     typescript
     version-control
     vimscript
     )

   ;; List of additional packages that will be installed without being
   dotspacemacs-additional-packages '(kite)

   ;; A list of packages that cannot be updated.
   dotspacemacs-frozen-packages '()

   ;; A list of packages that will not be installed and loaded.
   dotspacemacs-excluded-packages '()

   ;; installs only explicitly used packages and uninstall any unused
   dotspacemacs-install-packages 'used-only))

(defun dotspacemacs/init ()
  "Called at the very startup of Spacemacs initialization before layers configuration."

  (setq-default
   ;; no banner logo at startup
   dotspacemacs-startup-banner nil

   ;; configure startup buffer lists
   dotspacemacs-startup-lists '((recents . 5)
                                (projects . 7)
                                (bookmarks . 5)
                                (todos . 5))

   ;; The leader key accessible in `emacs state' and `insert state'
   dotspacemacs-emacs-leader-key "M-m"

   ;; Major mode leader key shortcut, same as `<leader> m`.
   dotspacemacs-major-mode-leader-key ","

   ;; Major mode leader key accessible in `emacs state' and `insert state'.
   dotspacemacs-major-mode-emacs-leader-key "C-M-m"

   ;; If non-nil `Y' is remapped to `y$' in Evil states. (default nil)
   dotspacemacs-remap-Y-to-y$ t

   ;; Size (in MB) above which spacemacs will prompt to open the large file
   ;; literally to avoid performance issues. Opening a file literally means that
   ;; no major mode or minor modes are active. (default is 1)
   dotspacemacs-large-file-size 10

   ;; Which-key delay in seconds. The which-key buffer is the popup listing
   ;; the commands bound to the current keystroke sequence. (default 0.4)
   dotspacemacs-which-key-delay 0.4

   ;; If non-nil `spacemacs/toggle-fullscreen' will not use native fullscreen.
   ;; Use to disable fullscreen animations in OSX. (default nil)
   dotspacemacs-fullscreen-use-non-native t

   ;; transparency of frame
   dotspacemacs-active-transparency 100
   dotspacemacs-inactive-transparency 100

   ;; If non-nil line numbers are turned on in all `prog-mode' and `text-mode'
   ;; derivatives. If set to `relative', also turns on relative line numbers.
   dotspacemacs-line-numbers nil

   ;; Code folding method. Possible values are `evil' and `origami'.
   dotspacemacs-folding-method 'evil
   ))

;; Spacemacs Init (before layers load)
;; --------------------

(defun dotspacemacs/user-init ()
  "Initialization function for user code. It is called immediately after `dotspacemacs/init'."

  (if _is_mac (eric/init-mac))
  (if _is_linux (eric/init-linux))

  ;; disable loading shell variables.  Need to move zshrc to .zshenv for exports.
  (setq exec-path-from-shell-check-startup-files nil)

  (eric/init-go)
  (eric/init-haskell)
  (eric/init-node)
  (eric/init-ruby)
  (eric/init-scala))

;; Spacemacs Config (after layers load)
;; --------------------

(defun dotspacemacs/user-config ()
  "This function is called at the very end of Spacemacs initialization layers configuration."
  (eric/config-variables)
  (eric/config-mappings)

  (if _is_mac (eric/config-mac))
  (if _is_linux (eric/config-linux))

  (eric/config-evil)
  (eric/config-buffers)
  (eric/config-c-c++)
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
  (eric/config-react)
  (eric/config-repls)
  (eric/config-ruby)
  (eric/config-scala)
  (eric/config-scrolling)
  (eric/config-searching)
  (eric/config-snippets)
  (eric/config-ssh)
  (eric/config-theme))

;; User Init
;; --------------------

(defun eric/init-mac()
  ; add additional packages
  (add-to-list 'dotspacemacs-configuration-layers 'dash)
  (add-to-list 'dotspacemacs-configuration-layers 'osx)

  ; font size not respected on osx
  (set-face-attribute 'default nil :family "Monaco")
  (set-face-attribute 'default nil :height 165)

   dotspacemacs-default-font '("Monaco"
                               :size 16
                               :weight normal
                               :width normal
                               :powerline-scale 1.1)

  ; disable fullscreen animation
  (setq ns-use-native-fullscreen nil)
  (setq ns-use-fullscreen-animation nil))

(defun eric/init-go ()
  ; add GOPATH env var.
  (setenv "GOPATH" "/Users/eric/code/go"))

(defun eric/init-haskell ()
  ; add cabal to executable path
  (add-to-list 'exec-path "~/.cabal/bin"))

(defun eric/init-node ()
  ; add tern to the path
  (add-to-list 'exec-path "~/code/tern/bin"))

(defun eric/init-ruby ()
  (setq-default ruby-version-manager 'rbenv)
  (setq-default ruby-enable-ruby-on-rails-support t))

(defun eric/init-scala ()
  (setq-default flycheck-scala-executable "/usr/local/bin/scalastyle")
  (setq-default flycheck-scalastyle-jar "/usr/local/Cellar/scalastyle/0.7.0/libexec/scalastyle_2.11-0.7.0-batch.jar")

  ; ensime
  (setq-default ensime-sem-high-enabled-p nil) ; Don't add highlighting on save

  ; make ensime use stable version
  (push '("melpa-stable" . "stable.melpa.org/packages/") configuration-layer--elpa-archives)
  (push '(ensime . "melpa-stable") package-pinned-packages))

(defun eric/init-linux ()
   dotspacemacs-default-font '("DejaVu Sans Mono"
                               :size 13
                               :weight normal
                               :width normal
                               :powerline-scale 1.1))

;; User Config
;; --------------------

(defun eric/config-variables()
    ; always follow symlinks in git repos, don't ask
    (setq vc-follow-symlinks t))

(defun eric/config-mappings()
    ; fullscreen
    (define-key evil-normal-state-map (kbd "<s-return>") 'spacemacs/toggle-frame-fullscreen)

    ; show functions and variables
    (define-key evil-normal-state-map (kbd ", RET") 'helm-semantic-or-imenu)

    ; remove highlights
    (define-key evil-normal-state-map (kbd ", SPC") 'evil-search-highlight-persist-remove-all)

    ; show shell
    (define-key evil-normal-state-map (kbd ",s") 'eric/show-or-create-shell))

(defun eric/config-mac()
  ; Don't use slow OSX fullscreen
  (setq ns-use-native-fullscreen nil)
  (setq ns-use-fullscreen-animation nil))

(defun eric/config-linux())

(defun eric/config-evil()
  (setq evil-search-wrap nil) ;; don't wrap search
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

(defun eric/config-c-c++ ()
  (setq c-c++-enable-clang-support t)
)

(defun eric/config-completion ()
    (setq company-idle-delay nil)                     ;; only auto-complete on key binding
    (setq company-tooltip-limit 20)                   ;; use a bigger popup window
    (setq company-selection-wrap-around t)            ;; wrap selection
    (setq ac-auto-start nil)                          ;; only auto-complete on key binding
    (setq auto-completion-enable-snippets-in-popup t) ;; snippets in popups
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

  ; set error list format so that ID is not trunctated
  (setq flycheck-error-list-format
        [("Line" 4 flycheck-error-list-entry-< :right-align t)
         ("Col" 3 nil :right-align t)
         ("Level" 8 flycheck-error-list-entry-level-<)
         ("ID" 20 t)
         ("Message (Checker)" 0 t)])
)

(defun eric/config-git ()
  (setq git-enable-github-support t)
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

(defun eric/config-react()
  (setq flycheck-eslintrc "/Users/eric/code/web/.eslintrc.js")
  (setq-default flycheck-eslintrc "/Users/eric/code/web/.eslintrc.js")
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

(defun eric/config-scala ()
  (evil-define-key 'insert ensime-search-mode-map
    (kbd "C-q") 'ensime-search-quit
    (kbd "C-j") 'ensime-search-next-match
    (kbd "C-k") 'ensime-search-prev-match
    (kbd "RET") 'ensime-search-choose-current-result
    (kbd "C-i") 'ensime-search-insert-import-of-current-result)
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
        (spacemacs/load-theme 'zenburn)
        (spacemacs/load-theme 'zenburn))
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


;; Do not write anything past this comment. This is where Emacs will
;; auto-generate custom variable definitions.

