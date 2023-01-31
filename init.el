;;; init.el -*- lexical-binding: t; -*-
(setenv "LSP_USE_PLISTS" "1")
(setq create-lockfiles nil)
(setq recentf-max-menu-items 25)
(recentf-mode 1)
(setq gc-cons-threshold 100000000)
(setq read-process-output-max (* 1024 1024))
(set-default-coding-systems 'utf-8)
(setq inhibit-startup-message t)
(setq make-backup-files nil)
(setq visible-bell t)
(setq-default tab-width 4)
(setq-default evil-shift-width tab-width)
(setq-default indent-tabs-mode nil)
(global-display-line-numbers-mode t)
(setq display-line-numbers-type 'relative
      scroll-margin 30)
(setq electric-pair-preserve-balance nil)
(setq dired-recursive-copies 'always)
(setq dired-recursive-deletes 'always)
(setq delete-by-moving-to-trash t)
(setq dired-listing-switches
      "-AGFhlv --group-directories-first --time-style=long-iso")
(setq dired-dwim-target t)
(setq dired-auto-revert-buffer #'dired-directory-changed-p) ; also see `dired-do-revert-buffer'
(setq dired-make-directory-clickable t) ; Emacs 29.1
(setq dired-free-space nil) ; Emacs 29.1

(add-hook 'dired-mode-hook #'dired-hide-details-mode)
(add-hook 'dired-mode-hook #'hl-line-mode)
(setq dired-isearch-filenames 'dwim)
;; The following variables were introduced in Emacs 27.1
(setq dired-create-destination-dirs 'ask)
(setq dired-vc-rename-file t)
;; And this is for Emacs 28
(setq dired-do-revert-buffer (lambda (dir) (not (file-remote-p dir))))

(setq dired-clean-up-buffers-too t)
(setq dired-clean-confirm-killing-deleted-buffers t)
(setq dired-x-hands-off-my-keys t)    ; easier to show the keys I use
(setq dired-bind-man nil)
(setq dired-bind-info nil)
(setq delete-by-moving-to-trash t
      +vertico-consult-fd-args "fd -p --color=never -i --type f -E node_modules --regex")
(scroll-bar-mode -1)
(tool-bar-mode -1)
(menu-bar-mode -1)
(tooltip-mode -1)
(electric-pair-mode 1)
(set-fringe-mode 10)
(set-face-attribute 'default nil :font "JetBrains Mono" :height 100)
(setq auto-save-default t
            truncate-string-ellipsis "<>"
            which-key-idle-delay 0.5
            evil-snipe-scope 'whole-visible)
(put 'narrow-to-region 'disabled nil)
(setq x-stretch-cursor t
            window-combination-resize t
            global-auto-revert-mode 1
            global-auto-revert-non-file-buffers t)

(require 'package)

(setq package-archives '(("melpa" . "https://melpa.org/packages/")
                         ("org" . "https://orgmode.org/elpa/")
                         ("elpa" . "https://elpa.gnu.org/packages/")))

(package-initialize)
(unless package-archive-contents
 (package-refresh-contents))

;; Initialize use-package on non-Linux platforms
(unless (package-installed-p 'use-package)
   (package-install 'use-package))

(require 'use-package)
(setq use-package-always-ensure t)

(eval-when-compile (setq evil-want-keybinding nil))

(use-package evil
  :init
  (progn
      (setq evil-want-integration t) ;; This is optional since it's already set to t by default.
      (setq evil-want-keybinding nil)
      (setq evil-undo-system 'undo-fu))
  :config
  (progn
  (evil-mode 1))
      (setq evil-move-cursor-back nil
          evil-want-fine-undo t
          evil-move-beyond-eol t
          evil-vsplit-window-right t
          evil-split-window-below t))

(use-package general
  :config
  (general-evil-setup t))

(use-package evil-collection
    :after evil
    :config
    (evil-collection-init))

(use-package emms
  :init
    (require 'emms-setup)
    (emms-all)
    (setq emms-source-file-default-directory "~/Music/"
	  emms-info-functions '(emms-info-native)
	  emms-player-list '(emms-player-vlc)
	  emms-repeat-track t
	  emms-mode-line-mode t
	  emms-playlist-buffer-name "*Music*"
	  emms-playing-time-mode t
	  emms-info-asynchronously t
	  emms-source-file-directory-tree-function 'emms-source-file-directory-tree-find)
    (emms-add-directory-tree "~/Music/")
    (emms-add-directory-tree "~/Videos/Test Video"))

(use-package helpful
  :ensure t
    :config
        (global-set-key (kbd "C-h f") #'helpful-callable)
        (global-set-key (kbd "C-h v") #'helpful-variable)
        (global-set-key (kbd "C-h k") #'helpful-key)
        (global-set-key (kbd "C-c C-d") #'helpful-at-point)
        (global-set-key (kbd "C-h F") #'helpful-function)
        (global-set-key (kbd "C-h C") #'helpful-command))

(use-package undo-fu)

(use-package undohist
  :config
    (undohist-initialize))

(use-package savehist
  :init
  (savehist-mode))

(use-package which-key 
  :init
  (which-key-mode))

(use-package doom-themes
    :ensure t
    :config
    ;; Global settings (defaults)
    (setq doom-themes-enable-bold t    ; if nil, bold is universally disabled
            doom-themes-enable-italic t) ; if nil, italics is universally disabled
    (load-theme 'doom-dracula t)

    ;; Enable flashing mode-line on errors
    (doom-themes-visual-bell-config)
    ;; Enable custom neotree theme (all-the-icons must be installed!)
    (doom-themes-neotree-config)
    ;; or for treemacs users
    (setq doom-themes-treemacs-theme "doom-atom") ; use "doom-colors" for less minimal icon theme
    (doom-themes-treemacs-config)
    ;; Corrects (and improves) org-mode's native fontification.
    (doom-themes-org-config)
    (custom-set-faces
        '(doom-themes-visual-bell (( t(:background "#00FFFF"))))
        '(emms-playlist-selected-face (( t(:foreground "royal blue"))))
        '(emms-playlist-track-face (( t(:foreground "#5da3e7"))))
        '(emms-playlist-selected-face (( t(:foreground "royal blue"))))
        '(emms-playlist-track-face (( t(:foreground "#5da3e7"))))
        '(org-ellipsis (( t(:foreground "#C678DD"))))))

;; (use-package modus-themes
;;    :config
;;    (setq modus-themes-italic-constructs t
;;          modus-themes-bold-constructs nil)
;;    (load-theme 'modus-vivendi t))

(use-package doom-modeline
  :init (doom-modeline-mode 1))

(use-package all-the-icons)

(use-package all-the-icons-completion
      :config
      (all-the-icons-completion-mode)
      (add-hook 'marginalia-mode-hook #'all-the-icons-completion-marginalia-setup))

(use-package unicode-fonts)

(use-package emojify)

(use-package corfu
  :init
  (global-corfu-mode)
  ;; Setup corfu for popup like completion
  (setq corfu-cycle t  ; Allows cycling through candidates
        corfu-auto t   ; Enable auto completion
        corfu-auto-prefix 0  ; Complete with less prefix keys
        corfu-auto-delay 0.0  ; No delay for completion
        corfu-echo-documentation 0.25  ; Echo docs for current completion option
        corfu-quit-at-boundary 'insert
        )
  (global-corfu-mode 1)
  (advice-add #'lsp-completion-at-point :around #'cape-wrap-noninterruptible)
  (corfu-popupinfo-mode 1))

(use-package emacs
  :init
  (defun crm-indicator (args)
    (cons (format "[CRM%s] %s"
                  (replace-regexp-in-string
                   "\\`\\[.*?]\\*\\|\\[.*?]\\*\\'" ""
                   crm-separator)
                  (car args))
          (cdr args)))
  (advice-add #'completing-read-multiple :filter-args #'crm-indicator)

  (setq minibuffer-prompt-properties
        '(read-only t cursor-intangible t face minibuffer-prompt))
  (add-hook 'minibuffer-setup-hook #'cursor-intangible-mode)
  (setq enable-recursive-minibuffers t)
  (setq completion-cycle-threshold 3)
  (setq tab-always-indent 'complete))

(use-package cape
    :init
    (add-to-list 'completion-at-point-functions #'cape-file)
    (add-to-list 'completion-at-point-functions #'cape-dabbrev))

(use-package vertico
  :init
  (setq vertico-count 20
        vertico-resize nil
        vertico-cycle t)
  (vertico-mode)
  (setq completion-styles '(substring orderless basic)))

(use-package marginalia
  :config
  (marginalia-mode)
  (setq marginalia-align 'center
    marginalia-align-offset 20))

(use-package embark
  :ensure t
  :bind
  (("C-;" . embark-act)         ;; pick some comfortable binding
   ("M-." . embark-dwim)        ;; good alternative: M-.
   ("C-h B" . embark-bindings)) ;; alternative for `describe-bindings'

  :init

  ;; Optionally replace the key help with a completing-read interface
  (setq prefix-help-command #'embark-prefix-help-command)

  :config
  ;; (define-key embark-symbol-map "D" #'devdocs-lookup)
  ;; (define-key embark-function-map "D" #'devdocs-lookup)

  ;; Hide the mode line of the Embark live/completions buffers
  (add-to-list 'display-buffer-alist
               '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
                 nil
                 (window-parameters (mode-line-format . none)))))

(use-package embark-consult
  :ensure t ; only need to install it, embark loads it after consult if found
  :hook
  (embark-collect-mode . consult-preview-at-point-mode))

(use-package orderless
:ensure t
  :config
  (defun +vertico-orderless-dispatch (pattern _index _total)
    (cond
     ;; Ensure $ works with Consult commands, which add disambiguation suffixes
     ((string-suffix-p "$" pattern)
      `(orderless-regexp . ,(concat (substring pattern 0 -1) "[\x200000-\x300000]*$")))
     ;; Ignore single !
     ((string= "!" pattern) `(orderless-literal . ""))
     ;; Without literal
     ((string-prefix-p "!" pattern) `(orderless-without-literal . ,(substring pattern 1)))
     ;; Character folding
     ((string-prefix-p "%" pattern) `(char-fold-to-regexp . ,(substring pattern 1)))
     ((string-suffix-p "%" pattern) `(char-fold-to-regexp . ,(substring pattern 0 -1)))
     ;; Initialism matching
     ((string-prefix-p "`" pattern) `(orderless-initialism . ,(substring pattern 1)))
     ((string-suffix-p "`" pattern) `(orderless-initialism . ,(substring pattern 0 -1)))
     ;; Literal matching
     ((string-prefix-p "=" pattern) `(orderless-literal . ,(substring pattern 1)))
     ((string-suffix-p "=" pattern) `(orderless-literal . ,(substring pattern 0 -1)))
     ;; Flex matching
     ((string-prefix-p "~" pattern) `(orderless-flex . ,(substring pattern 1)))
     ((string-suffix-p "~" pattern) `(orderless-flex . ,(substring pattern 0 -1)))))
  (add-to-list
   'completion-styles-alist
   '(+vertico-basic-remote
     +vertico-basic-remote-try-completion
     +vertico-basic-remote-all-completions
     "Use basic completion on remote files only"))
  (setq completion-styles '(orderless basic flex)
        completion-category-defaults nil
        ;; note that despite override in the name orderless can still be used in
        ;; find-file etc.
        completion-category-overrides '((file (styles +vertico-basic-remote orderless partial-completion)))
        orderless-style-dispatchers '(+vertico-orderless-dispatch)
        orderless-component-separator "[ &]")
  ;; ...otherwise find-file gets different highlighting than other commands
  (setq orderless-matching-styles '(orderless-prefixes orderless-regexp orderless-flex))
  (set-face-attribute 'completions-first-difference nil :inherit nil))

(use-package consult
  :ensure t
  :bind (;; C-c bindings (mode-specific-map)
         ("C-c M-x" . consult-mode-command)
         ("C-c h" . consult-history)
         ("C-c k" . consult-kmacro)
         ("C-c m" . consult-man)
         ("C-c i" . consult-info)
         ([remap Info-search] . consult-info)
         ;; C-x bindings (ctl-x-map)
         ("C-x M-:" . consult-complex-command)     ;; orig. repeat-complex-command
         ("C-x b" . consult-buffer)                ;; orig. switch-to-buffer
         ("C-x 4 b" . consult-buffer-other-window) ;; orig. switch-to-buffer-other-window
         ("C-x 5 b" . consult-buffer-other-frame)  ;; orig. switch-to-buffer-other-frame
         ("C-x r b" . consult-bookmark)            ;; orig. bookmark-jump
         ("C-x p b" . consult-project-buffer)      ;; orig. project-switch-to-buffer
         ;; Custom M-# bindings for fast register access
         ("M-#" . consult-register-load)
         ("M-'" . consult-register-store)          ;; orig. abbrev-prefix-mark (unrelated)
         ("C-M-#" . consult-register)
         ;; Other custom bindings
         ("M-y" . consult-yank-pop)                ;; orig. yank-pop
         ;; M-g bindings (goto-map)
         ("M-g e" . consult-compile-error)
         ("M-g f" . consult-flymake)               ;; Alternative: consult-flycheck
         ("M-g g" . consult-goto-line)             ;; orig. goto-line
         ("M-g M-g" . consult-goto-line)           ;; orig. goto-line
         ("M-g o" . consult-outline)               ;; Alternative: consult-org-heading
         ("M-g m" . consult-mark)
         ("M-g k" . consult-global-mark)
         ("M-g i" . consult-imenu)
         ("M-g I" . consult-imenu-multi)
         ;; M-s bindings (search-map)
         ("M-s d" . consult-find)
         ("M-s D" . consult-locate)
         ("M-s g" . consult-grep)
         ("M-s G" . consult-git-grep)
         ("M-s r" . consult-ripgrep)
         ("M-s l" . consult-line)
         ("M-s L" . consult-line-multi)
         ("M-s k" . consult-keep-lines)
         ("M-s u" . consult-focus-lines)
         ;; Isearch integration
         ("M-s e" . consult-isearch-history)
         :map isearch-mode-map
         ("M-e" . consult-isearch-history)         ;; orig. isearch-edit-string
         ("M-s e" . consult-isearch-history)       ;; orig. isearch-edit-string
         ("M-s l" . consult-line)                  ;; needed by consult-line to detect isearch
         ("M-s L" . consult-line-multi)            ;; needed by consult-line to detect isearch
         ;; Minibuffer history
         :map minibuffer-local-map
         ("M-s" . consult-history)                 ;; orig. next-matching-history-element
         ("M-r" . consult-history))                ;; orig. previous-matching-history-element
  :hook (completion-list-mode . consult-preview-at-point-mode)
  :init
  (setq register-preview-delay 0.5
        register-preview-function #'consult-register-format)
  (advice-add #'register-preview :override #'consult-register-window)
  (setq xref-show-xrefs-function #'consult-xref
        xref-show-definitions-function #'consult-xref)
  :config
  (consult-customize
   consult-theme :preview-key '(:debounce 0.2 any)
   consult-ripgrep consult-git-grep consult-grep
   consult-bookmark consult-recent-file consult-xref
   consult--source-bookmark consult--source-file-register
   consult--source-recent-file consult--source-project-recent-file
   ;; :preview-key (kbd "M-.")
   :preview-key '(:debounce 0.4 any))
  (setq consult-narrow-key "<")) ;; (kbd "C-+")

(use-package embark-consult
  :hook
  (embark-collect-mode . consult-preview-at-point-mode))

(use-package evil-nerd-commenter)

(use-package lsp-mode
  :ensure t
  :custom
  (lsp-completion-provider :none) ;; we use Corfu!
  :init
  (setq lsp-log-io nil)
  (defun my/lsp-mode-setup-completion ()
    (setf (alist-get 'styles (alist-get 'lsp-capf completion-category-defaults))
          '(flex))) ;; Configure flex
  :hook
  (lsp-completion-mode . my/lsp-mode-setup-completion))

(use-package rustic
:ensure t
  :config
    (setq 
        lsp-rust-analyzer-display-chaining-hints t
        lsp-rust-analyzer-expand-macro t
        lsp-rust-analyzer-display-parameter-hints t
        lsp-rust-analyzer-server-display-inlay-hints t))

(setq web-mode-markup-indent-offset 2)
(setq web-mode-code-indent-offset 2)
(setq web-mode-css-indent-offset 2)
(use-package web-mode
  :ensure t
  :mode (("\\.js\\'" . web-mode)
	 ("\\.jsx\\'" .  web-mode)
	 ("\\.tsx\\'" .  web-mode)
	 ("\\.ts\\'" . web-mode)
	 ("\\.tsx\\'" . web-mode)
	 ("\\.html\\'" . web-mode))
  :commands web-mode)

(use-package flycheck)

(defun setup-tide-mode ()
      (interactive)
      (tide-setup)
      (flycheck-mode +1)
      (setq flycheck-check-syntax-automatically '(save mode-enabled))
      (eldoc-mode +1)
      (tide-hl-identifier-mode +1))

(add-hook 'tsx-ts-mode-hook #'setup-tide-mode)
;; if you use treesitter based typescript-ts-mode (emacs 29+)
(use-package tide
  :ensure t
  :after (flycheck)
  :hook ((typescript-ts-mode . tide-setup)
         (tsx-ts-mode . tide-setup)
         (typescript-ts-mode . tide-hl-identifier-mode)
         (before-save . tide-format-before-save)))

(use-package lsp-ui
  :hook (lsp-mode . lsp-ui-mode)
  :config
  (setq lsp-ui-peek-enable t
        lsp-ui-doc-position 'bottom
        lsp-ui-peek-always-show t
        lsp-signature-auto-activate t
        lsp-ui-doc-delay 0.0
        lsp-ui-sideline-show-diagnostics t 
        lsp-enable-symbol-highlighting t 
        lsp-ui-doc-enable t 
        lsp-ui-doc-show-with-cursor t 
        lsp-ui-doc-show-with-mouse t 
        lsp-lens-enable t 
        lsp-headerline-breadcrumb-enable t 
        lsp-ui-sideline-show-diagnostics t 
        lsp-modeline-code-actions-enable t 
        lsp-eldoc-enable-hover t 
        lsp-completion-show-detail t 
        lsp-completion-show-kind t 
        lsp-ui-sideline-ignore-duplicate t
        lsp-ui-sideline-actions-icon lsp-ui-sideline-actions-icon-default))

(use-package tree-sitter)
(use-package tree-sitter-langs)

(use-package yasnippet
:ensure t
  :config
  (setq yas-snippet-dirs
      '("~/.config/emacs/snippets"))

(yas-global-mode 1) ;; or M-x yas-reload-all if you've started YASnippet already.

(use-package magit)

(use-package git-gutter-fringe
        :config
        (global-git-gutter-mode +1)
(setq-default fringes-outside-margins t)
    ;; thin fringe bitmaps
    (define-fringe-bitmap 'git-gutter-fr:added [224]
      nil nil '(center repeated))
    (define-fringe-bitmap 'git-gutter-fr:modified [224]
      nil nil '(center repeated))
    (define-fringe-bitmap 'git-gutter-fr:deleted [128 192 224 240]
      nil nil 'bottom)
  )

(use-package smart-compile
  :config
  (setq smart-compile-check-build-system 'nil)
  (add-to-list 'smart-compile-alist '("\\.[Cc]+[Pp]*\\'" . "make %n && touch inputf.in && timeout 4s ./%n < inputf.in &> outputf.in "))
  (add-to-list 'smart-compile-alist  '("\\.rs$" . "touch inputf.in && cargo run -q < inputf.in &> outputf.in "))))

(use-package evil-multiedit
    :config
    (evil-multiedit-default-keybinds))

(use-package projectile
  :init
  (projectile-mode +1)
  :bind (:map projectile-mode-map
              ("s-p" . projectile-command-map)
              ("C-c p" . projectile-command-map)))

(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

(add-hook 'org-mode-hook (lambda () (org-indent-mode 1)))
(add-hook 'org-mode-hook (lambda () (org-toggle-pretty-entities 1)))

(use-package evil-org)

(use-package org-cliplink)

(use-package link-hint)

(use-package toc-org)  
(add-hook 'org-mode-hook (lambda () (toc-org-mode 1)))

(use-package org-superstar)
(add-hook 'org-mode-hook (lambda () (org-superstar-mode 1)))

(use-package org-appear)
(add-hook 'org-mode-hook 'org-appear-mode)

(setq org-todo-keywords
    '((sequence "TODO(t)" "PROJ(p)" "ACTIVE(a)" "REVIEW(r)" "START(s)" "NEXT(n)" "WORKING(w)" "HOLD(h)" "|" "DONE(d)" "KILL(k)")
        (sequence "|" "OKAY(o)" "YES(y)" "NO(n)")))

(require 'org-tempo)
(add-to-list 'org-structure-template-alist '("la" . "src latex"))
(add-to-list 'org-structure-template-alist '("ec" . "src emacs-lisp"))

(use-package denote
    :config
    (setq denote-directory "~/Documents/Denote")
    (setq  denote-known-keywords '()))

(defun kitty-async-process ()
    (interactive)
    (start-process "kitty" nil "setsid" "kitty" "-d" default-directory))
(define-key evil-normal-state-map "," 'kitty-async-process)

(defun brave-vscode-docs ()
  (interactive)
  (start-process "brave" nil "setsid" "brave" "--incognito" "https://code.visualstudio.com/api/language-extensions/language-server-extension-guide"))

(defun Competitive-coding-output-input-toggle ()
    (interactive)
    (delete-other-windows)
    (kill-matching-buffers "*.in")
    (evil-window-vsplit)
    (other-window 1)
    (find-file (expand-file-name "inputf.in" default-directory))
    (evil-window-split)
    (other-window 1)
    (find-file (expand-file-name "outputf.in" default-directory))
    (other-window 1)
    (enlarge-window-horizontally 40))
(evil-define-key 'normal c++-mode-map "C-c z" 'Competitive-coding-output-input-toggle)

(defun rust-reset()
  (interactive)
  (widen)
  (erase-buffer)
  (insert "<cp")
  (yas-expand)
  (narrow-to-defun))

(defun code-input-refresh()
    (interactive)
    (write-region (current-kill 0) nil (concat default-directory "inputf.in") nil)
    (Competitive-coding-output-input-toggle))
(evil-define-key 'normal c++-mode-map "C-c z" 'code-input-refresh)

;; source: http://steve.yegge.googlepages.com/my-dot-emacs-file
(defun copy-current-file (new-name)
  "Copy current file to a NEW-NAME."
  (interactive (list
                (read-string "New name: " (current-kill 0) nil (current-kill 0))))
  (let ((name (buffer-name))
        (filename (buffer-file-name)))
    (if (not filename)
        (message "Buffer '%s' is not visiting a file!" name)
      (if (get-buffer new-name)
          (message "A buffer named '%s' already exists!" new-name)
          (copy-file filename (concat (replace-regexp-in-string " " "" (capitalize (replace-regexp-in-string "[^[:word:]_]" " " new-name))) ".rs") 1)))))

(global-set-key (kbd "<escape>") 'keyboard-escape-quit)
(global-set-key (kbd "C-;") 'embark-act)
(general-create-definer adi/leader-keys
 :states '(normal visual emacs jpnb)
 :keymaps 'override
 :prefix "SPC")
(general-create-definer adi/leader-local-keys
 :states '(normal visual emacs jpnb)
 :keymaps 'override
 :prefix "SPC m")
(adi/leader-keys
    "SPC" 'find-file
    "RET" 'denote-open-or-create)
(general-define-key
    :states 'motion
    "K" 'helpful-at-point
    "M-/" 'evilnc-comment-or-uncomment-lines)

(adi/leader-keys
    "gg" 'magit)

(adi/leader-keys
    "bb" 'consult-buffer
    "bk" 'kill-this-buffer)

(adi/leader-keys
    "fr" 'consult-recent-file)

(general-define-key
    :keymap 'org-mode-map
    :states 'normal
      "?\t" 'org-cycle
      "<RET>" 'org-open-at-point)
(adi/leader-local-keys org-mode-map
    "lc" 'org-cliplink)

(general-define-key
    :keymap 'lsp-mode-map
    :states 'normal
      "K" 'lsp-describe-thing-at-point
      "C-c a" 'lsp-format-buffer)

(adi/leader-local-keys
    :keymap 'rustic-mode-map
    "z" 'Competitive-coding-output-input-toggle
    "r" 'rust-reset
    "i" 'code-input-refresh
    "c" 'copy-current-file)
