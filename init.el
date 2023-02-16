;;; init.el -*- lexical-binding: t; -*-
(setenv "LSP_USE_PLISTS" "1")
(setq create-lockfiles nil)
(setq recentf-max-menu-items 25)
(setq revert-without-query '(".*"))
(recentf-mode 1)
(save-place-mode 1)
(setq read-process-output-max (* 1024 1024))
(set-default-coding-systems 'utf-8)
(setq inhibit-startup-message t)
(setq make-backup-files nil)
(setq visible-bell t)
(setq-default tab-width 4)
(setq-default evil-shift-width tab-width)
(setq-default indent-tabs-mode nil)
(global-display-line-numbers-mode t)
(setq custom-file (locate-user-emacs-file "custom.el"))
(load custom-file 'noerror 'nomessage)
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
(setq resize-mini-windows t)

(add-hook 'dired-mode-hook #'hl-line-mode)
(setq dired-isearch-filenames 'dwim)
(setq dired-create-destination-dirs 'ask)
(setq dired-vc-rename-file t)
(setq dired-do-revert-buffer (lambda (dir) (not (file-remote-p dir))))

(setq dired-clean-up-buffers-too t)
(setq dired-clean-confirm-killing-deleted-buffers t)
(setq dired-x-hands-off-my-keys t)    ; easier to show the keys I use
(setq dired-bind-man nil)
(setq dired-bind-info nil)
(setq delete-by-moving-to-trash t
      +vertico-consult-fd-args "fd -p --color=never -i --type f -E node_modules --regex")
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

(defun random-element-of-list (items)
  (let* ((size (length items))
         (index (random size)))
    (nth index items)))

(defun kitty-async-process ()
  (interactive)
  (start-process "kitty" nil "setsid" "kitty" "-d" default-directory))

(defun brave-vscode-docs ()
  (interactive)
  (start-process "brave" nil "setsid" "brave" "--incognito" "https://code.visualstudio.com/api/language-extensions/language-server-extension-guide"))

(defun Competitive-coding-output-input-toggle ()
  (interactive)
  (delete-other-windows)
  (kill-matching-buffers "*.in")
  (evil-window-vsplit)
  (find-file (expand-file-name "inputf.in" default-directory))
  (evil-window-split)
  (find-file (expand-file-name "outputf.in" default-directory))
  (other-window 1)
  (enlarge-window-horizontally 40))

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

(defvar bootstrap-version)
(let ((bootstrap-file
         (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
        (bootstrap-version 6))
    (unless (file-exists-p bootstrap-file)
      (with-current-buffer
          (url-retrieve-synchronously
           "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
           'silent 'inhibit-cookies)
        (goto-char (point-max))
        (eval-print-last-sexp)))
    (load bootstrap-file nil 'nomessage))
(setq-default straight-vc-git-default-clone-depth 1)
(setq straight-use-package-by-default t) 
(straight-use-package 'use-package)

(eval-when-compile (setq evil-want-keybinding nil))

(use-package evil
      :init
        (setq evil-want-integration t) ;; This is optional since it's already set to t by default.
        (setq evil-want-keybinding nil)
        (setq evil-undo-system 'undo-fu)
      :config
      (evil-mode 1))
  (setq evil-move-cursor-back nil
      evil-want-fine-undo t
      evil-move-beyond-eol t
      evil-respect-visual-line-mode t
      evil-org-retain-visual-state-on-shift t
      evil-vsplit-window-right t
      evil-split-window-below t)

(use-package general
  :config
  (general-evil-setup t))

(use-package evil-collection
    :after evil
    :config
    (evil-collection-init))

(use-package notmuch)

(use-package docker
   :config
   (setq tramp-docker-program "podman"
         docker-command "podman"
         docker-composee-command "podman-compose"
         tramp-docker-method "podman"))

(use-package ace-window
    :config
    (setq aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l)))

(use-package pulsar
   :config
   (setq pulsar-pulse t)
   (setq pulsar-delay 0.055)
   (setq pulsar-iterations 10)
   (setq pulsar-face 'pulsar-magenta)
   (setq pulsar-highlight-face 'pulsar-yellow)
   (add-hook 'next-error-hook #'pulsar-pulse-line)
   (add-hook 'consult-after-jump-hook #'pulsar-recenter-top)
   (add-hook 'consult-after-jump-hook #'pulsar-reveal-entry)
   (pulsar-global-mode 1))

(use-package vimish-fold)

(use-package ligature
  :config
  (ligature-set-ligatures 'prog-mode '("-|" "-~" "---" "-<<" "-<" "--" "->" "->>" "-->" "///" "/=" "/=="
                                      "/>" "//" "/*" "*>" "***" "*/" "<-" "<<-" "<=>" "<=" "<|" "<||"
                                      "<|||" "<|>" "<:" "<>" "<-<" "<<<" "<==" "<<=" "<=<" "<==>" "<-|"
                                      "<<" "<~>" "<=|" "<~~" "<~" "<$>" "<$" "<+>" "<+" "</>" "</" "<*"
                                      "<*>" "<->" "<!--" ":>" ":<" ":::" "::" ":?" ":?>" ":=" "::=" "=>>"
                                      "==>" "=/=" "=!=" "=>" "===" "=:=" "==" "!==" "!!" "!=" ">]" ">:"
                                      ">>-" ">>=" ">=>" ">>>" ">-" ">=" "&&&" "&&" "|||>" "||>" "|>" "|]"
                                      "|}" "|=>" "|->" "|=" "||-" "|-" "||=" "||" ".." ".?" ".=" ".-" "..<"
                                      "..." "+++" "+>" "++" "[||]" "[<" "[|" "{|" "??" "?." "?=" "?:" "##"
                                      "###" "####" "#[" "#{" "#=" "#!" "#:" "#_(" "#_" "#?" "#(" ";;" "_|_"
                                      "__" "~~" "~~>" "~>" "~-" "~@" "$>" "^=" "]#"))
  (global-prettify-symbols-mode)
  (global-ligature-mode t))

(use-package emms
  :init
    (require 'emms-setup)
    (emms-all)
    (setq emms-source-file-default-directory "~/Music/"
          emms-info-functions '(emms-info-native)
          emms-player-list '(emms-player-mpv)
          emms-repeat-track t
          emms-mode-line-mode t
          emms-playlist-buffer-name "*Music*"
          emms-playing-time-mode t
          emms-info-asynchronously t
          emms-source-file-directory-tree-function 'emms-source-file-directory-tree-find)
    (emms-add-directory-tree "~/Music/")
    (emms-add-directory-tree "~/Videos/Test Video"))

(use-package helpful
    :config
        (global-set-key (kbd "C-h f") #'helpful-callable)
        (global-set-key (kbd "C-h v") #'helpful-variable)
        (global-set-key (kbd "C-h k") #'helpful-key)
        (global-set-key (kbd "C-h F") #'helpful-function)
        (global-set-key (kbd "C-h C") #'helpful-command))

(use-package avy
     :config
     (setq avy-background t)
     (avy-setup-default))

(use-package undo-fu)

(use-package undohist
    :config
    (undohist-initialize))

(use-package savehist
  :init
  (savehist-mode))

(setq banner-icons-list (file-expand-wildcards (concat user-emacs-directory "icons/*")))
(use-package dashboard
        :after all-the-icons
        :config
        (setq dashboard-items '((recents  . 5)
                                (agenda . 5)
                                (projects . 5)))
        (setq dashboard-set-heading-icons t)
        (setq dashboard-startup-banner (random-element-of-list banner-icons-list))
        (setq dashboard-banner-logo-title "")
        (setq dashboard-image-banner-max-height 500)
        (setq dashboard-set-footer nil)
        (setq dashboard-set-file-icons t)
        (setq dashboard-set-init-info t)
        (setq initial-buffer-choice (lambda () (get-buffer-create "*dashboard*")))
        (dashboard-setup-startup-hook))
(add-hook 'server-after-make-frame-hook 'dashboard-refresh-buffer)

(use-package which-key 
  :init
  (which-key-mode))

(use-package doom-themes
    :config
    (setq doom-themes-enable-bold t
            doom-themes-enable-italic t)
    (load-theme 'doom-dracula t)
    (doom-themes-visual-bell-config)
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
    :init (doom-modeline-mode 1)
    :config
     (display-battery-mode 1)
     (setq doom-modeline-project-detection 'truncate-upto-project)
     (setq doom-modeline-enable-word-count t)
     (setq doom-modeline-buffer-encoding nil)
     (setq doom-modeline-env-version t)
     (setq doom-modeline-hud t))

(use-package all-the-icons)

(use-package all-the-icons-completion
      :config
      (all-the-icons-completion-mode)
      (add-hook 'marginalia-mode-hook #'all-the-icons-completion-marginalia-setup))

(use-package kind-icon
  :after corfu
  :custom
  (kind-icon-default-face 'corfu-default) ; to compute blended backgrounds correctly
  :config
  (add-to-list 'corfu-margin-formatters #'kind-icon-margin-formatter))

(use-package all-the-icons-dired
  :config
  (add-hook 'dired-mode-hook 'all-the-icons-dired-mode))

(use-package unicode-fonts)

(use-package emojify)

(use-package evil-nerd-commenter)

(use-package lsp-mode
  :custom
  (lsp-completion-provider :none)
  :init
  (setq lsp-log-io nil)
  (defun my/lsp-mode-setup-completion ()
    (setf (alist-get 'styles (alist-get 'lsp-capf completion-category-defaults))
          '(flex))) ;; Configure flex
  :hook
  (lsp-completion-mode . my/lsp-mode-setup-completion)
  (prog-mode . lsp-mode)
  (web-mode . lsp-mode))

(use-package rustic
  :config
    (setq 
        lsp-rust-analyzer-display-chaining-hints t
        lsp-rust-analyzer-expand-macro t
        lsp-rust-analyzer-display-parameter-hints t
        lsp-rust-analyzer-server-display-inlay-hints t))

(use-package typescript-mode)

(setq web-mode-markup-indent-offset 2)
(setq web-mode-code-indent-offset 2)
(setq web-mode-css-indent-offset 2)
(use-package web-mode
    :commands web-mode)

(add-to-list 'auto-mode-alist '("\\.svelte\\'" . web-mode))
(setq web-mode-engines-alist
    '(("svelte" . "\\.svelte\\'")))

(use-package ccls)

(use-package solidity-mode)

(use-package lsp-pyright
  :hook (python-mode . (lambda ()
                          (require 'lsp-pyright)
                          (lsp))))

(use-package flycheck
  :init (global-flycheck-mode))

(use-package format-all
   :config
   (add-hook 'prog-mode-hook 'format-all-mode)
   (add-hook 'format-all-mode-hook 'format-all-ensure-formatter))

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
        lsp-ui-sideline-actions-icon lsp-ui-sideline-actions-icon-default))

(use-package tree-sitter-langs
      :after tree-sitter
      :config
      (tree-sitter-require 'tsx)
      (tree-sitter-require 'typescript)
      (tree-sitter-require 'rust)
      (tree-sitter-require 'javascript)
      (tree-sitter-require 'python)
      (tree-sitter-require 'html)
      (tree-sitter-require 'cpp)
      (tree-sitter-require 'css)
      (add-to-list 'tree-sitter-major-mode-language-alist '(typescript-ts-mode . tsx)))
(global-tree-sitter-mode)
(add-hook 'tree-sitter-after-on-hook #'tree-sitter-hl-mode)

(use-package yasnippet
  :config
  (setq yas-snippet-dirs
      '("~/.config/emacs/snippets"))
(yas-global-mode 1))

(use-package doom-snippets
  :after yasnippet
  :straight (doom-snippets :type git :host github :repo "hlissner/doom-snippets" :files ("*.el" "*")))

(use-package magit
  :config
    (setq magit-display-buffer-function #'magit-display-buffer-fullframe-status-v1))

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
        nil nil 'bottom))

(use-package smart-compile
  :config
  (setq smart-compile-check-build-system 'nil)
  (add-to-list 'smart-compile-alist '("\\.[Cc]+[Pp]*\\'" . "make %n && touch inputf.in && timeout 4s ./%n < inputf.in &> outputf.in "))
  (add-to-list 'smart-compile-alist  '("\\.rs$" . "touch inputf.in && cargo run -q < inputf.in &> outputf.in ")))

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

(use-package company)

(use-package corfu
  :init
  ;; Setup corfu for popup like completion
  (setq corfu-cycle t  ; Allows cycling through candidates
        corfu-auto t   ; Enable auto completion
        corfu-auto-prefix 1  ; Complete with less prefix keys
        corfu-auto-delay 0.0  ; No delay for completion
        corfu-echo-documentation 0.0  ; Echo docs for current completion option
        corfu-quit-at-boundary 'insert)
  (global-corfu-mode 1)
  (advice-add #'lsp-completion-at-point :around #'cape-wrap-noninterruptible))

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
    (add-to-list 'completion-at-point-functions (cape-company-to-capf #'company-yasnippet))
    (add-to-list 'completion-at-point-functions #'cape-file)
    (add-to-list 'completion-at-point-functions #'cape-dabbrev))

(use-package vertico
    :init
    (setq vertico-count 20
            vertico-resize nil
            vertico-cycle t)
    (vertico-mode))

(use-package marginalia
  :config
  (marginalia-mode)
  (setq marginalia-align 'center
    marginalia-align-offset 20))

(defun embark-act-noquit ()
      "Run action but don't quit the minibuffer afterwards."
      (interactive)
      (let ((embark-quit-after-action nil))
        (embark-act)))

(use-package embark
        :bind
        (("C-;" . embark-act-noquit)         ;; pick some comfortable binding
         ("M-." . embark-dwim)        ;; good alternative: M-.
         ("C-h B" . embark-bindings)) ;; alternative for `describe-bindings'

        :init
        (setq prefix-help-command #'embark-prefix-help-command)
        :config
        (add-to-list 'display-buffer-alist
                     '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
                       nil
                       (window-parameters (mode-line-format . none)))))
    (defun embark-which-key-indicator ()
      "An embark indicator that displays keymaps using which-key.
    The which-key help message will show the type and value of the
    current target followed by an ellipsis if there are further
    targets."
      (lambda (&optional keymap targets prefix)
        (if (null keymap)
            (which-key--hide-popup-ignore-command)
          (which-key--show-keymap
           (if (eq (plist-get (car targets) :type) 'embark-become)
               "Become"
             (format "Act on %s '%s'%s"
                     (plist-get (car targets) :type)
                     (embark--truncate-target (plist-get (car targets) :target))
                     (if (cdr targets) "…" "")))
           (if prefix
               (pcase (lookup-key keymap prefix 'accept-default)
                 ((and (pred keymapp) km) km)
                 (_ (key-binding prefix 'accept-default)))
             keymap)
           nil nil t (lambda (binding)
                       (not (string-suffix-p "-argument" (cdr binding))))))))

    (setq embark-indicators
      '(embark-which-key-indicator
        embark-highlight-indicator
        embark-isearch-highlight-indicator))

    (defun embark-hide-which-key-indicator (fn &rest args)
      "Hide the which-key indicator immediately when using the completing-read prompter."
      (which-key--hide-popup-ignore-command)
      (let ((embark-indicators
             (remq #'embark-which-key-indicator embark-indicators)))
          (apply fn args)))

    (advice-add #'embark-completing-read-prompter
                    :around #'embark-hide-which-key-indicator)

(use-package embark-consult
  :hook
  (embark-collect-mode . consult-preview-at-point-mode))

(use-package orderless
    :custom
    (orderless-matching-styles '(orderless-literal orderless-regexp orderless-flex))
    (completion-styles '(orderless))
    (completion-category-overrides '((file (styles partial-completion)))))

(use-package consult
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

(defun adi/org-setup()
    (org-indent-mode 1)
    (setq org-pretty-entities 1)
    (setq org-confirm-babel-evaluate nil))

(add-hook 'org-mode-hook 'adi/org-setup)
(defadvice org-babel-execute-src-block (around load-language nil activate)
    "Load language if needed"
    (let ((language (org-element-property :language (org-element-at-point))))
        (unless (cdr (assoc (intern language) org-babel-load-languages))
        (add-to-list 'org-babel-load-languages (cons (intern language) t))
        (org-babel-do-load-languages 'org-babel-load-languages org-babel-load-languages))
        ad-do-it))

(use-package evil-org)

(use-package org-cliplink)

(use-package link-hint)

(use-package toc-org)  
(add-hook 'org-mode-hook (lambda () (toc-org-mode 1)))

(use-package org-modern
   :config
    (setq
        org-auto-align-tags nil
        org-tags-column 0
        org-catch-invisible-edits 'show-and-error
        org-special-ctrl-a/e t
        org-insert-heading-respect-content t
        org-hide-emphasis-markers t
        org-pretty-entities t
        org-ellipsis "…"
        org-agenda-tags-column 0
        org-agenda-block-separator ?─
        org-agenda-time-grid
        '((daily today require-timed)
            (800 1000 1200 1400 1600 1800 2000)
            " ┄┄┄┄┄ " "┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄")
            org-agenda-current-time-string
            "⭠ now ─────────────────────────────────────────────────")
        (global-org-modern-mode))

(setq org-todo-keywords
    '((sequence "TODO(t)" "PROJ(p)" "ACTIVE(a)" "REVIEW(r)" "START(s)" "NEXT(n)" "WORKING(w)" "HOLD(h)" "|" "DONE(d)" "KILL(k)")
        (sequence "|" "OKAY(o)" "YES(y)" "NO(n)")))

(require 'org-tempo)
(add-to-list 'org-structure-template-alist '("la" . "src latex"))
(add-to-list 'org-structure-template-alist '("ec" . "src emacs-lisp"))

(setq org-agenda-files (directory-files-recursively "~/Documents/Denote/Todo/" "\\.org$"))
(setq org-agenda-window-setup 'current-window
    org-agenda-start-day "-3d"
    org-agenda-inhibit-startup t)

(use-package denote
    :straight (denote :type git :host github :repo "protesilaos/denote")
    :config
    (setq denote-directory "~/Documents/Denote")
    (setq denote-known-keywords '())
    (setq denote-infer-keywords t)
    (setq denote-sort-keywords t)
    (setq denote-excluded-directories-regexp nil)
    (setq denote-excluded-keywords-regexp nil)
    (setq denote-date-prompt-use-org-read-date t)
    (setq denote-backlinks-show-context t))

(with-eval-after-load 'org-capture
    (add-to-list 'org-capture-templates
               '("n" "Notes" plain
                (file denote-last-path)
                (function
                    (lambda ()
                        (let ((denote-directory (file-name-as-directory (concat (denote-directory) "Notes"))))
                            (denote-org-capture))))
                :no-save t
                :immediate-finish nil
                :kill-buffer t
                :jump-to-captured t))
    (add-to-list 'org-capture-templates
               '("r" "Resources" plain
                (file denote-last-path)
                (function
                    (lambda ()
                        (let ((denote-directory (file-name-as-directory (concat (denote-directory) "Resources"))))
                            (denote-org-capture))))
                :no-save t
                :immediate-finish nil
                :kill-buffer t
                :jump-to-captured t))
    (add-to-list 'org-capture-templates
               '("t" "Todo" plain
                (file denote-last-path)
                (function
                    (lambda ()
                        (let ((denote-directory (file-name-as-directory (concat (denote-directory) "Todo"))))
                            (denote-org-capture))))
                :no-save t
                :immediate-finish nil
                :kill-buffer t
                :jump-to-captured t)))

(global-set-key (kbd "<escape>") 'keyboard-escape-quit)
(global-set-key (kbd "C-;") 'embark-act)
(general-define-key
    :keymaps 'evil-window-map
    "C-w" 'ace-window)
(define-key minibuffer-mode-map (kbd "C-S-v") 'evil-paste-after)
(general-create-definer aadi/leader-keys
    :states '(normal visual emacs)
    :keymaps 'override
    :prefix "SPC")
(general-create-definer aadi/leader-local-keys
    :states '(normal visual emacs)
    :keymaps 'override
    :prefix "SPC m")
(aadi/leader-keys
    "SPC" 'find-file
    "RET" 'denote-open-or-create)
(general-define-key
    :states 'motion
    "K" 'helpful-at-point
    "M-/" 'evilnc-comment-or-uncomment-lines)

(general-define-key
  :states 'normal
  "," 'kitty-async-process)

(aadi/leader-keys
     "z" 'org-agenda)

(general-define-key
    :keymaps 'dashboard-mode-map
    :states '(normal visual emacs)
    "RET" 'dashboard-return)

(aadi/leader-keys
    "g" '(:ignore t :which-key "magit")
    "g g" 'magit)

(aadi/leader-keys
   "n" '(:ignore t :which-key "denote")
   "n c" 'denote-create-note-in-subdirectory
   "n j" 'my-denote-journal
   "n n" 'denote
   "n N" 'denote-type
   "n d" 'denote-date
   "n s" 'denote-subdirectory
   "n t" 'denote-template
   "n i" 'denote-link
   "n I" 'denote-link-add-links
   "n b" 'denote-link-backlinks
   "n f f" 'denote-link-find-file
   "n f b" 'denote-link-find-backlink
   "n r" 'denote-rename-file
   "n R" 'denote-rename-file-using-front-matter)

(general-define-key
    :states 'normal
    "m" '(avy-goto-char :which-key "avy goto char"))

(aadi/leader-keys
    "b" '(:ignore t :which-key "buffer")
    "b b" 'consult-buffer
    "b k" 'kill-this-buffer)

(aadi/leader-keys
    "f" '(:ignore t :which-key "files")
    "f r" 'consult-recent-file)

(general-define-key
    :keymaps 'org-mode-map
    :states 'normal
    "?\t" 'org-cycle
    "<RET>" 'org-open-at-point
    "C-c a" 'link-hint-copy-link-at-point
    "z i" '(org-toggle-inline-images :whick-key "inline images"))

(aadi/leader-keys org-mode-map
    "m" '(:ignore t :which-key "localleader"))
(aadi/leader-local-keys org-mode-map
    "l" '(:ignore t :which-key "link")
    "l c" 'org-cliplink)

(aadi/leader-keys lsp-mode-map
    "m" '(:ignore t :which-key "localleader"))
(general-define-key
    :keymaps 'lsp-mode-map
    :states 'normal
    "K" 'lsp-describe-thing-at-point
    "C-c a" 'format-all-buffer)

(aadi/leader-local-keys
    :keymaps 'rustic-mode-map
    "z" 'Competitive-coding-output-input-toggle
    "r" 'rust-reset
    "i" 'code-input-refresh
    "f" 'copy-current-file
    "c" 'smart-compile)

(general-define-key
    :states '(normal emacs visual)
    "z" '(:ignore t :which-key "fold")
    "z c" 'vimish-fold-toggle
    "z a" 'vimish-fold-avy
    "z f" 'vimish-fold-refold-all
    "z u" 'vimish-fold-unfold-all)
