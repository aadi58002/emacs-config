;;; init.el -*- lexical-binding: t; -*-
(defvar native-comp-deferred-compilation-deny-list nil)      ;;Native comp variable change in emacs 30
(setenv "LSP_USE_PLISTS" "1")                                ;;Use a faster implmentation for list for lsp mode
(setq create-lockfiles nil                                   ;;Don't want #..# files everywhere
      make-backup-files nil)                                 ;;Don't want Redundant copy of files
(setq recentf-max-menu-items 25)                             ;;Recent files opened list size
(setq revert-without-query '(".*"))                          ;;Keeps the file in sync with what is on the disk without a prompt to confirm
(recentf-mode 1)                                             ;;You can disable recent files here (just set 1 to -1)
(save-place-mode 1)                                          ;;When you open a file the cursor will be in the same position at which you closed the file
(setq read-process-output-max (* 1024 1024))                 ;;Emacs can read output from programs faster ( makes lsp mode faster )
(set-default-coding-systems 'utf-8)                          ;;Don't want to have encoding errors
(setq inhibit-startup-screen t)                              ;;Don't want to see the emacs startup screen
(setq visible-bell t)                                        ;;Blinks the top bar and modeline to the color set in doom-themes-visual-bell
(setq-default tab-width 4)                                   ;;The tab width battle continues
(setq-default evil-shift-width tab-width)                    ;;We want the tab width to be same in the vim mode of emacs
(setq-default indent-tabs-mode nil)                          ;;Don't want formatters to insert <TAB> just use spaces

(setq use-short-answers t)                                   ;; In prompt answer instead of typing complete yes with this y will work as well

(global-display-line-numbers-mode t)                         ;;All people like line numbers right
(setq display-line-numbers-type 'relative
      scroll-margin 30)

(setq custom-file (locate-user-emacs-file "custom.el"))
(load custom-file 'noerror 'nomessage)

(setq electric-pair-preserve-balance t)
(setq dired-recursive-copies 'always 
      dired-recursive-deletes 'always)

(setq delete-by-moving-to-trash t 
      dired-listing-switches "-AGFhlv --group-directories-first --time-style=long-iso")
(setq dired-dwim-target t
      dired-auto-revert-buffer #'dired-directory-changed-p
      dired-make-directory-clickable nil
      dired-free-space nil)

(setq resize-mini-windows t)

(add-hook 'dired-mode-hook #'hl-line-mode)
(setq dired-isearch-filenames 'dwim 
      dired-create-destination-dirs 'ask 
      dired-vc-rename-file t 
      dired-do-revert-buffer (lambda (dir  (not (file-remote-p dir ))) )
      dired-clean-up-buffers-too t 
      dired-clean-confirm-killing-deleted-buffers t 
      dired-x-hands-off-my-keys t     ; easier to show the keys I use
      dired-bind-man nil 
      dired-bind-info nil 
      delete-by-moving-to-trash t
      +vertico-consult-fd-args "fd -p --color=never -i --type f -E node_modules --regex")
(electric-pair-mode 1)
(set-fringe-mode 10)
(set-face-attribute 'default nil :font "JetBrains Mono" :height 100)
(setq auto-save-default t
      truncate-string-ellipsis "<>"
      which-key-idle-delay 0.5
      evil-snipe-scope 'whole-visible)
(setq x-stretch-cursor t
      window-combination-resize t
      global-auto-revert-mode 1
      global-auto-revert-non-file-buffers t)
(setq backup-directory-alist
      `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))

(autoload #'+org/dwim-at-point (concat user-emacs-directory "autoload/+org"))

;; (defun my-denote--add-todo-keyword ()
;;    "Add the todo keyword to the new captured note if it is under the Todo Sub directory"
;;     (let* ((file denote-last-path))
;;         (if (string= (file-name-directory file) (file-name-as-directory denote-todo-directory))
;;             (let* ((file-type (denote-filetype-heuristics file))
;;                 (title (denote-retrieve-title-value file file-type))
;;                 (keywords (denote-retrieve-keywords-value file file-type)))
;;                 (denote-rename-file file title (append '("todo") keywords))))))

;; (defun my-denote--add-todo-or-archive-keyword (file file-type)
;;   "Replace todo with archive keyword (or vice versa) in FILE, given FILE-TYPE.
;;   See `my-denote-move-from-todo-to-archive'."
;;   (let* ((keywords (denote-retrieve-keywords-value file file-type)))
;;     (cond
;;      ((member "todo" keywords)
;;       (setq keywords (delete "todo" keywords)
;;             keywords (append '("archive") keywords)))
;;      ((member "archive" keywords)
;;       (setq keywords (delete "archive" keywords)
;;             keywords (append '("todo") keywords)))
;;      (t keywords))))

;; (defun my-denote-move-from-todo-to-archive ()
;;   (interactive)
;;   ;; Like the above example, but we pass values directly to
;;   ;; `denote-rename-file' instead of doing it interactively.  More
;;   ;; precisely, we re-use the existing title and keywords, while
;;   ;; adding "todo" to the list of keywords.
;;   (let* ((file (denote--rename-dired-file-or-prompt))
;;          (file-type (denote-filetype-heuristics file)))
;;     (denote-rename-file
;;      file
;;      (denote-retrieve-title-value file file-type)
;;      (my-denote--add-todo-or-archive-keyword file file-type)))
;;   (let* ((file (denote--rename-dired-file-or-prompt))
;;          (archive-target (string-replace "/Todo/" "/Archived/" file)))
;;     (rename-file file archive-target)
;;     (denote-update-dired-buffers)))

(defun random-element-of-list (items)
  (let* ((size (length items))
         (index (random size)))
    (nth index items)))

;; (defun Competitive-coding-output-input-toggle ()
;;   (interactive)
;;   (delete-other-windows)
;;   (kill-matching-buffers "*.in")
;;   (evil-window-vsplit)
;;   (find-file (expand-file-name "inputf.in" default-directory))
;;   (evil-window-split)
;;   (find-file (expand-file-name "outputf.in" default-directory))
;;   (other-window 1)
;;   (enlarge-window-horizontally 40))

;; (defun rust-reset()
;;   (interactive)
;;   (widen)
;;   (erase-buffer)
;;   (insert "chef")
;;   (tempel-expand)
;;   (narrow-to-defun))

;; (defun code-input-refresh()
;;   (interactive)
;;   (write-region (current-kill 0) nil (concat default-directory "inputf.in") nil)
;;   (Competitive-coding-output-input-toggle))

;; source: http://steve.yegge.googlepages.com/my-dot-emacs-file
;; (defun copy-current-file (new-name)
;;     "Copy current file to a NEW-NAME."
;;     (interactive (list
;;                 (read-string "New name: " (current-kill 0) nil (current-kill 0))))
;;     (let ((name (buffer-name))
;;         (filename (buffer-file-name)))
;;     (if (not filename)
;;         (message "Buffer '%s' is not visiting a file!" name)
;;         (if (get-buffer new-name)
;;             (message "A buffer named '%s' already exists!" new-name)
;;             (copy-file filename (concat (replace-regexp-in-string " " "" (capitalize (replace-regexp-in-string "[^[:word:]_]" " " new-name))) ".rs") 1)))))

(defun kitty-async-process ()
  (interactive)
  (start-process "kitty" nil "setsid" "kitty" "-d" default-directory))

;; (defun brave-vscode-docs ()
;;   (interactive)
;;   (start-process "brave" nil "setsid" "brave" "--incognito" "https://code.visualstudio.com/api/language-extensions/language-server-extension-guide"))

(defvar elpaca-installer-version 0.2)
(defvar elpaca-directory (expand-file-name "elpaca/" user-emacs-directory))
(defvar elpaca-builds-directory (expand-file-name "builds/" elpaca-directory))
(defvar elpaca-repos-directory (expand-file-name "repos/" elpaca-directory))
(defvar elpaca-order '(elpaca :repo "https://github.com/progfolio/elpaca.git"
                              :ref nil
                              :files (:defaults (:exclude "extensions"))
                              :build (:not elpaca--activate-package)))
(when-let ((repo  (expand-file-name "elpaca/" elpaca-repos-directory))
           (build (expand-file-name "elpaca/" elpaca-builds-directory))
           (order (cdr elpaca-order))
           ((add-to-list 'load-path (if (file-exists-p build) build repo)))
           ((not (file-exists-p repo))))
  (condition-case-unless-debug err
      (if-let ((buffer (pop-to-buffer-same-window "*elpaca-installer*"))
               ((zerop (call-process "git" nil buffer t "clone"
                                     (plist-get order :repo) repo)))
               (default-directory repo)
               ((zerop (call-process "git" nil buffer t "checkout"
                                     (or (plist-get order :ref) "--"))))
               (emacs (concat invocation-directory invocation-name))
               ((zerop (call-process emacs nil buffer nil "-Q" "-L" "." "--batch"
                                     "--eval" "(byte-recompile-directory \".\" 0 'force)"))))
          (progn (require 'elpaca)
                 (elpaca-generate-autoloads "elpaca" repo)
                 (kill-buffer buffer))
        (error "%s" (with-current-buffer buffer (buffer-string))))
    ((error) (warn "%s" err) (delete-directory repo 'recursive))))
(require 'elpaca-autoloads)
(add-hook 'after-init-hook #'elpaca-process-queues)
(elpaca `(,@elpaca-order))

;; Install use-package support
(elpaca elpaca-use-package
  ;; Enable :elpaca use-package keyword.
  (elpaca-use-package-mode)
  ;; Assume :elpaca t unless otherwise specified.
  (setq elpaca-use-package-by-default t))

;; Block until current queue processed.
(elpaca-wait)

;; (defvar bootstrap-version)
;; (let ((bootstrap-file
;;          (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
;;         (bootstrap-version 6))
;;     (unless (file-exists-p bootstrap-file)
;;       (with-current-buffer
;;           (url-retrieve-synchronously
;;            "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
;;            'silent 'inhibit-cookies)
;;         (goto-char (point-max))
;;         (eval-print-last-sexp)))
;;     (load bootstrap-file nil 'nomessage))
;; (setq-default straight-vc-git-default-clone-depth '(1 single-branch))
;; (setq straight-use-package-by-default t) 
;; (straight-use-package 'use-package)

;; (let ((straight-x-file (expand-file-name "straight/repos/straight.el/straight-x.el" user-emacs-directory)))
;;   (if (file-exists-p straight-x-file) (load straight-x-file)))

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

;; (use-package docker
;;    :config
;;    (setq tramp-docker-program "podman"
;;          docker-command "podman"
;;          docker-composee-command "podman-compose"
;;          tramp-docker-method "podman"))

(use-package pdf-tools
   :config
   (add-to-list 'auto-mode-alist '("\\.pdf\\'" . pdf-view-mode)))

(use-package tempel
  :init
  (global-tempel-abbrev-mode))

(use-package tempel-collection)

(use-package ace-window
    :config
    (setq aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l)))

(use-package pulsar
   :config
   (setq pulsar-pulse t 
         pulsar-delay 0.055 
         pulsar-iterations 10 
         pulsar-face 'pulsar-magenta
         pulsar-highlight-face 'pulsar-yellow)
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

;; (use-package emms
;;   :init
;;   (require 'emms-setup)
;;   (emms-all)
;;   (setq emms-source-file-default-directory "~/Music/"
;;         emms-info-functions '(emms-info-native)
;;         emms-player-list '(emms-player-vlc)
;;         emms-repeat-track t
;;         emms-mode-line-mode t
;;         emms-playlist-buffer-name "*Music*"
;;         emms-playing-time-mode t
;;         emms-info-asynchronously t
;;         emms-source-file-directory-tree-function 'emms-source-file-directory-tree-find)
;;   (emms-add-directory-tree "~/Music/")
;;   (emms-add-directory-tree "~/Videos/Test Video"))

(use-package helpful)

(use-package avy
     :config
     (setq avy-background t)
     (avy-setup-default))

(use-package undo-fu)

(use-package undohist
  :init
  (setq undo-tree-history-directory-alist '(((concat user-emacs-directory "/undohist"))))
  :config
  (undohist-initialize))

(use-package savehist
  :elpaca nil
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
    (doom-themes-visual-bell-config)
    (load-theme 'doom-dracula t)
    (custom-set-faces
        '(doom-themes-visual-bell (( t(:background "#00FFFF"))))
        ;; '(emms-playlist-selected-face (( t(:foreground "royal blue"))))
        ;; '(emms-playlist-track-face (( t(:foreground "#5da3e7"))))
        ;; '(emms-playlist-selected-face (( t(:foreground "royal blue"))))
        ;; '(emms-playlist-track-face (( t(:foreground "#5da3e7"))))
        '(org-ellipsis (( t(:foreground "#C678DD"))))))

;; (use-package modus-themes
;;    :config
;;    (setq modus-themes-italic-constructs t
;;          modus-themes-bold-constructs t)
;;    (load-theme 'modus-vivendi t))

(use-package doom-modeline
    :init (doom-modeline-mode 1)
    :config
    (display-battery-mode 1)
    (setq doom-modeline-project-detection 'truncate-upto-project
          doom-modeline-enable-word-count t
          doom-modeline-buffer-encoding nil
          doom-modeline-env-version t
          doom-modeline-hud t))

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
  (setq lsp-rust-analyzer-display-chaining-hints t
        lsp-rust-analyzer-expand-macro t
        lsp-rust-analyzer-display-parameter-hints t
        lsp-rust-analyzer-server-display-inlay-hints t))

(use-package typescript-mode)

(setq web-mode-markup-indent-offset 2
      web-mode-code-indent-offset 2
      web-mode-css-indent-offset 2)
(use-package web-mode
    :commands web-mode)

;; (add-to-list 'auto-mode-alist '("\\.svelte\\'" . web-mode))
;; (setq web-mode-engines-alist
;;     '(("svelte" . "\\.svelte\\'")))

;; (use-package ccls)

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
          lsp-enable-snippet nil
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
        (add-to-list 'tree-sitter-major-mode-language-alist '(typescript-ts-mode . tsx))
        (global-tree-sitter-mode)
        (add-hook 'tree-sitter-after-on-hook #'tree-sitter-hl-mode))

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
  (projectile-mode +1))

(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

(use-package corfu
  :elpaca (corfu :files (:defaults "extensions/*"))
  :init
  ;; Setup corfu for popup like completion
  (setq corfu-cycle t  ; Allows cycling through candidates
        corfu-auto t   ; Enable auto completion
        corfu-auto-prefix 1  ; Complete with less prefix keys
        corfu-auto-delay 0.0  ; No delay for completion
        corfu-popupinfo-delay 0.0  ; No delay for completion
        corfu-echo-documentation nil  ; Echo docs for current completion option
        corfu-quit-at-boundary 'insert)
  (corfu-history-mode 1)
  (global-corfu-mode 1)
  (advice-add #'lsp-completion-at-point :around #'cape-wrap-noninterruptible)
  (define-key corfu-map "\M-m" #'corfu-move-to-minibuffer))

(defun corfu-enable-in-minibuffer ()
  "Enable Corfu in the minibuffer if `completion-at-point' is bound."
  (when (where-is-internal #'completion-at-point (list (current-local-map)))
    (setq-local corfu-auto t) ;; Enable/disable auto completion
    (setq-local corfu-echo-delay 0.0 ;; Disable automatic echo and popup
                corfu-popupinfo-delay 0.0)
    (corfu-mode 1)))

(add-hook 'minibuffer-setup-hook #'corfu-enable-in-minibuffer)

(defun corfu-move-to-minibuffer ()
  (interactive)
  (let ((completion-extra-properties corfu--extra)
        completion-cycle-threshold completion-cycling)
    (apply #'consult-completion-in-region completion-in-region--data)))

(use-package emacs
  :elpaca nil
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
    (setq enable-recursive-minibuffers t
          completion-cycle-threshold 3
          tab-always-indent 'complete))

(use-package cape
    :init
    (add-to-list 'completion-at-point-functions #'cape-file)
    (add-to-list 'completion-at-point-functions #'cape-dabbrev))

(use-package embark
        :bind
        (("C-;" . embark-act)         ;; pick some comfortable binding
         ("M-." . embark-dwim)        ;; good alternative: M-.
         ("C-h B" . embark-bindings)) ;; alternative for `describe-bindings'

        :init

        ;; Optionally replace the key help with a completing-read interface
        (setq prefix-help-command #'embark-prefix-help-command
              embark-quit-after-action nil)

        :config
        ;; (define-key embark-symbol-map "D" #'devdocs-lookup)
        ;; (define-key embark-function-map "D" #'devdocs-lookup)

        ;; Hide the mode line of the Embark live/completions buffers
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

(use-package vertico
    :init
    (setq vertico-count 20
          vertico-resize nil
          vertico-cycle t)
    (vertico-mode))


(defun +embark-live-vertico ()
  "Shrink Vertico minibuffer when `embark-live' is active."
  (when-let (win (and (string-prefix-p "*Embark Live" (buffer-name))
                      (active-minibuffer-window)))
    (with-selected-window win
      (when (and (bound-and-true-p vertico--input)
                 (fboundp 'vertico-multiform-unobtrusive))
        (vertico-multiform-unobtrusive)))))

(add-hook 'embark-collect-mode-hook #'+embark-live-vertico)

(use-package marginalia
  :config
  (marginalia-mode)
  (setq marginalia-align 'center
    marginalia-align-offset 20))

(use-package orderless
    :custom
    ;; (orderless-matching-styles '(orderless-literal orderless-regexp orderless-flex))
    (completion-styles '(orderless))
    (completion-category-overrides '((file (styles partial-completion)))))

(defvar consult--fd-command nil)
(defun consult--fd-builder (input)
  (unless consult--fd-command
    (setq consult--fd-command
          (if (eq 0 (call-process-shell-command "fdfind"))
              "fdfind"
            "fd")))
  (pcase-let* ((`(,arg . ,opts) (consult--command-split input))
               (`(,re . ,hl) (funcall consult--regexp-compiler
                                      arg 'extended t)))
    (when re
      (cons (append
             (list consult--fd-command
                   "--color=never" "--full-path"
                   (consult--join-regexps re 'extended))
             opts)
            hl))))

(defun consult-fd (&optional dir initial)
  (interactive "P")
  (let* ((prompt-dir (consult--directory-prompt "Fd" dir))
         (default-directory (cdr prompt-dir)))
    (find-file (consult--find (car prompt-dir) #'consult--fd-builder initial))))

(use-package consult
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
   (defun consult--orderless-regexp-compiler (input type &rest _config)
        (setq input (orderless-pattern-compiler input))
        (cons
        (mapcar (lambda (r) (consult--convert-regexp r type)) input)
        (lambda (str) (orderless--highlight input str))))

  (setq consult--regexp-compiler #'consult--orderless-regexp-compiler)
  (setq consult-narrow-key "<")) ;; (kbd "C-+")

(use-package embark-consult
  :hook
  (embark-collect-mode . consult-preview-at-point-mode))

(defadvice org-babel-execute-src-block (around load-language nil activate)
    "Load language if needed"
    (let ((language (org-element-property :language (org-element-at-point))))
        (unless (cdr (assoc (intern language) org-babel-load-languages))
        (add-to-list 'org-babel-load-languages (cons (intern language) t))
        (org-babel-do-load-languages 'org-babel-load-languages org-babel-load-languages))
        ad-do-it))

(defun adi/org-setup()
    (org-indent-mode +1)
    (toc-org-mode +1))

(add-hook 'org-mode-hook 'adi/org-setup)

(use-package evil-org
    :config
    (evil-org-mode +1))

(use-package org-cliplink)

(use-package link-hint)

(use-package toc-org)  
(add-hook 'org-mode-hook (lambda () (toc-org-mode 1)))

(use-package org-modern
   :config
    (setq org-use-property-inheritance t ;;Might fix some bugs with org mode src block
          org-startup-indented t
          org-confirm-babel-evaluate nil
          org-src-preserve-indentation t
          org-export-preserve-breaks t
          org-log-into-drawer t
          org-link-file-path-type 'relative
          org-agenda-start-on-weekday nil
          ;; org-ellipsis "  "                                     ;;fun symbols   ,    , 
          org-enforce-todo-checkbox-dependencies t
          org-enforce-todo-dependencies t
          org-auto-align-tags nil
          org-tags-column 0
          org-catch-invisible-edits 'show-and-error
          org-modern-checkbox nil
          org-modern-table nil
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

(setq org-log-done 'time)
(setq org-todo-keywords
    '((sequence "TODO(t)" "PROJ(p)" "ACTIVE(a)" "REVIEW(r)" "START(s)" "NEXT(N)" "WORKING(w)" "HOLD(h)" "|" "DONE(d)" "KILL(k)")
        (sequence "|" "OKAY(o)" "YES(y)" "NO(n)")))

;; (setq org-agenda-files '("~/Documents/Denote/Todo/"))
(setq org-agenda-window-setup 'current-window
    org-agenda-span 14
    org-agenda-start-day "-3d"
    org-agenda-inhibit-startup t)

;; (defvar denote-todo-directory)
;; (use-package denote
;;     :elpaca '(denote :host github :repo "protesilaos/denote")
;;     :config
;;     (setq denote-directory "~/Documents/Denote")
;;     (setq denote-todo-directory (concat (denote-directory) "Todo"))
;;     (setq denote-known-keywords '())
;;     (setq denote-infer-keywords t)
;;     (setq denote-sort-keywords t)
;;     (setq denote-excluded-directories-regexp nil)
;;     (setq denote-excluded-keywords-regexp nil)
;;     (setq denote-date-prompt-use-org-read-date t)
;;     (setq denote-backlinks-show-context t))

;; (with-eval-after-load 'org-capture
;;     (add-to-list 'org-capture-templates
;;                '("n" "Notes" plain
;;                 (file file)
;;                 (function
;;                     (lambda ()
;;                         (let ((denote-directory (file-name-as-directory (concat (denote-directory) "Notes")))
;;                               (denote-org-capture-specifiers "%l\n%i* Notes: %?"))
;;                             (denote-org-capture)
;;                 )))
;;                 :no-save t
;;                 :immediate-finish nil
;;                 :kill-buffer t
;;                 :jump-to-captured t))
;;     (add-to-list 'org-capture-templates
;;                '("r" "Resources" plain
;;                 (file denote-last-path)
;;                 (function
;;                     (lambda ()
;;                         (let ((denote-directory (file-name-as-directory (concat (denote-directory) "Resources")))
;;                               (denote-org-capture-specifiers "%l\n%i\n* Resource for: %?"))
;;                             (denote-org-capture))))
;;                 :no-save t
;;                 :immediate-finish nil
;;                 :kill-buffer t
;;                 :jump-to-captured t))
;;     (add-to-list 'org-capture-templates
;;                '("t" "Todo" plain
;;                 (file denote-last-path)
;;                 (function
;;                     (lambda ()
;;                         (let ((denote-directory (file-name-as-directory denote-todo-directory))
;;                               (denote-org-capture-specifiers "%l\n%i\n* TODO %?"))
;;                             (denote-org-capture))))
;;                 :no-save t
;;                 :immediate-finish nil
;;                 :kill-buffer t
;;                 :jump-to-captured t)))
;; (add-hook 'org-capture-after-finalize-hook 'my-denote--add-todo-keyword)

(elpaca-wait)

(global-set-key (kbd "<escape>") 'keyboard-escape-quit)
(global-set-key (kbd "C-;") 'embark-act)
(general-define-key
    :keymaps 'evil-window-map
    "C-w" 'ace-window)
(keymap-set minibuffer-mode-map "C-S-v" 'evil-paste-after)
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

(general-define-key
    :keymaps 'transient-map
    "<escape>" 'transient-quit-one)
(aadi/leader-keys
    :states '(normal motion)
    "g" '(:ignore t :which-key "git")
    "g s" 'consult-git-grep
    "g g" 'magit)

(aadi/leader-keys
    :states '(normal motion)
    "n" '(:ignore t :which-key "denote")
    "n c" 'denote-create-note-in-subdirectory
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

(aadi/leader-keys
    :keymaps 'projectile-mode-map
    :states '(normal motion)
    "p" '(projectile-command-map :whick-key "projects"))

;; (general-define-key
;;   "M-S-x" 'execute-extended-command
;;   "M-x" 'consult-mode-command)

(aadi/leader-keys
    :states '(normal motion)
    "m" '(:ignore t :which-key "mode")
    "m k" 'consult-kmacro)

(aadi/leader-keys
    :states '(normal motion)
    "c" '(:ignore t :which-key "commands")
    "c r" '(consult-complex-command :which-key "Complex Command repeat"))

(aadi/leader-keys
    :states '(normal motion)
    "f" '(:ignore t :which-key "files")
    "f b" 'consult-bookmark
    "f r" 'consult-recent-file)

(general-define-key
    [remap projectile-ripgrep] 'consult-ripgrep
    [remap projectile-find-file] 'consult-find)

(general-define-key
    :states '(normal motion)
    "g" '(:ignore t :which-key "goto"))

(general-define-key
    :states '(normal motion)
    :prefix "g"
    "e" 'consult-compile-error
    "f" 'consult-flycheck
    "l" 'consult-goto-line)

(general-define-key
    :states '(normal motion)
    "M-C-'" 'consult-register-load
    "M-'" 'consult-register-store
    "M-\"" 'consult-register)

(general-define-key
    :states '(normal motion)
    ";" '(avy-goto-char :which-key "avy goto char"))

(aadi/leader-keys
    :states '(normal motion)
    "b" '(:ignore t :which-key "buffer")
    "b f" '(consult-line :which-key "filter buffer")
    "b b" 'consult-buffer
    "b B" 'bookmark-bmenu-list
    "b k" 'kill-this-buffer)

(general-define-key
    :states '(normal motion)
    "C-c a" 'org-capture)
(general-define-key
    :keymaps 'org-mode-map
    :states 'normal
    "<RET>" '+org/dwim-at-point
    "?\t" 'org-cycle
    "C-c a" 'link-hint-copy-link-at-point
    "z i" '(org-toggle-inline-images :whick-key "inline images"))

(aadi/leader-keys org-mode-map
    "m" '(:ignore t :which-key "org localleader"))
    ;; "a" 'my-denote-move-from-todo-to-archive)
(aadi/leader-local-keys org-mode-map
    "h" '(:ignore t :which-key "heading")
    "h h" 'consult-org-heading
    "l" '(:ignore t :which-key "link")
    "l c" 'org-cliplink)

(aadi/leader-keys lsp-mode-map
    "m" '(:ignore t :which-key "lsp localleader"))
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

(general-define-key
   :prefix "C-h"
   "f" #'helpful-callable
   "v" #'helpful-variable
   "k" #'helpful-key
   "F" #'helpful-function
   "C" #'helpful-command)

(general-define-key
   :states 'insert
   "C-s" 'tempel-complete)
(general-define-key
   :keymaps 'tempel-map
   "S-TAB" 'tempel-previous
   "TAB" 'tempel-next)
