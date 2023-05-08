;;; init.el -*- lexical-binding: t; -*-
(defvar native-comp-deferred-compilation-deny-list nil)      ;;Native comp variable change in emacs 30
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
(setq display-line-numbers-type 'relative)

(setq custom-file (locate-user-emacs-file "custom.el"))
(load custom-file 'noerror 'nomessage)

(setq electric-pair-preserve-balance t)
(setq dired-recursive-copies 'always
      dired-recursive-deletes 'always)

(setq delete-by-moving-to-trash t
      dired-listing-switches "-AGFhlv --group-directories-first --time-style=long-iso"
      dired-dwim-target t
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
      delete-by-moving-to-trash t)

(put 'narrow-to-region 'disabled nil)

(electric-pair-mode 1)
(set-fringe-mode 10)
(setq auto-save-default t
      truncate-string-ellipsis "<>"
      which-key-idle-delay 0.5)
(setq x-stretch-cursor t
      window-combination-resize t
      global-auto-revert-mode 1
      global-auto-revert-non-file-buffers t)
(setq backup-directory-alist `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms `((".*" ,temporary-file-directory t)))

(autoload #'+org/dwim-at-point (concat user-emacs-directory "autoload/+org"))

(defun my/backward-kill-word ()
  "Kill backward to the beginning of the current word, but do not cross lines."
  (interactive)
  (if (not (looking-back "^\\s-*")) (backward-kill-word 1) (delete-horizontal-space)))

(defun adi--sudo-file-path (file)
  (let ((host (or (file-remote-p file 'host) "localhost")))
    (concat "/" (when (file-remote-p file)
                  (concat (file-remote-p file 'method) ":"
                          (if-let (user (file-remote-p file 'user))
                              (concat user "@" host)
                            host)
                          "|"))
            "sudo:root@" host
            ":" (or (file-remote-p file 'localname)
                    file))))

(defun adi/sudo-find-file (file)
  "Open FILE as root."
  (interactive "FOpen file as root: ")
  (find-file (adi--sudo-file-path file)))

(defun adi/sudo-this-file ()
  "Open the current file as root."
  (interactive)
  (find-file
   (adi--sudo-file-path
    (or buffer-file-name
        (when (or (derived-mode-p 'dired-mode)
                  (derived-mode-p 'wdired-mode))
          default-directory)))))

(defun adi/sudo-save-buffer ()
  "Save this file as root."
  (interactive)
  (let ((file (adi--sudo-file-path buffer-file-name)))
    (if-let (buffer (find-file-noselect file))
        (let ((origin (current-buffer)))
          (copy-to-buffer buffer (point-min) (point-max))
          (unwind-protect
              (with-current-buffer buffer
                (save-buffer))
            (unless (eq origin buffer)
              (kill-buffer buffer))
            (with-current-buffer origin
              (revert-buffer t t))))
      (user-error "Unable to open %S" file))))

(defun my-denote--add-todo-keyword ()
  "Add the todo keyword to the new captured note if it is under the Todo Sub directory"
  (let* ((file denote-last-path))
    (if (string= (file-name-directory file) (file-name-as-directory denote-todo-directory))
        (let* ((file-type (denote-filetype-heuristics file))
               (title (denote-retrieve-title-value file file-type))
               (keywords (denote-retrieve-keywords-value file file-type)))
          (denote-rename-file file title (append '("todo") keywords))))))

(defun my-denote--add-todo-or-archive-keyword (file file-type)
  "Replace todo with archive keyword (or vice versa) in FILE, given FILE-TYPE.
  See `my-denote-move-from-todo-to-archive'."
  (let* ((keywords (denote-retrieve-keywords-value file file-type)))
    (cond
     ((member "todo" keywords)
      (setq keywords (delete "todo" keywords)
            keywords (append '("archive") keywords)))
     ((member "archive" keywords)
      (setq keywords (delete "archive" keywords)
            keywords (append '("todo") keywords)))
     (t keywords))))

(defun my-denote-move-from-todo-to-archive ()
  (interactive)
  ;; Like the above example, but we pass values directly to
  ;; `denote-rename-file' instead of doing it interactively.  More
  ;; precisely, we re-use the existing title and keywords, while
  ;; adding "todo" to the list of keywords.
  (let* ((file (denote--rename-dired-file-or-prompt))
         (file-type (denote-filetype-heuristics file)))
    (denote-rename-file
     file
     (denote-retrieve-title-value file file-type)
     (my-denote--add-todo-or-archive-keyword file file-type)))
  (let* ((file (denote--rename-dired-file-or-prompt))
         (archive-target (string-replace "/Todo/" "/Archived/" file)))
    (rename-file file archive-target)
    (denote-update-dired-buffers)))

(defun random-element-of-list (items)
  ;; Selects a random element from a list
  (let* ((size (length items))
         (index (random size)))
    (nth index items)))

(defun Competitive-coding-output-input-toggle ()
  ;; Open side buffer to show inputf.in and outputf.in files as input and output of code file with the `SPC m z` Keybinding in rust-mode
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
  ;;Delete the entire buffer and expand a default template defined in `./templates` with the `SPC m r` Keybinding in rust-mode
  (interactive)
  (widen)
  (erase-buffer)
  (tempel-insert 'cp))

(defun code-input-refresh()
  ;; Places the clipboard content in the inputf.in file with the `SPC m i` Keybinding in rust-mode
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

(defun kitty-async-process ()
  "Launch a kitty terminal process in the current emacs directory"
  (interactive)
  (start-process "kitty" nil "setsid" "kitty" "-d" default-directory))

(defvar elpaca-installer-version 0.4)
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

(if (fboundp 'elpaca-wait)(elpaca-wait))

(use-package evil
  :init
  (setq evil-want-integration t) ;; This is optional since it's already set to t by default.
  (setq evil-want-keybinding nil)
  (setq evil-undo-system 'undo-tree)
  :config
  (evil-mode 1))

(setq evil-move-cursor-back nil
      evil-want-fine-undo t
      evil-move-beyond-eol t
      evil-respect-visual-line-mode t         ;; I don't know why this does not work and keep the visual selection after one indentation
      evil-org-retain-visual-state-on-shift t
      evil-search-module 'evil-search
      evil-vsplit-window-right t
      evil-split-window-below t)

(with-eval-after-load 'evil
  (with-eval-after-load 'elpaca-ui (evil-make-intercept-map elpaca-ui-mode-map))
  (with-eval-after-load 'elpaca-info (evil-make-intercept-map elpaca-info-mode-map)))

(use-package general
  :after (evil)
  :config
  (general-create-definer aadi/leader-keys
    :states '(normal motion visual operator emacs)
    :keymaps '(override global local)
    :prefix "SPC")
  (general-create-definer aadi/leader-local-keys
    :states '(normal motion visual operator emacs)
    :keymaps '(override global local)
    :prefix "SPC m")
  (general-override-mode)
  (general-auto-unbind-keys)
  (general-evil-setup t))

(if (fboundp 'elpaca-wait)(elpaca-wait))

(use-package evil-collection
  :after (evil)
  :config
  (evil-collection-init))

(use-package undo-tree
  :config
  (setq undo-tree-history-directory-alist `(("." . ,(concat user-emacs-directory "undotree")))
        undo-tree-visualizer-diff t
        undo-tree-auto-save-history t)
  (global-undo-tree-mode))

(use-package tramp
  :elpaca nil)

(use-package tempel
  :general
  (:states 'insert
           "C-s" 'tempel-complete)
  (:states '(insert normal)
           :keymaps 'tempel-map
           "S-TAB" 'tempel-previous
           "TAB" 'tempel-next)
  :config
  (global-tempel-abbrev-mode))

(use-package tempel-collection)

(use-package emms
  :config
  (emms-all)
  (setq emms-info-functions '(emms-info-mpd)
        emms-player-list '(emms-player-mpd)
        emms-player-mpd-music-directory "~/Music"
        emms-history-file nil
        emms-repeat-track t
        emms-mode-line-mode t
        emms-playlist-buffer-name "*Music*"
        emms-playing-time-mode t
        emms-info-asynchronously t
        emms-source-file-directory-tree-function 'emms-source-file-directory-tree-find)
  (emms-player-mpd-connect))

(use-package helpful
  :general
  (:prefix "C-h"
           "f" 'helpful-callable
           "v" 'helpful-variable
           "k" 'helpful-key
           "F" 'helpful-function
           "C" 'helpful-command))

(use-package unicode-fonts)

(use-package fontaine
  :config
  (setq fontaine-presets
        '((regular
           :default-height 100)
          (medium
           :default-weight semilight
           :default-height 140)
          (large
           :default-weight semilight
           :default-height 180
           :bold-weight extrabold)
          (t ; our shared fallback properties
           :default-family "CaskaydiaCove Nerd Font Mono"
           :default-weight normal)))
  (fontaine-set-preset 'regular))

(setq banner-icons-list (file-expand-wildcards (concat user-emacs-directory "icons/*")))
(use-package dashboard
  :after all-the-icons
  :general
  (dashboard-mode-map "RET" 'dashboard-return)
  :init
  (setq dashboard-items '((recents  . 5)
                          (agenda . 5)))
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
  (add-hook 'server-after-make-frame-functions
            (lambda (frame)
              (with-selected-frame frame
                (load-theme 'doom-dracula t))))
  (custom-set-faces
   '(doom-themes-visual-bell ((t (:background "#00FFFF"))))
   '(emms-playlist-selected-face ((t (:foreground "royal blue"))))
   '(emms-playlist-track-face ((t (:foreground "#5da3e7"))))
   '(emms-playlist-selected-face ((t (:foreground "royal blue"))))
   '(emms-playlist-track-face ((t (:foreground "#5da3e7"))))
   '(org-ellipsis (( t(:foreground "#C678DD"))))))

;; (use-package modus-themes
;;   :config
;;   (setq modus-themes-italic-constructs t
;;         modus-themes-bold-constructs t)
;;   (load-theme 'modus-vivendi-tinted t))

(use-package doom-modeline
  :elpaca (doom-modeline :host github :repo "seagle0128/doom-modeline")
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

;; (use-package kind-icon
;;   :after corfu
;;   :custom
;;   (kind-icon-default-face 'corfu-default) ; to compute blended backgrounds correctly
;;   :config
;;   (add-to-list 'corfu-margin-formatters #'kind-icon-margin-formatter))

(use-package all-the-icons-dired
  :config
  (add-hook 'dired-mode-hook 'all-the-icons-dired-mode))

;; (use-package gptel)

(use-package evil-nerd-commenter)

(use-package project)

(use-package flycheck
  :config
  (global-flycheck-mode 1))

(use-package flycheck-eglot
  :after (flycheck eglot)
  :config
  (global-flycheck-eglot-mode 1))

(use-package eglot
  :elpaca (eglot :host github :repo "joaotavora/eglot")
  :after (web-mode project)
  :hook ((prog-mode . eglot-ensure))
  :config

  ;; (add-to-list 'eglot-ignored-server-capabilities :hoverProvider)
  (setq eglot-events-buffer-size 0
        eglot-autoshutdown t)
  (add-hook 'eglot--managed-mode-hook (lambda () (flymake-mode -1)))

  ;; Yaml mode 
  (add-to-list 'auto-mode-alist '("\\.yml\\'" . yaml-ts-mode))
  (add-to-list 'auto-mode-alist '("\\.yaml\\'" . yaml-ts-mode))

  ;; Svelte Mode
  (define-derived-mode svelte-mode web-mode "Svelte")
  (add-to-list 'auto-mode-alist '("\\.svelte\\'" . svelte-mode))
  (add-to-list 'eglot-server-programs '(svelte-mode . ("svelteserver" "--stdio")))

  ;; C++ Mode
  (add-to-list 'eglot-server-programs `((c-mode c-ts-mode c++-mode c++-ts-mode)
                                        . ,(eglot-alternatives
                                            '("ccls" "clangd"))))
  ;; web-mode setup
  (define-derived-mode vue-mode web-mode "Vue")
  (add-to-list 'auto-mode-alist '("\\.vue\\'" . vue-mode))
  
  (defun vue-eglot-init-options ()
    (let ((tsdk-path (expand-file-name
                      "lib"
                      (shell-command-to-string "npm list --global --parseable typescript | head -n1 | tr -d \"\n\""))))
      `(:typescript (:tsdk ,tsdk-path
                           :languageFeatures (:completion
                                              (:defaultTagNameCase "both"
                                                                   :defaultAttrNameCase "kebabCase"
                                                                   :getDocumentNameCasesRequest nil
                                                                   :getDocumentSelectionRequest nil)
                                              :diagnostics
                                              (:getDocumentVersionRequest nil))
                           :documentFeatures (:documentFormatting
                                              (:defaultPrintWidth 100
                                                                  :getDocumentPrintWidthRequest nil)
                                              :documentSymbol t
                                              :documentColor t)))))
  
  ;; Volar
  (add-to-list 'eglot-server-programs
               `(vue-mode . ("vue-language-server" "--stdio" :initializationOptions ,(vue-eglot-init-options)))))

(use-package rustic
  :general
  :config
  (setq rustic-enable-detached-file-support t)
  (setq rustic-lsp-client 'eglot))

;; (use-package poetry)

;; Credits to karthink > https://github.com/karthink/project-x/blob/234f528bf3cf320b0d07ca61c6f9b2566167f0b3/project-x.el#L157
;; Recognize directories as projects by defining a new project backend `local'
;; -------------------------------------

(defcustom project-x-local-identifier '("package.json" "Cargo.toml" ".project")
  "Filename(s) that identifies a directory as a project.
You can specify a single filename or a list of names."
  :type '(choice (string :tag "Single file")
                 (repeat (string :tag "Filename")))
  :group 'project)

(cl-defmethod project-root ((project (head local)))
  "Return root directory of current PROJECT."
  (cdr project))

(defun project-x-try-local (dir)
  "Determine if DIR is a non-VC project.
DIR must include a .project file to be considered a project."
  (if-let ((root (if (listp project-x-local-identifier)
                     (seq-some (lambda (n)
                                 (locate-dominating-file dir n))
                               project-x-local-identifier)
                   (locate-dominating-file dir project-x-local-identifier))))
      (cons 'local root)))

(add-hook 'project-find-functions 'project-x-try-local 90)

;; (use-package protobuf-mode)

(use-package web-mode
  :config
  (setq web-mode-markup-indent-offset 2
        web-mode-code-indent-offset 2
        web-mode-css-indent-offset 2))

;; (use-package treesit
;;   :elpaca nil)

;; (use-package treesit-langs
;;   :elpaca (treesit-langs :host github :repo "kiennq/treesit-langs"))
(use-package tree-sitter
  :config
  (global-tree-sitter-mode))
(use-package tree-sitter-langs)

(use-package magit
  :general
  (:keymaps 'transient-map
            "<escape>" 'transient-quit-one)
  :config
  (add-hook 'git-commit-post-finish-hook 'magit)
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

(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

(use-package corfu
  :elpaca (corfu :host github :repo "minad/corfu" :files (:defaults "extensions/*.el"))
  :config
  ;; Setup corfu for popup like completion
  (setq corfu-cycle t  ; Allows cycling through candidates
        corfu-auto t   ; Enable auto completion
        corfu-auto-prefix 1  ; Complete with less prefix keys
        corfu-auto-delay 0.1  ; No delay for completion
        corfu-echo-documentation t ; Echo docs for current completion option
        corfu-popupinfo-delay 0.0
        corfu-quit-no-match 'separator
        corfu-quit-at-boundary 'separator)
  (global-corfu-mode 1)
  (corfu-popupinfo-mode 1))

(use-package cape
  :init
  (add-to-list 'completion-at-point-functions #'cape-file)
  (add-to-list 'completion-at-point-functions #'cape-dabbrev)


  ;; Silence the pcomplete capf, no errors or messages!
  (advice-add 'pcomplete-completions-at-point :around #'cape-wrap-silent)

  ;; Ensure that pcomplete does not write to the buffer
  ;; and behaves as a pure `completion-at-point-function'.
  (advice-add 'pcomplete-completions-at-point :around #'cape-wrap-purify))

(use-package embark
  :general
  ("C-;" 'embark-act)
  ("C-h B" 'embark-bindings)
  :init
  ;; Optionally replace the key help with a completing-read interface
  (setq prefix-help-command #'embark-prefix-help-command
        embark-quit-after-action nil)
  :config
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

(advice-add #'embark-completing-read-prompter :around #'embark-hide-which-key-indicator)

(use-package vertico
  :elpaca (vertico :files (:defaults "extensions/*.el"))
  :general
  (:keymaps 'vertico-map
            "C-j" 'vertico-next
            "C-k" 'vertico-previous)
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

;; A few more useful configurations...
(use-package emacs
  :elpaca nil
  :init
  ;; Add prompt indicator to `completing-read-multiple'.
  ;; We display [CRM<separator>], e.g., [CRM,] if the separator is a comma.
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

(use-package marginalia
  :config
  (marginalia-mode)
  (setq marginalia-align 'center
        marginalia-align-offset 20))

(use-package orderless
  :custom
  (orderless-matching-styles '(orderless-literal orderless-regexp orderless-flex))
  (completion-styles '(orderless))
  (completion-category-overrides '((file (styles partial-completion)))))

(use-package consult
  :general
  ("?" '(consult-line :which-key "filter buffer"))
  (:states '(normal motion)
           "g" '(:ignore t :which-key "goto")
           "g e" 'consult-compile-error
           "g l" 'consult-goto-line)
  ("M-C-'" 'consult-register-load)
  ("M-'" 'consult-register-store)
  ("M-\"" 'consult-register)
  :init
  ;; Optionally configure the register formatting. This improves the register
  ;; preview for `consult-register', `consult-register-load',
  ;; `consult-register-store' and the Emacs built-ins.
  (setq register-preview-delay 0.5
        register-preview-function #'consult-register-format)

  ;; Optionally tweak the register preview window.
  ;; This adds thin lines, sorting and hides the mode line of the window.
  (advice-add #'register-preview :override #'consult-register-window)

  ;; Use Consult to select xref locations with preview
  (setq xref-show-xrefs-function #'consult-xref
        xref-show-definitions-function #'consult-xref)

  ;; Enable using fd with consult
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
    (pcase-let* ((`(,prompt ,paths ,dir) (consult--directory-prompt "Fd" dir))
                 (default-directory dir))
      (find-file (consult--find prompt #'consult--fd-builder initial)))))

(use-package embark-consult
  :hook (embark-collect-mode . consult-preview-at-point-mode))

;; Automatically paste a online link with the description set to the title of the page
(use-package org-cliplink)

;; Don't want to create table of content manually in org mode
(use-package toc-org)

(use-package org
  :elpaca nil
  :general
  (:states '(normal motion)
           "C-c a" 'org-capture)
  (:keymaps 'org-mode-map
            :states 'normal
            "<RET>" '+org/dwim-at-point
            "?\t" 'org-cycle
            "z i" '(org-toggle-inline-images :whick-key "inline images"))
  (defadvice org-babel-execute-src-block (around load-language nil activate)
    "Load language if needed"
    (let ((language (org-element-property :language (org-element-at-point))))
      (unless (cdr (assoc (intern language) org-babel-load-languages))
        (add-to-list 'org-babel-load-languages (cons (intern language) t))
        (org-babel-do-load-languages 'org-babel-load-languages org-babel-load-languages))
      ad-do-it))

  ;; Life todo mangement with org mode and org agenda
  (setq org-log-done 'time)
  (setq org-todo-keywords
        '((sequence "TODO(t)" "PROJ(p)" "ACTIVE(a)" "REVIEW(r)" "START(s)" "NEXT(N)" "WORKING(w)" "HOLD(h)" "|" "DONE(d)" "KILL(k)")
          (sequence "|" "OKAY(o)" "YES(y)" "NO(n)")))

  (defun adi/org-setup()
    (org-indent-mode +1)
    (toc-org-mode +1))

  (add-hook 'org-mode-hook 'adi/org-setup))

(use-package org-modern
  :config
  (setq org-use-property-inheritance t ;;Might fix some bugs with org mode src block
        org-startup-indented t
        org-confirm-babel-evaluate nil
        org-src-preserve-indentation t
        org-export-preserve-breaks t
        org-log-into-drawer t
        org-link-file-path-type 'relative
        org-ellipsis "  "                                     ;;fun symbols   ,    , 
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
        org-ellipsis "…")
  (global-org-modern-mode))

(use-package org-agenda
  :elpaca nil
  :general
  (aadi/leader-keys
    "z" 'org-agenda)
  :init
  (setq org-agenda-files '("~/Documents/Denote/Todo/"))
  (setq org-agenda-window-setup 'current-window
        org-agenda-tags-column 0
        org-agenda-start-on-weekday nil
        org-agenda-block-separator ?─
        org-agenda-time-grid
        '((daily today require-timed)
          (800 1000 1200 1400 1600 1800 2000)
          " ┄┄┄┄┄ " "┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄")
        org-agenda-current-time-string
        "⭠ now ─────────────────────────────────────────────────"
        org-agenda-span 14
        org-agenda-start-day "-3d"
        org-agenda-inhibit-startup t))

(defvar denote-todo-directory)
(use-package denote
  :elpaca '(denote :host github :repo "protesilaos/denote")
  :config
  (setq denote-directory "~/Documents/Denote")
  (setq denote-todo-directory (concat (denote-directory) "Todo"))
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
                 (file file)
                 (function
                  (lambda ()
                    (let ((denote-directory (file-name-as-directory (concat (denote-directory) "Notes")))
                          (denote-org-capture-specifiers "%l\n%i* Notes: %?"))
                      (denote-org-capture)
                      )))
                 :no-save t
                 :immediate-finish nil
                 :kill-buffer t
                 :jump-to-captured t))
  (add-to-list 'org-capture-templates
               '("r" "Resources" plain
                 (file denote-last-path)
                 (function
                  (lambda ()
                    (let ((denote-directory (file-name-as-directory (concat (denote-directory) "Resources")))
                          (denote-org-capture-specifiers "%l\n%i\n* Resource for: %?"))
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
                    (let ((denote-directory (file-name-as-directory denote-todo-directory))
                          (denote-org-capture-specifiers "%l\n%i\n* TODO %?"))
                      (denote-org-capture))))
                 :no-save t
                 :immediate-finish nil
                 :kill-buffer t
                 :jump-to-captured t)))
(add-hook 'org-capture-after-finalize-hook 'my-denote--add-todo-keyword)

(if (fboundp 'elpaca-wait)(elpaca-wait))

(global-set-key (kbd "<escape>") 'keyboard-escape-quit)

(general-define-key
 :keymaps '(minibuffer-mode-map isearch-mode-map)
 "C-S-v" 'evil-paste-after)

(general-define-key
 :states '(normal motion operator emacs)
 :keymaps '(local global)

 "H" 'evil-beginning-of-line
 "L" 'evil-end-of-line)

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

(general-define-key
 :states 'insert
 "<C-backspace>" 'my/backward-kill-word)

(aadi/leader-keys
  "b" '(:ignore t :which-key "buffer")
  "b b" 'consult-buffer
  "b k" 'kill-this-buffer
  "c" '(:ignore t :which-key "commands")
  "c r" '(consult-complex-command :which-key "Complex Command repeat")
  "f" '(:ignore t :which-key "files")
  "f f" 'consult-fd
  "f g" 'consult-ripgrep
  "f b" 'consult-bookmark
  "f r" 'consult-recent-file
  "g s" 'consult-git-grep
  "m" '(:ignore t :which-key "mode")
  "m k" 'consult-kmacro
  )

(aadi/leader-keys
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

(aadi/leader-keys org-mode-map
  "m" '(:ignore t :which-key "org localleader")
  "a" 'my-denote-move-from-todo-to-archive)
(aadi/leader-local-keys org-mode-map
  "h" '(:ignore t :which-key "heading")
  "h h" 'consult-org-heading
  "l" '(:ignore t :which-key "link")
  "l c" 'org-cliplink)

(aadi/leader-keys
  :states '(normal motion)
  "g" '(:ignore t :which-key "git")
  "g g" 'magit)

(aadi/leader-keys :keymaps 'eglot-mode-map "m" '(:ignore t :which-key "eglot localleader"))
(aadi/leader-local-keys :keymaps 'eglot-mode-map "a" 'eglot-format)

(aadi/leader-local-keys
  :keymaps 'rustic-mode-map
  "z" 'Competitive-coding-output-input-toggle
  "r" 'rust-reset
  "i" 'code-input-refresh
  "f" 'copy-current-file
  "c" 'smart-compile)

(aadi/leader-keys
  "b f" 'fontaine-set-preset)
