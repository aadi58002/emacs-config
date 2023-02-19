(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(safe-local-variable-values
   '((git-commit-major-mode . git-commit-elisp-text-mode)
     (eval add-hook 'after-save-hook
           (lambda nil
             (org-babel-tangle))
           nil t))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(doom-themes-visual-bell ((t (:background "#00FFFF"))))
 '(emms-playlist-selected-face ((t (:foreground "royal blue"))))
 '(emms-playlist-track-face ((t (:foreground "#5da3e7"))))
 '(org-ellipsis ((t (:foreground "#C678DD")))))
