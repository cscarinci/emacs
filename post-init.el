;;; post-init.el -*- lexical-binding: t; -*-

(use-package server
  :ensure nil
  :config
  (unless (server-running-p)
    (server-start)))

(add-hook 'after-init-hook #'global-auto-revert-mode)
(add-hook 'after-init-hook #'savehist-mode)
(add-hook 'after-init-hook #'save-place-mode)
(add-hook 'after-init-hook #'(lambda()
                               (let ((inhibit-message t))
                                 (recentf-mode 1))))

(use-package recentf
  :config
  (setq recentf-exclude '("/tmp/"
                          "\\.pdf$"))
  (setq recentf-max-saved-items 2048))

(setq auto-save-default t)
(setq auto-save-interval 300)
(setq auto-save-timeout 30)

(global-visual-line-mode 1)

(global-hl-line-mode 1)

(display-line-numbers-mode 1)
(setq display-line-numbers-type 'relative
      display-line-numbers-width-start t)

;; (pixel-scroll-precision-mode 1)
;; (setq pixel-scroll-precision-use-momentum nil
;;       scroll-conservatively 101
;;       scroll-margin 0
;;       scroll-step 1
;;       auto-window-vscroll nil
;;       scroll-preserve-screen-position t
;;       fast-but-imprecise-scrolling t)

(use-package centered-cursor-mode
  :demand
  :config
  ;; Optional, enables centered-cursor-mode in all buffers.
  (global-centered-cursor-mode))

(use-package which-key
  :config
  (which-key-mode))

(use-package helpful
  :ensure t
  :bind
  (("C-h f" . helpful-callable)
   ("C-h v" . helpful-variable)
   ("C-h k" . helpful-key)))

(use-package ztree
  :ensure t)

(use-package magit
  :bind (("C-x g" . magit-status)
         ("C-x C-g" . magit-status))
  :ensure t)

(use-package undo-fu
  :ensure t
  :config
  (setq undo-limit 67108864)
  (setq undo-strong-limit 100663296)
  (setq undo-outer-limit 1006632960))

(use-package undo-fu-session
  :ensure t
  :config
  (undo-fu-session-global-mode))

(use-package evil
  :ensure t
  :init
  (setq evil-want-keybinding nil)
  (setq evil-want-fine-undo t)
  (setq evil-undo-system 'undo-fu)
  :bind (:map evil-normal-state-map
              ("u" . undo-fu-only-undo)
              ("C-r" . undo-fu-only-redo))
  :config
  (evil-mode 1)
  (global-unset-key (kbd "C-z"))
  (global-unset-key (kbd "C-S-z"))
  (define-key evil-normal-state-map (kbd "C-z") 'undo-fu-only-undo)
  (define-key evil-normal-state-map (kbd "C-S-z") 'undo-fu-only-redo)
  (define-key evil-motion-state-map (kbd "C-z") 'undo-fu-only-undo)
  (define-key evil-motion-state-map (kbd "C-S-z") 'undo-fu-only-redo)
  (define-key evil-insert-state-map (kbd "C-z") 'undo-fu-only-undo)
  (define-key evil-insert-state-map (kbd "C-S-z") 'undo-fu-only-redo)
  (define-key evil-visual-state-map (kbd "C-z") 'undo-fu-only-undo)
  (define-key evil-visual-state-map (kbd "C-S-z") 'undo-fu-only-redo)
  (define-key evil-replace-state-map (kbd "C-z") 'undo-fu-only-undo)
  (define-key evil-replace-state-map (kbd "C-S-z") 'undo-fu-only-redo))

(add-hook 'after-init-hook
          (lambda ()
            (evil-mode 1)))

(use-package evil-collection
  :ensure t
  :after evil
  :config
  (evil-collection-init))

(use-package corfu
  :ensure t
  :init
  (global-corfu-mode)
  :config
  (setq corfu-auto t
        corfu-cycle t
        corfu-preview-current nil))

;; Configure Tempel
(use-package tempel
  ;; Require trigger prefix before template name when completing.
  :custom
  (tempel-trigger-prefix "<")

  :bind (:map tempel-map
         ("TAB" . 'tempel-next))
  :init
  (setq tempel-path "./tempel")
  ;; Setup completion at point
  (defun tempel-setup-capf ()
    ;; Add the Tempel Capf to `completion-at-point-functions'.
    ;; `tempel-expand' only triggers on exact matches. Alternatively use
    ;; `tempel-complete' if you want to see all matches, but then you
    ;; should also configure `tempel-trigger-prefix', such that Tempel
    ;; does not trigger too often when you don't expect it. NOTE: We add
    ;; `tempel-expand' *before* the main programming mode Capf, such
    ;; that it will be tried first.
    (setq-local completion-at-point-functions
                (cons #'tempel-expand
                      completion-at-point-functions)))

  (add-hook 'conf-mode-hook 'tempel-setup-capf)
  (add-hook 'prog-mode-hook 'tempel-setup-capf)
  (add-hook 'text-mode-hook 'tempel-setup-capf)

  ;; Optionally make the Tempel templates available to Abbrev,
  ;; either locally or globally. `expand-abbrev' is bound to C-x '.
  ;; (add-hook 'prog-mode-hook #'tempel-abbrev-mode)
  ;; (global-tempel-abbrev-mode)
)

(use-package cape
  :ensure t
  :bind ("C-c p" . cape-prefix-map)
  :init
  (add-hook 'completion-at-point-functions #'cape-dabbrev)
  (add-hook 'completion-at-point-functions #'cape-file)
  (add-hook 'completion-at-point-functions #'cape-elisp-block)
  ;; (add-hook 'completion-at-point-functions #'cape-yasnippet)
  )

(use-package vertico
  :ensure t
  :init
  (vertico-mode))

(use-package orderless
  :ensure t
  :custom
  (completion-styles '(orderless basic))
  (completion-category-defaults nil)
  (completion-category-overrides '((file (styles partial-completion)))))

(use-package marginalia
  :ensure t
  :init
  (marginalia-mode))

(use-package embark
  :ensure t
  :bind
  (("C-." . embark-act)
   ("C-;" . embark-dwim)
   ("C-h B" . embark-bindings)))

(use-package embark-consult
  :ensure t
  :hook
  (embark-collect-mode . consult-preview-at-point-mode))

(use-package consult
  :ensure t
  :bind (("C-x b" . consult-buffer)
         ("C-x r b" . consult-bookmark)
         ("M-s d" . consult-find)
         ("M-s g" . consult-grep)
         ("M-s G" . consult-git-grep)
         ("M-s r" . consult-ripgrep)
         ("M-s l" . consult-line)))

(minimal-emacs-load-user-init "org-config.el")
(minimal-emacs-load-user-init "tex-config.el")

(provide 'post-init)

;; Local variables:
;; byte-compile-warnings: (not obsolete free-vars)
;; End:

;;; post-init.el ends here
