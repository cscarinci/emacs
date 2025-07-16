;;; post-init.el -*- no-byte-compile: t; lexical-binding: t; -*-

(use-package server
  :ensure nil
  :config
  (unless (server-running-p)
    (server-start)))

(add-hook 'after-init-hook #'lost-selection-mode)
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
  :demand t)

(use-package which-key
  :config
  (which-key-mode))

;;; DIRED
(use-package dired
  :ensure nil
  :bind
  (("M-i" . emacs-solo/window-dired-vc-root-left))
  :custom
  (dired-dwim-target t)
  (dired-guess-shell-alist-user
   '(("\\.\\(png\\|jpe?g\\|tiff\\)" "feh" "xdg-open" "open")
     ("\\.\\(mp[34]\\|m4a\\|ogg\\|flac\\|webm\\|mkv\\)" "mpv" "xdg-open" "open")
     (".*" "xdg-open" "open")))
  (dired-kill-when-opening-new-dired-buffer t)
  (dired-listing-switches "-alh --group-directories-first")
  (dired-omit-files "^\\.")                                ; with dired-omit-mode (C-x M-o)
  (dired-hide-details-hide-absolute-location t)            ; EMACS-31
  :init
  (add-hook 'dired-mode-hook (lambda () (dired-omit-mode 1))) ;; Turning this ON also sets the C-x M-o binding.

  (defun emacs-solo/dired-rsync-copy (dest)
    "Copy marked files in Dired to DEST using rsync async, with real-time processing of output."
    (interactive
     (list (expand-file-name (read-file-name "rsync to: "
                                             (dired-dwim-target-directory)))))
    (let* ((files (dired-get-marked-files nil current-prefix-arg))
           (dest-original dest)
           (dest-rsync
            (if (file-remote-p dest)
                (let ((vec (tramp-dissect-file-name dest)))
                  (concat (tramp-file-name-user vec)
                          "@"
                          (tramp-file-name-host vec)
                          ":"
                          (tramp-file-name-localname vec)))
              dest))
           (files-rsync
            (mapcar
             (lambda (f)
               (if (file-remote-p f)
                   (let ((vec (tramp-dissect-file-name f)))
                     (concat (tramp-file-name-user vec)
                             "@"
                             (tramp-file-name-host vec)
                             ":"
                             (tramp-file-name-localname vec)))
                 f))
             files))
           (command (append '("rsync" "-hPur") files-rsync (list dest-rsync)))
           (buffer (get-buffer-create "*rsync*")))

      (message "[rsync] original dest: %s" dest-original)
      (message "[rsync] converted dest: %s" dest-rsync)
      (message "[rsync] source files: %s" files-rsync)
      (message "[rsync] command: %s" (string-join command " "))

      (with-current-buffer buffer
        (erase-buffer)
        (insert "Running rsync...\n"))

      (defun rsync-process-filter (proc string)
        (with-current-buffer (process-buffer proc)
          (goto-char (point-max))
          (insert string)
          (goto-char (point-max))
          (while (re-search-backward "\r" nil t)
            (replace-match "\n" nil nil))))

      (make-process
       :name "dired-rsync"
       :buffer buffer
       :command command
       :filter 'rsync-process-filter
       :sentinel
       (lambda (_proc event)
         (when (string-match-p "finished" event)
           (with-current-buffer buffer
             (goto-char (point-max))
             (insert "\n* rsync done *\n"))
           (dired-revert)))
       :stderr buffer)

      (display-buffer buffer)
      (message "rsync started...")))


  (defun emacs-solo/window-dired-vc-root-left (&optional directory-path)
    "Creates *Dired-Side* like an IDE side explorer"
    (interactive)
    (add-hook 'dired-mode-hook 'dired-hide-details-mode)

    (let ((dir (if directory-path
                   (dired-noselect directory-path)
                 (if (eq (vc-root-dir) nil)
                     (dired-noselect default-directory)
                   (dired-noselect (vc-root-dir))))))

      (display-buffer-in-side-window
       dir `((side . left)
             (slot . 0)
             (window-width . 30)
             (window-parameters . ((no-other-window . t)
                                   (no-delete-other-windows . t)
                                   (mode-line-format . (" "
                                                        "%b"))))))
      (with-current-buffer dir
        (let ((window (get-buffer-window dir)))
          (when window
            (select-window window)
            (rename-buffer "*Dired-Side*")
            )))))

  (defun emacs-solo/window-dired-open-directory ()
    "Open the current directory in *Dired-Side* side window."
    (interactive)
    (emacs-solo/window-dired-vc-root-left (dired-get-file-for-visit)))

  (defun emacs-solo/window-dired-open-directory-back ()
    "Open the parent directory in *Dired-Side* side window and refresh it."
    (interactive)
    (emacs-solo/window-dired-vc-root-left "../")
    (when (get-buffer "*Dired-Side*")
      (with-current-buffer "*Dired-Side*"
        (revert-buffer t t))))

  (eval-after-load 'dired
    '(progn
       ;; Users should navigate with p/n, enter new directories with =, go back with q,
       ;; quit with several q's, only use - to access stuff up on the tree from inicial
       ;; directory.
       (define-key dired-mode-map (kbd "=") 'emacs-solo/window-dired-open-directory)
       (define-key dired-mode-map (kbd "-") 'emacs-solo/window-dired-open-directory-back)

       ;; A better "BACK" keybiding
       (define-key dired-mode-map (kbd "b") 'dired-up-directory))))


;;; WDIRED
(use-package wdired
  :ensure nil
  :commands (wdired-change-to-wdired-mode)
  :config
  (setq wdired-allow-to-change-permissions t)
  (setq wdired-create-parent-directories t))

(use-package helpful
  :ensure t
  :bind
  (("C-h f" . helpful-callable)
   ("C-h v" . helpful-variable)
   ("C-h k" . helpful-key)))

(use-package ztree
  :ensure t)

(use-package eat
  :config
  (setq explicit-shell-file-name "/usr/bin/zsh")
  :bind ("C-c C-t" . eat)
  :hook ('eshell-load-hook #'eat-eshell-mode))
(use-package popper
  :ensure t ; or :straight t
  :bind (("C-`"   . popper-toggle)
         ("M-`"   . popper-cycle)
         ("C-M-`" . popper-toggle-type))
  :init
  (setq popper-reference-buffers
        '("\\*Messages\\*"
          "\\*eat\\*"
          "Output\\*$"
          "\\*Async Shell Command\\*"
          help-mode
          compilation-mode))
  (popper-mode +1)
  (popper-echo-mode +1))                ; For echo area hints

; (use-package magit
;   :bind (("C-x g" . magit-status)
;          ("C-x C-g" . magit-status))
;   :ensure t)

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
  (setq select-enable-clipboard t)
  (setq evil-want-keybinding nil)
  (setq evil-want-fine-undo t)
  
  (setq evil-undo-system 'undo-fu)
  :bind (:map evil-normal-state-map
              ("u" . undo-fu-only-undo)
              ("C-r" . undo-fu-only-redo))
  :config
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
  (define-key evil-replace-state-map (kbd "C-S-z") 'undo-fu-only-redo)
  (define-key evil-normal-state-map "Y" "y$"))

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
  (setq tempel-path "~/.emacs.d/tempel")
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
         ("C-x C-b" . consult-project-buffer)
         ("M-s d" . consult-find)
         ("M-s g" . consult-grep)
         ("M-s G" . consult-git-grep)
         ("M-s r" . consult-ripgrep)
         ("M-s l" . consult-line)
         ("C-s" . consult-line)))

(minimal-emacs-load-user-init "org-config.el")
(minimal-emacs-load-user-init "tex-config.el")

(provide 'post-init)

;;; post-init.el ends here
