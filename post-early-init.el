;;; post-early-init.el -*- no-byte-compile: t; lexical-binding: t; -*-

(setq custom-file (expand-file-name "custom.el" minimal-emacs-var-dir))

(mapc #'disable-theme custom-enabled-themes)  ; Disable all active themes
(load-theme 'modus-vivendi-tinted t)  ; Load the built-in theme

(setq custom-theme-directory
      (expand-file-name "themes/" minimal-emacs-var-dir))

(provide 'post-early-init)

;;; post-early-init.el ends here
