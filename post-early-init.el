;;; post-early-init.el -*- no-byte-compile: t; lexical-binding: t; -*-

(mapc #'disable-theme custom-enabled-themes)  ; Disable all active themes
(load-theme 'modus-vivendi-tinted t)  ; Load the built-in theme

;; Local variables:
;; byte-compile-warnings: (not obsolete free-vars)
;; End:

;;; post-early-init.el ends here
