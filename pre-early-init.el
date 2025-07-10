;;; pre-early-init.el -*- no-byte-compile: t; lexical-binding: t; -*-

(setq minimal-emacs-var-dir (expand-file-name ".var/" minimal-emacs-user-directory))
(setq package-user-dir (expand-file-name "elpa" minimal-emacs-var-dir))
(setq user-emacs-directory minimal-emacs-var-dir)

(defun display-startup-time ()
  "Display the startup time and number of garbage collections."
  (message "Emacs init loaded in %.2f seconds (Full emacs-startup: %.2fs) with %d garbage collections."
           (float-time (time-subtract after-init-time before-init-time))
           (time-to-seconds (time-since before-init-time))
           gcs-done))

(add-hook 'emacs-startup-hook #'display-startup-time 100)

;; Local variables:
;; byte-compile-warnings: (not obsolete free-vars)
;; End:

;;; pre-early-init.el ends here
