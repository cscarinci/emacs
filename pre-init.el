;;; pre-init.el -*- lexical-binding: t; -*-

(use-package modus-themes
  :ensure nil
  :defer t
  :custom
  (modus-themes-italic-constructs t)
  (modus-themes-bold-constructs t)
  (modus-themes-mixed-fonts nil)
  (modus-themes-prompts '(bold intense))
  (modus-themes-common-palette-overrides
   `((bg-main "#292D3E")
     (bg-active bg-main)
     (fg-main "#EEFFFF")
     (fg-active fg-main)
     (fg-mode-line-active "#A6Accd")
     (bg-mode-line-active "#232635")
     (fg-mode-line-inactive "#676E95")
     (bg-mode-line-inactive "#282c3d")
     ;; (border-mode-line-active "#676E95")
     ;; (border-mode-line-inactive bg-dim)
     (border-mode-line-active nil)
     (border-mode-line-inactive nil)
     (bg-tab-bar      "#242837")
     (bg-tab-current  bg-main)
     (bg-tab-other    "#242837")
     (fg-prompt "#c792ea")
     (bg-prompt unspecified)
     (bg-hover-secondary "#676E95")
     (bg-completion "#2f447f")
     (fg-completion white)
     (bg-region "#3C435E")
     (fg-region white)

     (fg-line-number-active fg-main)
     (fg-line-number-inactive "gray50")
     (bg-line-number-active unspecified)
     (bg-line-number-inactive "#292D3E")
     (fringe "#292D3E")

     (fg-heading-0 "#82aaff")
     (fg-heading-1 "#82aaff")
     (fg-heading-2 "#c792ea")
     (fg-heading-3 "#bb80b3")
     (fg-heading-4 "#a1bfff")

     (fg-prose-verbatim "#c3e88d")
     (bg-prose-block-contents "#232635")
     (fg-prose-block-delimiter "#676E95")
     (bg-prose-block-delimiter bg-prose-block-contents)

     (accent-1 "#79a8ff")

     (keyword "#89DDFF")
     (builtin "#82aaff")
     (comment "#676E95")
     (string "#c3e88d")
     (fnname "#82aaff")
     (type "#c792ea")
     (variable "#c792ea")
     (docstring "#8d92af")
     (constant "#f78c6c")))
  :config
  (modus-themes-with-colors
    (custom-set-faces
     `(tab-bar
       ((,c
         :background "#232635"
         :foreground "#A6Accd"
         ;; :box (:line-width 1 :color "#676E95")
         )))
     `(tab-bar-tab
       ((,c
         ;; :background "#232635"
         ;; :underline t
         ;; :box (:line-width 1 :color "#676E95")
         )))
     `(tab-bar-tab-inactive
       ((,c
         ;; :background "#232635"
         ;; :box (:line-width 1 :color "#676E95")
         )))))
  :init
  (load-theme 'modus-vivendi-tinted t))

(set-face-attribute 'default nil :family "FiraCode Nerd Font" :height 130)

(setq-default mode-line-format
              '("%e"
                mode-line-front-space
                mode-line-modified
                " | "
                (:eval (propertize "%b" 'face 'font-lock-keyword-face))  ; Buffer name with face
                " | "
                (:eval (propertize (format-mode-line "%l:%c")  ; Line:Column
                                   'face 'font-lock-type-face))
                " | "
                (:eval (propertize (format-mode-line "%m")  ; Major mode
                                   'face 'font-lock-string-face)
                       )))

(provide 'pre-init)

;; Local variables:
;; byte-compile-warnings: (not obsolete free-vars)
;; End:

;;; pre-init.el ends here
