;;; tex-config.el -*- no-byte-compile: t; lexical-binding: t; -*-

(defvar my/latex-colors
  '((blue         . "#82aaff")         ; Primary blue (commands, sectioning)
    (purple       . "#c792ea")       ; Primary purple (variables, italics)
    (green        . "#c3e88d")        ; Primary green (math, strings)
    (orange       . "#f78c6c")       ; Primary orange (warnings, math delimiters)
    (light-blue   . "#a1bfff")   ; Secondary blue
    (light-purple . "#bb80b3") ; Secondary purple
    (gray         . "#676E95")         ; Gray for subtle elements
    (dark-gray    . "#232635")    ; Dark gray for backgrounds
    (code-bg      . "#3C435E"))    ; Code block background
  "Color palette for LaTeX mode faces.")


;; (defvar my/latex-colors
;;   '((blue . "#82aaff")         ; Primary blue (commands, sectioning)
;;     (purple . "#c792ea")       ; Primary purple (variables, italics)
;;     (green . "#c3e88d")        ; Primary green (math, strings)
;;     (orange . "#f78c6c")       ; Primary orange (warnings, math delimiters)
;;     (light-blue . "#a1bfff")   ; Secondary blue
;;     (light-purple . "#bb80b3") ; Secondary purple
;;     (gray . "#676E95")         ; Gray for subtle elements
;;     (dark-gray . "#232635")    ; Dark gray for backgrounds
;;     (code-bg . "#3C435E"))    ; Code block background
;;   "Color palette for LaTeX mode faces.")


(defun my/latex-color (name)
  "Get theme-adaptive color value by NAME from `my/latex-colors`."
  (or (cdr (assoc name my/latex-colors))
      (progn
        (message "⚠️ my/latex-color: Unknown color key '%s'" name)
        "#ff00ff")))

;; (defun my/latex-color (name)
;;   "Get theme-adaptive color value by NAME from `my/latex-colors`."
;;   (cdr (assoc name my/latex-colors)))

;; (defmacro my/latex-color (name)
;;   "Get color value by NAME from my/latex-colors."
;;   `(cdr (assoc ',name my/latex-colors))
;;   )

(setq completion-ignored-extensions
      '(;; LaTeX auxiliary files
        ".aux" ".log" ".out" ".toc" ".lof" ".lot"
        ".bbl" ".blg" ".bcf" ".run.xml"
        ".synctex.gz" ".synctex(busy)" ".fls" ".fdb_latexmk"
        ".nav" ".snm" ".vrb" ".xdv" ".brf"
        ".pdfsync" ".dvi"

        ;; Asymptote files
        ".asy" ".pre"))

(defconst my/latex-prettify-alphabets
  '(("\\mathbb"   . ?𝔸)    ; A-Z
    ("\\mathcal"  . ?𝓐)    ; A-Z
    ("\\mathscr"  . ?𝒜)    ; A-Z
    ("\\mathfrak" . ?𝔄)    ; A-Z
    ("\\mathfrak" . ?𝔞))   ; a-z
  "Unicode starting points for LaTeX font styles.")

(defun my/latex-prettify-symbols-setup ()
  "Set up custom prettify symbols for LaTeX."
  (interactive)
  (setq prettify-symbols-unprettify-at-point t)
  ;; Generate mappings for each style
  (dolist (style my/latex-prettify-alphabets)
    (let ((base-char (cdr style))
          (prefix (car style)))
      (cl-loop for i from 0 to 25
               for c = (+ base-char i)
               for s = (char-to-string (+ (if (>= base-char ?𝔞) ?a ?A) i))
               do (push (cons (concat prefix s) c) prettify-symbols-alist))))
  ;; Base symbols
  (mapc (lambda (x) (push x prettify-symbols-alist))
        '(
          ("\\div" . ?÷)
          ("\\land" . ?∧)
          ("\\lor" . ?∨)
          ("\\bigsqcup" . ?⊔)
          ("\\bigodot" . ?⊙)
          ("\\bigotimes" . ?⊗)
          ("\\bigoplus" . ?⊕)
          ("\\biguplus" . ?⊎)
          ("^" . ?\u02C6)
          ("_" . ?\u02CD)
          ("\\circ" . ?∘)
          ("\\mathrm{i}" . ?𝔦)
          ("\\mathrm{Id}" . ?𝕀)
          ("\\leftsquigarrow" . ?⇜)
          ("\\rightsquigarrow" . ?⇝)))

  (prettify-symbols-mode))

(defun my/latex-font-faces-setup ()
  "Set up custom font faces for LaTeX using standardized color variables."
  (interactive)
  ;; Enhanced syntax highlighting
  (setq font-latex-fontify-script t)
  (setq font-latex-fontify-sectioning 'color)

  ;; Commands and Builtins
  (set-face-attribute 'font-latex-bold-face nil
                      :weight 'bold
                      :foreground (my/latex-color 'blue))

  ;; Italics (e.g., variables)
  (set-face-attribute 'font-latex-italic-face nil
                      :slant 'italic
                      :foreground (my/latex-color 'purple))

  ;; Math mode content
  (set-face-attribute 'font-latex-math-face nil
                      :slant 'italic
                      :weight 'normal
                      :foreground (my/latex-color 'green))

  ;; Brackets and structural
  (set-face-attribute 'font-latex-sedate-face nil
                      :foreground (my/latex-color 'gray))

  ;; Verbatim/code
  (set-face-attribute 'font-latex-verbatim-face nil
                      :background (my/latex-color 'code-bg)
                      :foreground (my/latex-color 'light-blue)
                      :family "Monospace")

  ;; Sectioning
  (set-face-attribute 'font-latex-sectioning-0-face nil
                      :foreground (my/latex-color 'blue)
                      :height 1.3
                      :weight 'bold)
  (set-face-attribute 'font-latex-sectioning-1-face nil
                      :foreground (my/latex-color 'blue)
                      :height 1.2
                      :weight 'bold)
  (set-face-attribute 'font-latex-sectioning-2-face nil
                      :foreground (my/latex-color 'light-purple)
                      :weight 'bold)
  (set-face-attribute 'font-latex-sectioning-3-face nil
                      :foreground (my/latex-color 'light-purple)
                      :slant 'italic)
  (set-face-attribute 'font-latex-sectioning-4-face nil
                      :foreground (my/latex-color 'light-blue)
                      :inherit 'font-latex-sectioning-3-face)
  (set-face-attribute 'font-latex-sectioning-5-face nil
                      :foreground (my/latex-color 'light-blue)
                      :inherit 'font-latex-sectioning-4-face)

  ;; Superscripts/Subscripts
  (set-face-attribute 'font-latex-script-char-face nil
                      :foreground (my/latex-color 'gray)
                      :height 0.9)
  (set-face-attribute 'font-latex-subscript-face nil
                      :foreground (my/latex-color 'gray)
                      :height 0.85)
  (set-face-attribute 'font-latex-superscript-face nil
                      :foreground (my/latex-color 'gray)
                      :height 0.85)

  ;; Warning/Error
  (set-face-attribute 'font-latex-warning-face nil
                      :foreground (my/latex-color 'orange)
                      :weight 'bold)

  ;; Strings
  (set-face-attribute 'font-latex-string-face nil
                      :foreground (my/latex-color 'green))

  ;; Underline
  (set-face-attribute 'font-latex-underline-face nil
                      :underline t
                      :foreground (my/latex-color 'orange))

  ;; DocTeX-specific
  (set-face-attribute 'font-latex-doctex-documentation-face nil
                      :background (my/latex-color 'dark-gray)
                      :foreground (my/latex-color 'gray))
  (set-face-attribute 'font-latex-doctex-preprocessor-face nil
                      :foreground (my/latex-color 'gray)
                      :slant 'italic)

  ;; Beamer slide titles
  (set-face-attribute 'font-latex-slide-title-face nil
                      :foreground (my/latex-color 'blue)
                      :height 1.1))

(make-face 'my/latex-math-paren-brack-face)
(set-face-attribute 'my/latex-math-paren-brack-face nil
                    :foreground (my/latex-color 'orange)
                    :weight 'bold)
(make-face 'my/latex-part-face)
(set-face-attribute 'my/latex-part-face nil
                    :foreground (my/latex-color 'blue)
                    :weight 'bold)
(make-face 'my/latex-environment-face)
(set-face-attribute 'my/latex-environment-face nil
                    :foreground (my/latex-color 'light-purple)
                    :weight 'bold)
(make-face 'my/latex-command-face)
(set-face-attribute 'my/latex-command-face nil
                    :foreground (my/latex-color 'blue)
                    :weight 'bold)
(make-face 'my/latex-argument-face)
(set-face-attribute 'my/latex-argument-face nil
                    :foreground (my/latex-color 'purple)
                    :slant 'italic)

;; (defface my/latex-math-paren-brack-face
;;   `((t (:foreground ,(my/latex-color orange) :weight bold)))
;;   "Face for inline and display math in LaTeX.")
;; 
;; (defface my/latex-part-face
;;   `((t (:foreground ,(my/latex-color blue) :weight bold)))
;;   "Face for \\part in LaTeX.")
;; 
;; (defface my/latex-environment-face
;;   '((t (:foreground (my/latex-color light-purple) :weight bold)))
;;   "Face for LaTeX environment names.")
;; 
;; (defface my/latex-command-face
;;   '((t (:foreground (my/latex-color blue) :weight bold)))
;;   "Face for LaTeX commands.")
;; 
;; (defface my/latex-argument-face
;;   '((t (:foreground (my/latex-color purple) :slant italic)))
;;   "Face for LaTeX command arguments.")


(defun my/latex-font-lock-setup ()
  "Highlight specific LaTeX math delimiters with custom faces."
  (font-lock-add-keywords nil
                          '(("\\\\("     0 'my/latex-math-paren-brack-face prepend)
                            ("\\\\)"     0 'my/latex-math-paren-brack-face prepend)
                            ("\\\\\\["   0 'my/latex-math-paren-brack-face prepend)
                            ("\\\\\\]"   0 'my/latex-math-paren-brack-face prepend)
                            ("^\\s-*\\\\part\\b" . 'my/latex-part-face))))

(defun my/latex-additional-font-lock ()
  "Additional font-lock rules for LaTeX."
  (font-lock-add-keywords
   nil
   `(
     ;; Environment names
     ("\\\\begin{\\([^}]+\\)}" 1 'my/latex-environment-face prepend)
     ("\\\\end{\\([^}]+\\)}" 1 'my/latex-environment-face prepend)

     ;; Commands
     ("\\\\[a-zA-Z@]+" 0 'my/latex-command-face prepend)

     ;; Optional arguments
     ("\\[\\([^]]+\\)\\]" 1 'my/latex-argument-face prepend)

     ;; Required arguments
     ("{\\([^}]+\\)}" 1 'my/latex-argument-face prepend)

     ;; Labels and references
     ("\\\\\\(label\\|ref\\|eqref\\|pageref\\|cite\\){\\([^}]+\\)}"
      2 'font-lock-constant-face prepend)

     ;; Comments
     ("%.*" 0 'font-lock-comment-face prepend)
     )))

(defun my/latex-indent-setup ()
  "Set up custom indentation rules for LaTeX environments."
  (interactive)
  (setq LaTeX-item-indent 0)
  (setq LaTeX-indent-environment-list
        `(;; Exam class environments
          ("questions")
          ("parts")
          ;; Verbatim-like environments (no indentation)
          ("verbatim"        current-indentation)
          ("verbatim*"       current-indentation)
          ("filecontents"    current-indentation)
          ("filecontents*"   current-indentation)
          ("macrocode"       current-indentation)
          ("macrocode*"      current-indentation)
          ;; Tabular / Matrix environments (default indentation)
          ("tabular") ("tabular*")
          ("array")
          ("matrix") ("matrix*")
          ("pmatrix") ("pmatrix*")
          ("bmatrix") ("bmatrix*")
          ("Bmatrix") ("Bmatrix*")
          ("vmatrix") ("vmatrix*")
          ("Vmatrix") ("Vmatrix*")
          ("smallmatrix") ("smallmatrix*")
          ;; Math alignment environments
          ("align") ("align*")
          ("aligned") ("alignedat")
          ("alignat") ("alignat*")
          ("xalignat") ("xalignat*")
          ("xxalignat")
          ("flalign") ("flalign*")
          ("gather") ("gather*")
          ("gathered")
          ("multline") ("multline*")
          ("split")
          ("cases") ("dcases") ("dcases*") ("rcases") ("rcases*")
          ;; Longtable variants
          ("longtable") ("longtable*")
          ("tabularx") ("tabulary")
          ("xltabular") ("xtabular") ("xtabular*")
          ;; Other environments
          ("equation") ("equation*")
          ("displaymath")
          ("picture")
          ("tabbing") )))

(use-package latex
  :ensure auctex
  :mode ("\\.tex\\'" . LaTeX-mode)
  :init
  (setq TeX-engine 'xetex
        TeX-PDF-mode t
        TeX-view-program-selection '((output-pdf "Zathura"))
        TeX-view-program-list '(("Zathura" "zathura --synctex-forward %n:0:%b %o")))
  :config
  (setq TeX-auto-save nil
        ;;TeX-auto-local (expand-file-name ".auctex-auto/" user-emacs-directory)
        TeX-parse-self nil
        TeX-save-query nil
        TeX-master nil
        TeX-show-compilation nil
        TeX-clean-confirm nil
        ;;TeX-output-dir "build"
        TeX-command-extra-options "--shell-escape"
        TeX-command-list
        '(("LaTeXMk"
           "latexmk %(latexmk-out) %(file-line-error) %(output-dir) %`%(extraopts) %S%(mode)%' %t"
           TeX-run-TeX nil
           (LaTeX-mode docTeX-mode)
           :help "Run LaTeXMk")
          ("Asymptote"
           "latexmk %(latexmk-out) %(file-line-error) %(output-dir) %`%(extraopts) %S%(mode)%' %t && asy %s-*.asy && latexmk -g %(latexmk-out) %(file-line-error) %(output-dir) %`%(extraopts) %S%(mode)%' %t"
           TeX-run-command nil t
           :help "Run LaTeXMk and Asymptote")
          ("Clean" "latexmk -c && rm -rf *.synctex.gz *.asy *.pre auto/ *.dvi"
           TeX-run-command nil t
           :help "Delete auxiliary files")
          ("View" "%V" TeX-run-discard-or-function t t :help "Run Viewer"))
        )

  (add-hook 'TeX-after-compilation-finished-functions #'TeX-revert-document-buffer)

  (add-hook 'LaTeX-mode-hook #'outline-minor-mode)
  (add-hook 'LaTeX-mode-hook #'centered-cursor-mode)

  (defun my/TeX-compile ()
    (interactive)
    (TeX-command "LaTeXMk" 'TeX-master-file))
  (defun my/TeX-asy-compile ()
    (interactive)
    (TeX-command "Asymptote" 'TeX-master-file))
  (defun my/TeX-clean ()
    (interactive)
    (TeX-command "Clean" 'TeX-master-file))
  :hook ((LaTeX-mode . my/latex-prettify-symbols-setup)
         (LaTeX-mode . my/latex-font-faces-setup)
         (LaTeX-mode . my/latex-indent-setup)
         (LaTeX-mode . my/latex-font-lock-setup)
         (LaTeX-mode . global-visual-line-mode)
         (LaTeX-mode . global-display-line-numbers-mode)

         (LaTeX-mode . (lambda ()
                         (setq TeX-command-default "LaTeXMk")
                         (setq TeX-source-correlate-mode t)
                         (setq TeX-source-correlate-start-server t))))
  
  :bind (:map LaTeX-mode-map
              ("C-c C-c"   . my/TeX-compile)
              ("C-c C-d"   . my/TeX-clean)
              ("C-c C-a"   . my/TeX-asy-compile)
              ("C-c C-v"   . TeX-view))
  ;; :config
  ;; (server-mode t)
  )

(use-package bicycle
  :ensure t
  :after outline
  :bind (:map outline-minor-mode-map
              ([C-tab] . bicycle-cycle)
              ([backtab] . bicycle-cycle-global)))

(use-package citar
  :ensure t
  :custom
  (citar-bibliography '("~/.repos/biblio/references.bib")))

(provide 'tex-config)

;;; post-init.el ends here
