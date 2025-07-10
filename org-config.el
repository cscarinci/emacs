;;; org-config.el -*- lexical-binding: t; -*-

(use-package org
  :mode ("\\.org\\'" . org-mode)
  :bind (("C-c l" . org-store-link)   
         ("C-c a" . org-agenda)
         ("C-c c" . org-capture)
         ("C-c b" . (lambda ()
                      (interactive)
                      (org-babel-tangle)
                      (my/org-tex-latexmk)))
         ("C-c v" . my/org-tex-view))
  :config
  (setq org-directory "~/Repos/org/"
        org-agenda-files (concat org-directory "agenda/")
        org-stored-links (concat org-directory ".links.org")
        org-default-notes-file (concat org-directory "notes.org")

        org-startup-folded t
        org-auto-align-tags nil
        org-tags-column 0
        org-catch-invisible-edits 'show-and-error
        org-special-ctrl-a/e t
        org-insert-heading-respect-content t

        org-src-fontify-natively t
        org-src-tab-acts-natively t
        org-edit-src-content-indentation 0
        org-support-shift-select t
        org-startup-indented t

        org-hide-emphasis-markers t
        org-pretty-entities t

        ;; TODOs
        org-todo-keywords '((sequence "TODO(t)" "ACTV(p)" "WAIT(w)" "|" "DONE(d)" "CANC(c)"))
        org-todo-keyword-faces
        '(("TODO" . (:foreground "#f78c6c" :weight bold))    ; orange
          ("ACTV" . (:foreground "#a1bfff" :weight bold))    ; purple 
          ("WAIT" . (:foreground "#bb80b3" :weight bold))    ; light-blue
          ("DONE" . (:foreground "#c3e88d" :weight bold))    ; green
          ("CANC" . (:foreground "#676E95" :weight bold)))   ; gray
        
        
        ;; AGENDA
        org-agenda-span 'week
        org-agenda-start-on-weekday 1 ; Monday
        org-agenda-window-setup 'current-window
        org-agenda-skip-deadline-if-done t
        org-agenda-skip-scheduled-if-done t
        org-agenda-todo-ignore-deadlines 'far
        org-agenda-todo-ignore-scheduled 'far
        org-agenda-start-with-log-mode t
        org-deadline-warning-days 7

        ;; CAPTURE
        org-capture-templates
        '(("t" "Todo" entry (file+headline "" "Tasks")
           "* TODO %?\n  %i\n  %a")
          ("j" "Journal" entry (file+datetree "~/org/journal.org")
           "* %?\nEntered on %U\n  %i\n  %a")
          ("n" "Note" entry (file "~/org/notes.org")
           "* %? :NOTE:\n%U\n%i\n%a")
          ("m" "Meeting" entry (file+headline "~/org/meetings.org" "Meetings")
           "* MEETING with %? :MEETING:\n%U"))
        )
  
  (defun my/latex-run-on-file (tex-file command-name)
    "Run LaTeX-mode COMMAND-NAME on TEX-FILE without leaving the current buffer."
    (interactive
     (let* ((default-tex "main.tex")
            (tex-path (read-file-name
                       "TeX file: "
                       default-directory
                       (expand-file-name default-tex default-directory)
                       t
                       default-tex
                       (lambda (f) (string-match-p "\\.tex\\'" f))))
            (cmd (completing-read "TeX command: "
                                  (mapcar #'car TeX-command-list)
                                  nil t)))
       (list tex-path cmd)))
    (if (not (file-exists-p tex-file))
        (message "File %s does not exist." tex-file)
      (let ((orig-buffer (current-buffer)))
        (with-current-buffer (find-file-noselect tex-file)
          (unless (derived-mode-p 'latex-mode)
            (LaTeX-mode))
          (setq TeX-master tex-file)
          (TeX-command command-name 'TeX-master-file))
        (message "Running %s on %s..." command-name tex-file)
        (switch-to-buffer orig-buffer))))

  (defun my/org-tex-latexmk ()
    "Run LaTeXMk on main.tex"
      (interactive)
    (my/latex-run-on-file "main.tex" "LaTeXMk"))
  
  (defun my/org-tex-view ()
    "Run View on main.tex"
      (interactive)
    (my/latex-run-on-file "main.tex" "View"))
  
  (defun my/org-tex-asy ()
    "Run Asymptote on main.tex"
      (interactive)
    (my/latex-run-on-file "main.tex" "Asymptote"))

  (defun my/org-tex-clean ()
    "Run Clean on main.tex"
      (interactive)
    (my/latex-run-on-file "main.tex" "Clean"))
  
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((emacs-lisp . t)
     (python . t)
     (shell . t)
     (latex . t)))
  )

(provide 'org-config)

;; Local variables:
;; byte-compile-warnings: (not obsolete free-vars)
;; End:

;;; post-init.el ends here
