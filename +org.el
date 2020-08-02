;;; ~/.doom.d/+org.el -*- lexical-binding: t; -*-


;;; :lang org
(after! org
  (setq org-directory (expand-file-name "Org" "~/Dropbox/")
        org-bullets-bullet-list '("› ")
        org-ellipsis "  "
        org-pretty-entities t
        org-hide-emphasis-markers t
        org-fontify-whole-heading-line t
        org-fontify-done-headline t
        org-fontify-quote-and-verse-blocks t
        org-superstar-headline-bullets-list '("#"))

  (setq +org-babel-mode-alist
        '((cpp . C)
          (C++ . C)
          (D . C)
          (R . R)
          (sh . shell)
          (ps . pwsh) ;; this one is home brewed.
          (bash . shell)))

  (setq org-log-done 'time)
  (setq org-journal-date-format "%A, %d %B %Y"))

(add-hook! org-mode
  (visual-line-mode))
