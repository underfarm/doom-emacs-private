
;;; ~/.doom.d/config.el -*- lexical-binding: t; -*-

(load! "+functions")
(load! "+org")

(setq user-full-name "Ulrik Bruun Farmen"
      user-mail-address "ulrik.bruun.farmen@gmail.net")

;;; Fonts
(setq doom-font (font-spec :family "Fira Code" :size 16)
      doom-variable-pitch-font (font-spec :family "Noto Sans" :size 16)
      doom-modeline-height 25)


;; OnSave()
(setq +format-on-save-enabled-modes
      '(not sql-mode                    ; sqlformat is currently broken
            tex-mode                    ; latexindent is broken
            latex-mode))

;; Company
(after! company
  (set-company-backend! 'emacs-lisp-mode 'company-files)
  (set-company-backend! 'gdscript-mode 'company-capf 'company-dabbrev))

;; Evil
(use-package-hook! evil
  :pre-init
  (setq evil-want-Y-yank-to-eol t)
  t)

(after! evil-escape
  (setq evil-escape-key-sequence "fd"))

(after! counsel
  (map!
   :g "C-s" #'swiper

   :leader
   (:prefix "c"
    "a" #'counsel-ag)))

(after! ace-window
  (map!
   :leader "w" #'ace-window))

(after! hugo
  (setq hugo-posts-directory "content/post"
        hugo-static-image-path "static/images"
        hugo-post-extension ".org"
        hugo-blog-root "~/repos/devlab-2.0")
  (defun hugo-image-to-static-and-insert ()
    (interactive)
    (let* ((img-folder (ufarmen-find-img-folder))
           (hugo-static-path (concat hugo-blog-root "/" hugo-static-image-path))
           (file (ivy-read "File:" (f-files img-folder))))
      (f-copy file (concat hugo-static-path "/" (f-filename file))))))

(after! code-library
  (setq code-library-directory (expand-file-name "CodeLibrary" "~/Dropbox/"))
  (add-to-list code-library-mode-file-alist '(gdscript-mode . "gdscript.org")))

(map! :map hugo-mode-map
      :m [return] #'hugo-open-at-point
      :localleader
      "c" #'hugo-create-thing
      "?" #'hugo-toggle-command-window
      "q" #'hugo-status-quit
      "s" #'hugo-start-stop-server
      "g" #'hugo-refresh-status
      "c" #'hugo-create-thing
      "b" #'build
      "v" #'hugo-show-server
      "!" #'hugo-show-process
      "n" #'hugo-move-to-next-thing
      "p" #'hugo-move-to-previous-thing
      "TAB" #'hugo-maybe-toggle-visibility)

(map! :m "M-j" #'multi-next-line
      :m "M-k" #'multi-previous-line

      (:map term-raw-map
       "M-k" #'term-send-up
       "M-j" #'term-send-down)

      :leader "|" #'ubf|eshell-switch

      :leader
      (:prefix "r"
       "r" #'copy-to-register
       "p" #'insert-register
       "b" #'revert-buffer)

      :leader
      (:prefix "c"
       "c" 'comment-dwim)

	    :leader "j1" #'(lambda () (interactive) (ubf|suround-word "'"))
	    :leader "j2" #'(lambda () (interactive) (ubf|suround-word "\""))
	    :leader 	 "j3" #'(lambda () (interactive) (ubf|suround-word "(" ")"))
	    :leader 	 "j4" #'(lambda () (interactive) (ubf|suround-word "[" "]")))




(custom-set-faces
 '(aw-leading-char-face
   ((t (:inherit ace-jump-face-foreground :height 3.0)))))
