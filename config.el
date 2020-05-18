
;;; ~/.doom.d/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here
;;
(toggle-frame-maximized)

(setq user-full-name "Ulrik Bruun Farmen"
      user-mail-address "ulrik.bruun.farmen@gmail.net"

      doom-scratch-initial-major-mode 'lisp-interaction-mode
      )

;;; Fonts
(setq doom-font (font-spec :family "Fira Code" :size 16)
      doom-variable-pitch-font (font-spec :family "Noto Sans" :size 16)
      doom-modeline-height 25)

;;; :lang org
(setq org-journal-date-format "%A, %d %B %Y")

(if (string-equal system-name "ulrikf-KPL-W0X")
    (setq org-journal-dir "~/Dropbox/Org/Journal/")
  (setq org-journal-dir "/mnt/c/Users/ulrik/Dropbox/Org/Journal/"))


(setq +format-on-save-enabled-modes
      '(not sql-mode     ; sqlformat is currently broken
            tex-mode         ; latexindent is broken
            latex-mode))

(after! company
  (set-company-backend!
    'emacs-lisp-mode
    'company-files))

;; Evil
(use-package-hook! evil
  :pre-init
  (setq evil-want-Y-yank-to-eol t)
  t)

(after! evil-escape
  (setq evil-escape-key-sequence "fd"))

;; Completion
(use-package-hook! company-lsp
  :post-config
  (setq company-lsp-async t
	      company-lsp-filter-candidates nil
	      company-lsp-cache-candidates 'auto))

;; (after! lsp-pwsh
;;   (setq pwsh-output (generate-new-buffer "pwsh-output"))
;;                                         ; this only works when 'output' is added to lsp-pwsh client notification handler.
;;                                         ;:notification-handlers (lsp-ht ("powerShell/executionStatusChanged" 'ignore)
;;   ;;                              ("output" 'ubf-lsp-pwsh-output))
;;   (defun ubf-send-lsp-region (start end)
;;     (interactive "r")
;;     (if (use-region-p)
;;         (let ((regionp (buffer-substring start end)))
;;           (lsp-send-request-async
;;            (lsp-make-request "evaluate"
;;                              (list :expression regionp))
;;            '(lambda (lspresulthash) "resutlhash")))))

;;   (defun ubf-lsp-pwsh-output (_workspace params)
;;     (setq out (gethash "output" params))
;;     (with-current-buffer "pwsh-output"
;;       (insert out)))

;;   (defun ubf-send-lsp-line ()
;;     (interactive)
;;     (let* ((begin (line-beginning-position))
;;            (end (line-end-position))
;;            (buffstring (with-current-buffer (current-buffer)
;;                          (buffer-substring-no-properties begin end))))
;;       (lsp-send-request-async
;;        (lsp-make-request "evaluate"
;;                          (list :expression buffstring))
;;        '(lambda (nilresult)
;;           (message "Executed eval")))))

;;   (defun ubf-send-lsp-buffer ()
;;     (interactive)
;;     (let* ((begin (point-min))
;;            (end (point-max))
;;            (buffstring (with-current-buffer (current-buffer)
;;                          (buffer-substring-no-properties begin end))))
;;       (lsp-send-request-async
;;        (lsp-make-request "evaluate"
;;                          (list :expression buffstring))
;;        '(lambda (nilresult)
;;           (message "Executed eval")))))
;;   )

(after! org
  (setq +org-babel-mode-alist
        '((cpp . C)
          (C++ . C)
          (D . C)
          (R . R)
          (sh . shell)
          (ps . pwsh) ;; this one is home brewed.
          (bash . shell))))

(add-hook! org-mode
  (visual-line-mode))

(after! org-download
  (setq org-download-screenshot-method "flameshot gui -p /tmp/img/")

  (defun aj-fetch-latest (path)
    (let ((e (f-entries path)))
      (car (sort e (lambda (a b)
                     (not (time-less-p (aj-mtime a)
                                       (aj-mtime b))))))))

  (defun aj-mtime (f) (let ((attrs (file-attributes f))) (nth 5 attrs)))

  (defun ubf|org-download-screenshot (old-function &rest arguments)
    (interactive)
    (let ((default-directory "~"))
      (make-directory "/tmp/img/" t)
      (call-process "flameshot" nil t nil "gui" "-p" "/tmp/img")
      (sleep-for 0.5)
      (org-download-image (aj-fetch-latest "/tmp/img"))))

  (advice-add #'org-download-screenshot :around #'ubf|org-download-screenshot)

  (defun ubf|org-download-screenshot-2 ()
    (interactive)
    (let ((default-directory "~"))
      (make-directory "/tmp/img/" t)
      (call-process "flameshot" nil t nil "gui" "-p" "/tmp/img")
      (sleep-for 0.5)
      (setq ubf|screenshot-image (aj-fetch-latest "/tmp/img"))
      (setq ubf|screenshot-image-description (read-string "Description:"))
      (f-move ubf|screenshot-image (format "~/repos/devlab/static/images/%s.png" ubf|screenshot-image-description))
      (insert (format "[[/images/%s.png]]" ubf|screenshot-image-description ubf|screenshot-image-description))
      ))

  )



(after! counsel
  (map!
   :g "C-s" #'swiper

   :leader
   (:prefix "c"
    "a" #'counsel-ag)))

(after! ace-window
  (map!
   :leader "w" #'ace-window)

  (custom-set-faces
   '(aw-leading-char-face
     ((t (:inherit ace-jump-face-foreground :height 3.0))))))


(after! hugo
  (setq hugo-posts-directory "content/post"
        hugo-static-image-path "static/images"
        hugo-post-extension ".org"
        hugo-blog-root "~/repos/devlab-2.0")

  (defun ufarmen-find-img-folder ()
    (if (string-equal system-name "ulrikf-KPL-W0X")
        (setq org-journal-dir (concat "~/Dropbox/ShareX/Screenshots/" (format-time-string "%Y-%m")))
      (setq org-journal-dir (concat "/mnt/c/Users/ulrik/Dropbox/ShareX/Screenshots/" (format-time-string "%Y-%m")))))

  (defun hugo-image-to-static-and-insert ()
    (interactive)
    (let* ((img-folder (ufarmen-find-img-folder))
           (hugo-static-path (concat hugo-blog-root "/" hugo-static-image-path))
           (file (ivy-read "File:" (f-files img-folder))))
      (f-copy file (concat hugo-static-path "/" (f-filename file))))))

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

	    :leader "j1" #'(lambda () (interactive) (ubf|suround-word "'"))
	    :leader "j2" #'(lambda () (interactive) (ubf|suround-word "\""))
	    :leader 	 "j3" #'(lambda () (interactive) (ubf|suround-word "(" ")"))
	    :leader 	 "j4" #'(lambda () (interactive) (ubf|suround-word "[" "]")))
