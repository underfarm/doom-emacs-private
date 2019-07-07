;;; ~/.doom.d/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here

(setq user-full-name "Ulrik Bruun Farmen"
      user-mail-address "ulrik.bruun.farmen@gmail.net"

      ;; lsp-ui-sideline is redundant with eldoc and much more invasive, so
      ;; disable it by default.
      lsp-ui-sideline-enable nil)

;;
;;; UI

;;; Fonts
(setq doom-font (font-spec :family "Fira Code" :size 16)
      doom-variable-pitch-font (font-spec :family "Noto Sans" :size 16))

;; How tall the mode-line should be. It's only respected in GUI.
;; If the actual char height is larger, it respects the actual height.
(setq doom-modeline-height 25)

;;
;;; Loads
(add-to-list 'load-path (expand-file-name "lisp" doom-private-dir))
(add-to-list 'load-path (expand-file-name "lisp/mu4e" doom-private-dir))

;;
;; Evil
(def-package-hook! evil
  :pre-init
  (setq evil-want-Y-yank-to-eol t)
  t)

(after! evil-escape
  (setq evil-escape-key-sequence "fd"))

;;
;;; Mail
(after! mu4e
  (setq +mu4e-backend 'offlineimap)
  (setq mu4e-maildir "~/Maildir"
        mu4e-drafts-folder "/Gmail/[Gmail].Drafts"
        mu4e-sent-folder "/Gmail/[Gmail].Sent Mail"
        mu4e-trash-folder "/Gmail/[Gmail].Trash"
        mu4e-refile-folder "/[Gmail].All")

  ;; That sweet, sweet spell checking.
  (add-hook 'mu4e-compose-mode-hook 'flyspell-mode)

(require 'smtpmail)

(setq message-send-mail-function 'smtpmail-send-it
      starttls-use-gnutls t
      smtpmail-starttls-credentials
      '(("smtp.gmail.com" 587 nil nil))
      smtpmail-default-smtp-server "smtp.gmail.com"
      smtpmail-smtp-server "smtp.gmail.com"
      smtpmail-smtp-service 587
      smtpmail-debug-info t))

;; Completion
(def-package-hook! company-lsp
  :post-config
  (setq company-lsp-async t
	      company-lsp-filter-candidates nil
	      company-lsp-cache-candidates 'auto))

(after! lsp-pwsh

  (setq pwsh-output (generate-new-buffer "pwsh-output"))
  ; this only works when 'output' is added to lsp-pwsh client notification handler.
  (defun ubf-send-lsp-region (start end)
    (interactive "r")
    (if (use-region-p)
        (let ((regionp (buffer-substring start end)))
          (lsp-send-request-async
           (lsp-make-request "evaluate"
                             (list :expression regionp))
           '(lambda (lspresulthash) "resutlhash")))))

  (defun ubf-lsp-pwsh-output (output)
    (with-current-buffer "pwsh-output"
      (insert output)))

)




(after! org
  (setq +org-babel-mode-alist
        '((cpp . C)
          (C++ . C)
          (D . C)
          (sh . shell)
          (ps . powershell) ;; this one is home brewed.
          (bash . shell)
          (matlab . octave))))

;;
;;; Shell
;; (after! eshell
;; (setq eshell-prompt-function
;;       (lambda ()
;;   	(concat
;;   	 (propertize "┌─[" 'face					`(:foreground "#ffaf00"))
;;   	 (propertize (user-login-name) 'face				`(:foreground "#870000"))
;;   	 (propertize "@" 'face						`(:foreground "#ffaf00"))
;;   	 (propertize (system-name) 'face				`(:foreground "#870000"))
;;   	 (propertize "]──[" 'face					`(:foreground "#ffaf00"))
;;   	 (propertize (format-time-string "%H:%M" (current-time)) 'face	`(:foreground "yellow"))
;;   	 (propertize "]──[" 'face					`(:foreground "#ffaf00"))
;;   	 (propertize (concat (eshell/pwd)) 'face			`(:foreground "#a8a8a8"))
;;   	 (propertize "]\n" 'face					`(:foreground "#ffaf00"))
;;   	 (propertize "└─>" 'face					`(:foreground "#ffaf00"))
;;   	 (propertize (if (= (user-uid) 0) " # " " $ ") 'face		`(:foreground "#ffaf00"))
;;   	 ))))

(map! :m "M-j" #'multi-next-line
      :m "M-k" #'multi-previous-line

      ;; Easier window movement
      ;; :n "C-h" #'evil-window-left
      ;; :n "C-j" #'evil-window-down
      ;; :n "C-k" #'evil-window-up
      ;; :n "C-l" #'evil-window-right

      :g "C-s" #'swiper

      ;; (:map vterm-mode-map
      ;;   ;; Easier window movement
      ;;   :i "C-h" #'evil-window-left
      ;;   :i "C-j" #'evil-window-down
      ;;   :i "C-k" #'evil-window-up
      ;;   :i "C-l" #'evil-window-right)

      (:map term-raw-map
          "M-k" #'term-send-up
          "M-j" #'term-send-down)

      (:map evil-treemacs-state-map
        "C-h" #'evil-window-left
        "C-l" #'evil-window-right
        "M-j" #'multi-next-line
        "M-k" #'multi-previous-line)

      :leader "w" #'ace-window
      :leader "|" #'ubf|eshell-switch

      :leader
      (:prefix "c"
        "a" #'counsel-ag)

      :leader
      (:prefix "r"
              "r" #'copy-to-register
              "p" #'insert-register
              "b" #'revert-buffer)

      :leader
      (:prefix "m"
              "u" #'mu4e)

      (:prefix "f"
        "t" #'find-in-dotfiles
        "T" #'browse-dotfiles)

		  :leader "j1" #'(lambda () (interactive) (ubf|suround-word "'"))
		  :leader "j2" #'(lambda () (interactive) (ubf|suround-word "\""))
	    :leader 	 "j3" #'(lambda () (interactive) (ubf|suround-word "(" ")"))
	    :leader 	 "j4" #'(lambda () (interactive) (ubf|suround-word "[" "]"))
      )
