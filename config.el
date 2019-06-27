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
(setq doom-font (font-spec :family "Fira Code" :size 14)
      doom-variable-pitch-font (font-spec :family "Noto Sans" :size 14))


;;
;;; Loads
(add-to-list 'load-path "~/.doom.d/packages/mu4e")

;;
;; Evil
(def-package-hook! evil
  :pre-init
  (setq evil-want-Y-yank-to-eol t)
  t)

(after! evil-escape
  (setq evil-escape-key-sequence "fd"))

;;
(after! mu4e
  (setq +mu4e-backend 'offlineimap))

;;
;; Completion
(def-package-hook! company-lsp
  :post-config
  (setq company-lsp-async t
	      company-lsp-filter-candidates nil
	      company-lsp-cache-candidates 'auto))


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
      :n "C-h" #'evil-window-left
      :n "C-j" #'evil-window-down
      :n "C-k" #'evil-window-up
      :n "C-l" #'evil-window-right

      :g "C-s" #'swiper

      (:map vterm-mode-map
        ;; Easier window movement
        :i "C-h" #'evil-window-left
        :i "C-j" #'evil-window-down
        :i "C-k" #'evil-window-up
        :i "C-l" #'evil-window-right)

      (:map evil-treemacs-state-map
        "C-h" #'evil-window-left
        "C-l" #'evil-window-right
        "M-j" #'multi-next-line
        "M-k" #'multi-previous-line)

      :leader "w" #'ace-window
      :leader "|" #'ubf|eshell-switch
      :leader "rr" #'copy-to-register
      :leader "rp" #'insert-register
      :leader "mu" #'mu4e

      )
