;;; ubf-mail/init.el -*- lexical-binding: t; -*-

(when (eq system-type 'gnu/linux)

  (def-package! mu4e
    :load-path "/opt/mu-1.2.0/mu4e"
    :config

    (use-package evil-mu4e
      :after mu4e)

    (use-package org-mime
      :after mu4e)

    (setq mu4e-mu-binary "/usr/local/bin/mu"
          setq mu4e-maildir "/home/ufarmen/snap/offlineimap/current/Maildir"
          mu4e-sent-folder "/home/ufarmen/snap/offlineimap/current/Maildir/sent"
          mu4e-attachment-dir "/home/ufarmen/snap/offlineimap/current/.attachments")


    (setq mu4e-compose-format-flowed t ; visual-line-mode + auto-fill upon sending
        mu4e-view-show-addresses t
        mu4e-sent-messages-behavior 'sent
        mu4e-hide-index-messages t
        ;; try to show images
        mu4e-view-show-images t
        mu4e-view-image-max-width 800
        ;; configuration for sending mai
        message-send-mail-function #'smtpmail-send-it
        smtpmail-stream-type 'starttls
        message-kill-buffer-on-exit t ; close after sending
        ;; start with the first (default) context;
        mu4e-context-policy 'pick-first
        ;; compose with the current context, or ask
        mu4e-compose-context-policy 'ask-if-none
        ;; use helm/ivy
        mu4e-completing-read-function #'ivy-completing-read)
        ;; no need to ask
        mu4e-confirm-quit nil
        ;; remove 'lists' column
        mu4e-headers-fields
        '((:account . 12)
          (:human-date . 12)
          (:flags . 4)
          (:from . 25)
          (:subject)))


  ;; Use fancy icons
  (setq mu4e-headers-has-child-prefix '("+" . "")
        mu4e-headers-empty-parent-prefix '("-" . "")
        mu4e-headers-first-child-prefix '("\\" . "")
        mu4e-headers-duplicate-prefix '("=" . "")
        mu4e-headers-default-prefix '("|" . "")
        mu4e-headers-draft-mark '("D" . "")
        mu4e-headers-flagged-mark '("F" . "")
        mu4e-headers-new-mark '("N" . "")
        mu4e-headers-passed-mark '("P" . "")
        mu4e-headers-replied-mark '("R" . "")
        mu4e-headers-seen-mark '("S" . "")
        mu4e-headers-trashed-mark '("T" . "")
        mu4e-headers-attach-mark '("a" . "")
        mu4e-headers-encrypted-mark '("x" . "")
        mu4e-headers-signed-mark '("s" . "")
        mu4e-headers-unread-mark '("u" . ""))

  ;; Refresh the current view after marks are executed
  (defun +mu4e*refresh (&rest _) (mu4e-headers-rerun-search))
  (advice-add #'mu4e-mark-execute-all :after #'+mu4e*refresh)

  (when (fboundp 'imagemagick-register-types)
    (imagemagick-register-types))

  (set-evil-initial-state!
    '(mu4e-main-mode
      mu4e-view-mode
      mu4e-headers-mode
      mu4e-compose-mode
      mu4e~update-mail-mode)
    'normal))

  (when (featurep! :tools flyspell)
    (add-hook 'mu4e-compose-mode-hook #'flyspell-mode))

    (setq mu4e-contexts
	        `( ,(make-mu4e-context
	             :name "Gmail"
	             :match-func (lambda (msg) (when msg
				                                   (string-prefix-p "/Gmail" (mu4e-message-field msg :maildir))))
	             :vars '(
		                   (mu4e-trash-folder . "/Gmail/[Gmail].Trash")
		                   (mu4e-refile-folder . "/Gmail/[Gmail].Archive")
		                   ))
	           ))

    ;; I have my "default" parameters from Gmail

	  mu4e-drafts-folder "/[Gmail].Drafts"
	  mu4e-sent-folder   "/[Gmail].Sent Mail"
	  mu4e-sent-messages-behavior 'delete

	  ;; mu4e-sent-messages-behavior 'delete ;; Unsure how this should be configured
	  user-mail-address "ulrik.bruun.farmen@gmail.com"
	  smtpmail-starttls-credentials '(("smtp.gmail.com" 587 nil nil))
	  smtpmail-auth-credentials
	  '(("smtp.gmail.com" 587 "ulrik.bruun.farmen@gmail.com" nil))
	  smtpmail-default-smtp-server "smtp.gmail.com"
	  smtpmail-smtp-server "smtp.gmail.com"
	  smtpmail-smtp-service 587

    (require 'smtpmail)

    (setq message-send-mail-function 'smtpmail-send-it
	        starttls-use-gnutls t
	        smtpmail-starttls-credentials
	        '(("smtp.gmail.com" 587 nil nil))
	        smtpmail-auth-credentials
	        (expand-file-name "~/.authinfo.gpg")
	        smtpmail-default-smtp-server "smtp.gmail.com"
	        smtpmail-smtp-server "smtp.gmail.com"
	        smtpmail-smtp-service 587
	        smtpmail-debug-info t)

    )
  )
