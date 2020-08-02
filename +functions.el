;;; ~/.doom.d/+functions.el -*- lexical-binding: t; -*-

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
      )))


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
