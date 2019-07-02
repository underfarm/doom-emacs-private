;; ob-powershell.el --- description -*- lexical-binding: t; -*-
;;;
;;; Code:
(require 'ob)
(require 'ob-core)
(eval-when-compile (require 'cl))

(defvar org-babel-tangle-lang-exts)
(add-to-list 'org-babel-tangle-lang-exts '("powershell" . "ps1"))

(defvar org-babel-default-header-args:powershell '())
(defvar org-babel-powershell-buffers '(:default . nil))
(defvar org-babel-powershell-preface nil)

(defun org-babel-expand-body:powershell (body params)
  body)

(defun powershell-proc-buff-contains-prompt? (start-pos)
  "Check if buffer contain powershell prompt."
  (with-current-buffer "*terminal*"
    (goto-char start-pos)
    (search-forward-regexp "#!#" nil t)))

(defun recieve-output (timeout proc-buffer-name current-point-pos)
  "When the prompt is returned from the command, we return the output of that command."
  (while (eq (powershell-proc-buff-contains-prompt? current-point-pos) nil)
    (sleep-for timeout))
  (with-current-buffer proc-buffer-name
    (let ((buffpoint current-point-pos))
      (buffer-substring-no-properties buffpoint (point-max)))))

(defun org-babel-execute:powershell (body params)
  "Execute a block of Powershell code with Babel.
This function is called by `org-babel-execute-src-block'."
  (let* (	 (proc-buffer "*terminal*")
             (proc (org-babel-powershell-initiate-session))
	           (src-body (org-babel-expand-body:powershell body nil))
	           (timeout (or (cdr (assq :timeout params))
		                      1)))
    ;;
    ;; We need to keep the current point from term - we dont want the previous output.
    (setq current-term-point (with-current-buffer "*terminal*" (point)))

    ;; We create our own prompt with #!#.
	  (send-string proc src-body)
	  (send-string proc "#!# \n")
    (recieve-output timeout proc-buffer current-term-point)))

(defun org-babel-powershell-initiate-session (&optional session params)
  "If powershell process is not running - then start process. Returns the powershell process."
  (unless (get-buffer-process "*terminal*")
    (term "~/bin/pwsh"))
  (get-buffer-process "*terminal*"))

(provide 'ob-powershell)
;;; ob-powershell.el ends here
