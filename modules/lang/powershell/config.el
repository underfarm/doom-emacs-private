;;; lang/powershell/config.el

(def-package! lsp-pwsh
  :defer t
  :hook (powershell-mode . (lambda () (require 'lsp-pwsh)
                             (lsp)
			                       (company-mode)
                             (flycheck-mode 0)
                             (setq-local company-idle-delay 0.2)
				                     (setq-local company-backends
						                             '(company-dabbrev
						                               company-files
                                           company-lsp
						                               company-keywords
						                               company-yasnippet
						                               company-capf)))))
