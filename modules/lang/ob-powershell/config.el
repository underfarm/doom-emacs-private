;;; lang/ob-powershell/config.el
;;;
;;;
(defvar ob-powershell-dir (concat doom-private-dir "/lisp/ob-powershell"))

(def-package! ob-powershell
  :load-path ob-powershell-dir
  :after (org))
