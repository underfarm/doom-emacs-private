;;; lang/org/ob-powershell/package.el -*- lexical-binding: t; -*-
;;; ;;
;;; Don't forget to use :files to include files in an unconventional project structure:
;
;(package! ob-powershell
;  :recipe (:local-repo "~/Dropbox/ob-powershell"
;           :files ("*.el")))


(package! ob-powershell
 :recipe (:local-repo "~/repos/ob-pwsh/src"
          :files ("*.el")))
