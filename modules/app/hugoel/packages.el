;;; app/hugoel/package.el -*- lexical-binding: t; -*-
;;; ;;
;;; Don't forget to use :files to include files in an unconventional project structure:
;

(package! hugo
 :recipe (:local-repo "~/repos/hugo.el"
          :files ("*.el")))
