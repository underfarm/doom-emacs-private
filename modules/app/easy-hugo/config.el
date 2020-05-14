;;; app/easy-hugo/config.el -*- lexical-binding: t; -*-
;;;
;;;
;;;
(use-package! easy-hugo
  :config
  (setq
   easy-hugo-basedir "~/repos/devlab-2.0/"
   easy-hugo-url "https://devlabstorage1991.z16.web.core.windows.net"
   easy-hugo-root "/home/repos/devlab-2.0"
   easy-hugo-previewtime "300"

   easy-hugo-default-ext ".org"

   easy-hugo-default-picture-directory (concat "~/Pictures/" (format-time-string "%Y-%m")))

  :bind ("C-c C-e" . easy-hugo))
