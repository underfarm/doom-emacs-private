;;; init.el -*- lexical-binding: t; -*-

;; This file controls what Doom modules are enabled and what order they load in.
;; Remember to run 'doom sync' after modifying it!

;; NOTE Press 'SPC h d h' (or 'C-h d h' for non-vim users) to access Doom's
;;      documentation. There you'll find information about all of Doom's modules
;;      and what flags they support.

;; NOTE Move your cursor over a module's name (or its flags) and press 'K' (or
;;      'C-c g k' for non-vim users) to view its documentation. This works on
;;      flags as well (those symbols that start with a plus).
;;
;;      Alternatively, press 'gd' (or 'C-c g d') on a module to browse its
;;      directory (for easy access to its source code).

(doom! :input

       :completion
       (company +childframe)            ; the ultimate code completion backend
       (ivy +childframe +icons)

       :ui
       doom                      ; what makes DOOM look the way it does
       doom-dashboard            ; a nifty splash screen for Emacs
       doom-quit                 ; DOOM quit-message prompts when you quit Emacs
       hl-todo                   ; highlight TODO/FIXME/NOTE tags
       modeline ; snazzy, Atom-inspired modeline, plus API nav-flash         ; blink the current line after jumping
       nav-flash
       unicode
       treemacs ; a project drawer, like neotree but cooler ;;unicode           ; extended unicode support for various languages
       ophints  ; highlight the region an operation acts on
       (popup   ; tame sudden yet inevitable temporary windows
        +all    ; catch all popups that start with an asterix
        +defaults)                   ; default popup rules
       pretty-code                   ; replace bits of code with pretty symbols
       treemacs                      ; a project drawer, like neotree but cooler
       hydra
       vc-gutter              ; vcs diff in the fringe
       vi-tilde-fringe        ; fringe tildes to mark beyond EOB
       window-select          ; visually switch windows
       workspaces             ; tab emulation, persistence & separate workspaces

       :editor
       (evil +everywhere)        ; come to the dark side, we have cookies
       file-templates            ; auto-snippets for empty files
       fold                      ; (nigh) universal code folding
       (format +onsave)          ; automated prettiness
       multiple-cursors          ; editing in many places at once
       rotate-text               ; cycle region at point between text candidates
       snippets                  ; my elves. They type so I don't have to
       lispy

       :emacs
       @functions
       (dired                           ; making dired pretty [functional]
        +icons                          ; colorful icons for dired-mode
        )
       undo
       electric                   ; smarter, keyword-based electric-indent
       vc                         ; version-control and Emacs, sitting in a tree

       :term
       eshell
       shell
       term


       :checkers
       syntax                        ; tasing you for every semicolon you forget
       (spell +everywhere)           ; tasing you for misspelling mispelling
       grammar                       ; tasing grammar mistake every you make


       :tools
       (eval +overlay)                  ; run code, run (also, repls)
       gist                             ; interacting with github gists
       (lookup)
       lsp
       magit                            ; a git porcelain for Emacs

       :lang
       ;; godot - made by me. There seems to be a proper module for gdscript now, with LSP support.
       plantuml
       gdscript
       ess
       powershell
       ob-powershell
       data             ; config/data formats
       elixir           ; erlang done right
       emacs-lisp       ; drown in parentheses
       ;; markdown          ; writing docs for people to ignore
       nix                            ; I hereby declare "nix geht mehr!"
       (javascript +lsp)              ; all(hope(abandon(ye(who(enter(here))))))
       (org                           ; organize your plain life in plain text
        +dragndrop
        +gnuplot
        +journal
        +dragndrop                      ; file drag & drop support
        +pandoc)                        ; pandoc integration into org's exporter
       sh
       web
       yaml
       rest

       :email
                                        ;(mu4e +gmail)       ; WIP
                                        ;notmuch             ; WIP
                                        ;(wanderlust +gmail) ; WIP

       :app
       hugoel
       ;;calendar
       ;;irc              ; how neckbeards socialize
       ;;(rss +org)        ; emacs as an RSS reader
       ;;twitter           ; twitter client https://twitter.com/vnought

       :collab
       ;;floobits          ; peer programming for a price
       ;;impatient-mode    ; show off code over HTTP

       :config
       (default +bindings +smartparens))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(aw-leading-char-face ((t (:inherit ace-jump-face-foreground :height 3.0)))))
