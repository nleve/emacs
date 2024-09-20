;;; init.el --- emacs config
;;; Commentary:
;
; This is my init.el.  There are many like it, but this one is mine.
;

(setq custom-file "~/.config/emacs/custom.el")

; Make it minimal-looking.
(menu-bar-mode -1)
(toggle-scroll-bar -1)
(tool-bar-mode -1)

; Smooth scroll
(pixel-scroll-mode 1)
(pixel-scroll-precision-mode 1)

; Performance tweaks
(setq gc-cons-threshold 100000000)
(setq read-process-output-max (* 1024 1024)) ;; 1mb
(setq max-lisp-eval-depth 10000)

;; UTF-8 files by default
(prefer-coding-system 'utf-8)

;; Font
(set-frame-font "Hack-12" nil t)

;; Global keybinds
(global-set-key (kbd "M-l") 'evil-window-right)
(global-set-key (kbd "M-h") 'evil-window-left)
(global-set-key (kbd "M-j") 'evil-window-down)
(global-set-key (kbd "M-k") 'evil-window-up)

;; Package setup
(require 'package)
(setq package-enable-at-startup nil)
(setq package-archives '(("org"   . "https://orgmode.org/elpa/")
                         ("gnu"   . "https://elpa.gnu.org/packages/")
                         ("melpa" . "https://melpa.org/packages/")))
(package-initialize)

;; Configure use-package
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(eval-when-compile (require 'use-package))
(setq use-package-always-ensure t)
(use-package diminish)

;; Theme & Appearance
(use-package doom-themes
  :init
  ;(load-theme 'doom-dark+ t))
  (load-theme 'doom-dracula t))

(use-package all-the-icons :defer)

;; Quality-of-life improvements
(use-package smooth-scrolling
  :custom
  (smooth-scrolling-mode 1)
  (smooth-scroll-margin 4))

;; Env vars
(use-package exec-path-from-shell
  :ensure t
  :init
  (exec-path-from-shell-copy-env "PATH")
  )

;; Fix indentation
(use-package dtrt-indent :ensure)

;; Evil
(use-package evil
  :custom
  (evil-undo-system 'undo-redo)
  (evil-emacs-state-modes nil)
  (evil-insert-state-modes nil)
  (evil-motion-state-modes nil)
  (evil-search-module 'evil-search)
  (evil-ex-complete-emacs-commands nil)
  (evil-ex-complete-emacs-commands nil)
  (evil-vsplit-window-right t)
  (evil-split-window-below t)
  (evil-shift-round nil)
  (evil-want-C-u-scroll t)
  (evil-want-keybinding nil)
  :config
  (evil-mode 1)
  ;; Make movement keys work like they should
  (define-key evil-normal-state-map (kbd "j") 'evil-next-visual-line)
  (define-key evil-normal-state-map (kbd "k") 'evil-previous-visual-line)
  (define-key evil-normal-state-map (kbd "g ]") 'xref-go-forward)
  (define-key evil-normal-state-map (kbd "g [") 'xref-go-back)
  ;; Add some quick fwd / bad keys
  (enable-smooth-scroll-for-function evil-next-visual-line)
  (enable-smooth-scroll-for-function evil-previous-visual-line)
  ;; Make horizontal movement cross wrapped lines
  (setq-default evil-cross-lines t)
  )

(use-package evil-collection
  :after evil
  :diminish (evil-collection-unimpaired-mode)
  :init
  (evil-collection-init))

;; Git
(use-package magit :defer)

(use-package git-gutter
  :defer
  :diminish
  :init
  (global-git-gutter-mode +1)
  :custom
  (git-gutter:ask-p nil))

;; Completion
(use-package company
  :defer
  :diminish
  :init
  (global-company-mode)
  :custom
  (company-idle-delay 0.1)
  (company-tooltip-align-annotations 't)
  (company-minimum-prefix-length 1))

(use-package ivy
  :defer
  :diminish
  :init
  (ivy-mode 1)
  :custom
  (ivy-initial-inputs-alist nil)
  :config
  ;(define-key ivy-minibuffer-map (kbd "C-k") #'ivy-previous-line)
  ;(define-key ivy-minibuffer-map (kbd "C-j") #'ivy-next-line)
  ;(define-key ivy-switch-buffer-map (kbd "C-k") #'ivy-previous-line)
  ;(define-key ivy-switch-buffer-map (kbd "C-j") #'ivy-next-line)
)

(use-package counsel :diminish :defer)

;; Which Key
(use-package which-key
  :diminish
  :init
  (which-key-mode 1)
  :custom
  (which-key-idle-delay 0.1)
  (which-key-separator ":")
  (which-key-prefix-prefix "+"))

;; Language-specific Modes
(use-package graphviz-dot-mode :defer)

(use-package zig-mode
  :defer
  :mode "\\.zig\\'"
  :hook (lsp-deferred)
  :ensure
)

(use-package elixir-ts-mode
  :ensure)

(use-package treesit-auto
  :custom
  (treesit-auto-install 'prompt)
  (treesit-auto-add-to-auto-mode-alist 'all)
  :config
  (treesit-auto-add-to-auto-mode-alist)
  (delete 'janet treesit-auto-langs) ;; fails to compile for some reason - yeet
  (global-treesit-auto-mode))

;; LSP
(use-package eglot
  :hook (prog-mode . eglot-ensure)
  :init
  (setq eglot-stay-out-of '(flymake))
  :bind (:map
	 eglot-mode-map
	 ("C-c c a" . eglot-code-actions)
	 ("C-c c o" . eglot-code-actions-organize-imports)
	 ("C-c c r" . eglot-rename)
	 ("C-c c f" . eglot-format)))

;; LSP and DAP
;; (use-package lsp-mode
;;   :ensure
;;   :custom
;;   (lsp-clients-clangd-args '("-j=8" "--malloc-trim" "--background-index" "--pch-storage=memory"))
;;   (lsp-idle-delay 0.5)
;;   (lsp-keep-workspace-alive nil)
;;   (lsp-warn-no-matched-clients nil)
;;   (lsp-headerline-breadcrumb-enable-diagnostics nil))
;; (use-package lsp-ui :ensure :defer)
;; (use-package lsp-ivy :ensure :defer)
;; (use-package treemacs :ensure :defer)
;; (use-package lsp-treemacs :ensure :defer)
;; (use-package dap-mode :ensure :defer)

;; (defun add-lsp-hook-to-ts-modes ()
;;   "Hook into each treesitter mode and also start lsp-mode."
;;   (dolist (mode global-treesit-auto-modes)
;;     (add-hook (intern (concat (symbol-name mode) "-hook"))
;;               #'lsp-deferred)))
;; (add-lsp-hook-to-ts-modes)

(use-package flycheck
  :ensure
  :hook (prog-mode . flycheck-mode)
  :bind (:map flycheck-mode-map
	      ("C-c ! n" . flycheck-goto-next-error)
	      ("C-c ! p" . flycheck-goto-prev-error)
	      ("C-c ! l" . flycheck-show-buffer-diagnostics)))

(use-package eldoc-box :defer)

;; General keymap
(use-package general
  :after evil
  :config 
  (general-evil-setup t)
  (setq general-override-states '(insert
                                  emacs
                                  hybrid
                                  normal
                                  visual
                                  motion
                                  operator
                                  replace))
  (general-override-mode)
  
  (general-define-key
   :states '(normal visual motion emacs)
   :keymaps 'override
   "C-n" 'scroll-up-line
   "C-p" 'scroll-down-line
   )

  (general-define-key
   :states '(normal visual motion emacs)
   :keymaps 'override
   :prefix "SPC"
   :non-normal-prefix "C-SPC"
   "" nil
   ;; "/" '(counsel-rg :wk "riggrep") ; need counsel
   "SPC" '(execute-extended-command :wk "M-x")
   ;; prx
   "["   '(:ignore t :wk "prx")
   "[["  '(prx-read-prompt :wk "prx-read-prompt")
   "[b"  '(prx-buffer-with-prompt :wk "prx-buffer-with-prompt")
   "[v"  '(prx-selection-with-prompt :wk "prx-selection-with-prompt")
   "[m"  '(prx-set-current-model :wk "prx-set-current-model")
   ;; eval
   "eb"  '(eval-buffer :wk "eval-buffer")
   "er"  '(eval-region :wk "eval-region")
   "ee"  '(eval-expression :wk "eval-expression")
   "em"  '(eval-minibuffer :wk "eval-minibuffer")
   ;; Buffers
   "b"   '(:ignore t :wk "buffers")
   "bX"  '(kill-buffer-and-window :wk "kill buffer / window")
   "bx"  '(kill-buffer :wk "kill buffer")
   "bb"  '(ivy-switch-buffer :wk "switch buffer")
   "bm"  '(buffer-menu :wk "buffer menu")
   "bi"  '(ibuffer :wk "ibuffer")
   "bn"  '(next-buffer :wk "next buffer")
   "bp"  '(previous-buffer :wk "previous buffer")
   "TAB" '(switch-to-prev-buffer :wk "previous buffer")
   ; Projectile
   "p"   '(:ignore t :wk "projectile")
   "ps"  '(projectile-switch-project :wk "switch project")
   "pi"  '(projectile-ibuffer :wk "ibuffer")
   "pb"  '(projectile-switch-to-buffer :wk "switch buffer")
   "pn"  '(projectile-next-project-buffer :wk "next buffer")
   "pp"  '(projectile-previous-project-buffer :wk "prev buffer")
   "pd"  '(projectile-dired :wk "dired")
   "pf"  '(project-find-file :wk "find file")
   "pm"  '(:ignore t :wk "manage projects")
   "pmc" '(projectile-configure-project :wk "configure project")
   "pma" '(projectile-add-known-project :wk "add known project")
   "pmr" '(projectile-remove-known-project :wk "remove known project")
   "pt"  '(:ignore t :wk "tests")
   "ptr"  '(projectile-test-project :wk "run project tests")
   "ptf"  '(projectile-find-test-file :wk "find test file")
   "ptt"  '(projectile-toggle-between-implementation-and-test :wk "toggle test")
   "pr"  '(projectile-run-project :wk "run project")
   ;; Window
   "w"   '(:ignore t :wk "window")
   "w/"  '(split-window-right :wk "split right")
   "w-"  '(split-window-below :wk "split below")
   "wx"  '(delete-window :wk "delete window")
   "wh"  '(evil-window-left :wk "left")
   "wj"  '(evil-window-down :wk "down")
   "wk"  '(evil-window-up :wk "up")
   "wl"  '(evil-window-right :wk "right")
   "wn"  '(evil-window-next :wk "next window")
   "wp"  '(evil-window-prev :wk "previous window")
   "wN"  '(evil-window-rotate-downwards :wk "window rotate downwards")
   "wP"  '(evil-window-rotate-upwards :wk "window rotate upwards")
   "wo"  '(delete-other-windows :wk "delete other windows")
   ;; Git
   "g"   '(:ignore t :wk "git")
   "gg"  '(magit-status :wk "status")
   "gS"  '(magit-stage :wk "stage file")
   "gU"  '(magit-unstage :wk "unstage file")
   "gc"  '(magit-commit :wk "commit")
   "gd"  '(magit-diff :wk "diff")
   "gl"  '(magit-log :wk "log")
   "gb"  '(magit-blame :wk "blame")
   "gp"  '(magit-push-to-remote :wk "push")
   "gP"  '(magit-pull :wk "pull")
   "gf"  '(magit-fetch :wk "fetch")
   "gj"  '(git-gutter:next-hunk :wk "next hunk")
   "gk"  '(git-gutter:previous-hunk :wk "previous hunk")
   "gs"  '(git-gutter:stage-hunk :wk "stage hunk")
   "gr"  '(git-gutter:stage-hunk :wk "revert hunk")
   "gm"  '(git-gutter:stage-hunk :wk "mark hunk")
   ;; Avy
   "l"    '(evil-avy-goto-line :wk "goto-line")
   "c"    '(evil-avy-goto-char-timer :wk "goto-char")
   "."    '(evil-avy-goto-word-0 :wk "goto-word")
   ;; Eyebrowse
   "0"    '(eyebrowse-switch-to-window-config-0 :wk "workspace 0")
   "1"    '(eyebrowse-switch-to-window-config-1 :wk "workspace 1")
   "2"    '(eyebrowse-switch-to-window-config-2 :wk "workspace 2")
   "3"    '(eyebrowse-switch-to-window-config-3 :wk "workspace 3")
   "4"    '(eyebrowse-switch-to-window-config-4 :wk "workspace 4")
   "5"    '(eyebrowse-switch-to-window-config-5 :wk "workspace 5")
   ;; Ace
   "a"    '(ace-select-window t :wk "ace")
   ;; Help
   "h"    '(:ignore t :wk "help and errors")
   "hh"   '(display-local-help :wk "display-local-help")
   "ho"   '(describe-package :wk "describe-package")
   "hf"   '(describe-function :wk "describe-function")
   "hs"   '(describe-symbol :wk "describe-symbol")
   "hk"   '(describe-keymap :wk "describe-keymap")
   "hm"   '(describe-mode :wk "describe-mode")
   "hn"   '(flymake-goto-next-error :wk "next error")
   "hp"   '(flymake-goto-prev-error :wk "prev error")
   ;; Obsidian
   "o"    '(:ignore t :wk "obsidian")
   "od"   '(obsidian-daily-note :wk "daily note")
   "oc"   '(obsidian-capture :wk "capture")
   "os"   '(obsidian-search :wk "search")
   "ot"   '(obsidian-tag-find :wk "find tag")
   ;; Other
   "i"   '((lambda () (interactive)(find-file "~/.config/emacs/init.el")) :wk "edit init.el")
   "y"   '(vterm :wk "open terminal")
   "f"   '(find-file :wk "find file")
   "t"   '(treemacs :wk "treemacs")
   "'"   '(comment-or-uncomment-region :wk "toggle comment")
   )
)

;; Navigation
(use-package avy
  :defer
  :diminish)

(use-package ace-window
  :defer
  :diminish
  :custom
  (ace-window-display-mode t))

(use-package eyebrowse
  :config
  (eyebrowse-mode)
  :custom
  (eyebrowse-new-workspace t))

;; Project management
(use-package projectile
  :defer)

;; LLM-related
(use-package llm
  :defer)

;; Term
(use-package vterm
  :defer)

;; Obsidian
(use-package obsidian
  :ensure t
  :demand t
  :config
  (obsidian-specify-path "~/Documents/Obsidian Vault")
  (global-obsidian-mode t)
  :custom
  ;; This directory will be used for `obsidian-capture' if set.
  (obsidian-inbox-directory "Random")
  ;; Create missing files in inbox? - when clicking on a wiki link
  ;; t: in inbox, nil: next to the file with the link
  ;; default: t
  ;(obsidian-wiki-link-create-file-in-inbox nil)
  ;; The directory for daily notes (file name is YYYY-MM-DD.md)
  (obsidian-daily-notes-directory "Daily")
  ;; Directory of note templates, unset (nil) by default
  ;(obsidian-templates-directory "Templates")
  ;; Daily Note template name - requires a template directory. Default: Daily Note Template.md
  ;(obsidian-daily-note-template "Daily Note Template.md")
  :bind (:map obsidian-mode-map
  ("C-c M-o" . obsidian-hydra/body)
  ;; Replace C-c C-o with Obsidian.el's implementation. It's ok to use another key binding.
  ("C-c C-o" . obsidian-follow-link-at-point)
  ;; Jump to backlinks
  ("C-c C-b" . obsidian-backlink-jump)
  ;; If you prefer you can use `obsidian-insert-link'
  ("C-c C-l" . obsidian-insert-wikilink)))

(use-package doom-modeline
  :ensure t
  :init
  (setq doom-modeline-minor-modes 1)
  (doom-modeline-mode 1)
  )

;;; Manually customized variables
(setq scroll-preserve-screen-position 1) ; fix scrolling?
(setq make-backup-files nil) ; stop creating backup~ files
(setq auto-save-default nil) ; stop creating #autosave# files
(setq create-lockfiles nil)  ; Temporary to make react dev server not puke...?

;; Load auto-customized vars & faces
(load custom-file)

(provide 'init)
;;; init.el ends here
(put 'dired-find-alternate-file 'disabled nil)
