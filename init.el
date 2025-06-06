;;; init.el --- emacs config
;;; Commentary:
;
; This is my init.el.  There are many like it, but this one is mine.
;
;;; Code:
(toggle-scroll-bar 1)
(defun n/disable-scroll-bar (window)
  "Disable scroll bar for WINDOW."
  (set-window-scroll-bars window nil nil nil nil 1))

(n/disable-scroll-bar (minibuffer-window))

(setq shell-command-switch "-ic")
(setq custom-file "~/.config/emacs/custom.el")

;; Load auto-customized vars & faces
(load custom-file)

;; eliminate gc stutter. incredible
(use-package gcmh :ensure :diminish
  :config
  (setq gcmh-idle-delay 3)
  :init
  (gcmh-mode))

;; Font
;(set-frame-font (font-spec :family "JetBrainsMono Nerd Font" :size 14) nil t)
;(set-frame-font (font-spec :family "Hack" :size 15) nil t)
;(set-frame-font (font-spec :family "Iosevka Nerd Font" :size 17) nil t)
;(set-frame-font (font-spec :family "Source Code Pro" :size 14) nil t)
(defvar my/fixed-font-spec '(:family "JetBrainsMono Nerd Font" :size 15))
(defvar my/variable-font-spec '(:family "Noto Sans" :size 16 :weight light))
(set-face-attribute 'default nil :font (apply #'font-spec my/fixed-font-spec))
(set-face-attribute 'fixed-pitch nil :font (apply #'font-spec my/fixed-font-spec))
(set-face-attribute 'variable-pitch nil :font (apply #'font-spec my/variable-font-spec))

 (custom-theme-set-faces
   'user
   `(variable-pitch ((t ,my/variable-font-spec)))
   `(org-default ((t ,my/variable-font-spec)))
   `(fixed-pitch ((t ,my/fixed-font-spec)))
   `(default ((t ,my/fixed-font-spec)))
   )

(dolist (face '(default fixed-pitch))
  (set-face-attribute `,face nil :font (apply #'font-spec my/fixed-font-spec)))

; Turn on some things I like.
(add-hook 'prog-mode-hook 'display-line-numbers-mode)
(add-hook 'prog-mode-hook 'hl-line-mode)

; fix silly defaults
(setopt sentence-end-double-space nil)

;; TODO use something like this to switch common modes quick
;; (defvar-keymap prot-prefix-mode-map
;;   :doc "Prefix keymap for minor mode toggles."
;;   :name "Toggle"
;;   :prefix 'prot-prefix-mode
;;   "f" #'flymake-mode
;;   "h" #'hl-line-mode
;;   "k" #'keycast-mode-line-mode
;;   "l" #'logos-focus-mode
;;   "m" #'menu-bar-mode
;;   "n" #'display-line-numbers-mode
;;   "t" #'toggle-truncate-lines
;;   "s" #'spacious-padding-mode
;;   "r" #'rainbow-mode
;;   "v" #'variable-pitch-mode)

; turn off middle-click paste
(global-unset-key [mouse-2])
(global-unset-key [mouse-3])
(global-set-key [mouse-3] 'eglot-code-actions-at-mouse)
(global-set-key [down-mouse-3] 'mouse-drag-drag)

;; UTF-8 files by default
(prefer-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)
(set-language-environment 'utf-8)
(set-selection-coding-system 'utf-8)

; Eldoc tweaks
;(setq eldoc-echo-area-display-truncation-message nil)
;(setq eldoc-echo-area-use-multiline-p nil)
(setq eldoc-display-functions '(eldoc-display-in-buffer))
;(setq eldoc-display-functions '(eldoc-display-in-buffer))
(setq eldoc-idle-delay 0.1)

;; Global keybinds
(global-set-key (kbd "s-l") 'evil-window-right)
(global-set-key (kbd "s-h") 'evil-window-left)
(global-set-key (kbd "s-j") 'evil-window-down)
(global-set-key (kbd "s-k") 'evil-window-up)

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

(use-package request
  :ensure)

;; Theme & Appearance
(use-package spacemacs-theme
  :init
  (load-theme 'spacemacs-dark)
  )
(use-package doom-themes
  :init
  ;(setq doom-ir-black-brighter-comments 't)
  ;(load-theme 'doom-monokai-spectrum)
  ;(load-theme 'modus-operandi)
  ;(load-theme 'doom-ir-black)
  ;(load-theme 'gruber-darker)
  ;(load-theme 'doom-dark+)
  ;(load-theme 'modus-vivendi-tinted)
  (custom-set-faces
   ;; Set a more legible background color for code blocks
   ;'(markdown-code-face ((t (:background "#292c33"))))
   ;'(org-code ((t (:background "#292c33"))))
   ;'(org-block ((t (:background "#292c33"))))
   ;'(org-block-begin-line ((t (:background "#292c33"))))
   ;'(org-block-end-line ((t (:background "#292c33"))))
   )
  )

;; Fast smooth-scrolling
(use-package ultra-scroll
  :vc (:url "https://github.com/jdtsmith/ultra-scroll"
	    :rev :newest)
  :init
  (setq scroll-conservatively 101 ; important!
        scroll-margin 0)
  :config
  (if (display-graphic-p) (ultra-scroll-mode 1))
  )

(use-package all-the-icons :defer) ;; run all-the-icons-install-fonts on first run
(use-package all-the-icons-dired :ensure :defer)

;; Env vars
(use-package exec-path-from-shell :ensure
  :init
  (exec-path-from-shell-initialize)
  )

;; Add padding to make everything look more comfy
;; (use-package spacious-padding :ensure
;;  :config
;;  (setq spacious-padding-widths
;; 	'( :internal-border-width 10
;; 	   :header-line-width 4
;; 	   :mode-line-width 3
;; 	   :tab-width 5
;; 	   :right-divider-width 1
;; 	   :scroll-bar-width 8
;; 	   :fringe-width 10))
;;  :init
;;  (spacious-padding-mode)
;;  )

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
  (evil-want-C-u-scroll nil)
  (evil-want-keybinding nil)
  :config
  (evil-mode 1)
  ;; Make movement keys work like they should
  (define-key evil-normal-state-map (kbd "j") 'evil-next-visual-line)
  (define-key evil-normal-state-map (kbd "k") 'evil-previous-visual-line)
  (define-key evil-normal-state-map (kbd "g ]") 'xref-go-forward)
  (define-key evil-normal-state-map (kbd "g [") 'xref-go-back)
  (define-key evil-normal-state-map (kbd "K") (lambda () (interactive) (if (display-graphic-p) (call-interactively #'eldoc-box-help-at-point) (call-interactively #'eldoc-print-current-symbol-info))))
  (define-key evil-normal-state-map [mouse-2] nil)
  (define-key evil-insert-state-map [mouse-2] nil)
  (define-key evil-visual-state-map [mouse-2] nil)
  (define-key evil-motion-state-map [mouse-2] nil)

  ;; Add some quick fwd / back keys
  (enable-smooth-scroll-for-function evil-next-visual-line)
  (enable-smooth-scroll-for-function evil-previous-visual-line)
  ;; Make horizontal movement cross wrapped lines
  (setq-default evil-cross-lines t)
  )

(use-package evil-collection
  :after evil
  :diminish (evil-collection-unimpaired-mode)
  :init
  ; prevent it from stealing K
  (add-hook 'evil-collection-setup-hook
            (lambda (mode keymaps)
              (when (eq mode 'eglot)
                (evil-collection-define-key 'normal 'eglot-mode-map
                  "K" nil))))
  (evil-collection-init)
  )

;; Git
(use-package magit :defer)

(use-package diff-hl
  :ensure
  :config
  (setq diff-hl-disable-on-remote 't)
  (setq diff-hl-show-staged-changes nil)
  (add-hook 'magit-post-refresh-hook 'diff-hl-magit-post-refresh)
  (global-diff-hl-mode 1)
  (diff-hl-flydiff-mode 1)
  (eval-after-load 'diff-hl
    '(progn
       (advice-add 'diff-hl-next-hunk :after (lambda (&rest _args) (recenter 5)))
       ))
  )

;; Completion
(use-package company
  :defer
  :diminish
  :init
  (global-company-mode)
  :bind (
	 ("C-<return>" . company-manual-begin))
  :custom
  (company-idle-delay 1)
  (company-tooltip-align-annotations 't)
  (company-minimum-prefix-length 1))

;; someday maybe I'll use corfu instead of company
;; (use-package corfu
;;   :ensure
;;   :config
;;   (global-corfu-mode)
;;   (setq corfu-auto t
;; 	corfu-quit-no-match 'separator)
;;   )

;; (use-package nerd-icons-corfu
;;   :ensure
;;   :config
;;   (add-to-list 'corfu-margin-formatters #'nerd-icons-corfu-formatter)
;;   )

;; Which Key
(use-package which-key
  :diminish
  :init
  (which-key-mode 1)
  :custom
  ; DO NOT SET TO 0. CAUSES LAG.
  (which-key-idle-delay 0.05)
  (which-key-separator ":")
  (which-key-prefix-prefix "+"))

;; Language-specific Modes
(use-package zig-mode
  :defer
  :mode "\\.zig\\'"
  :ensure
)

(use-package elixir-ts-mode
  :ensure)

(use-package treesit-auto
  :custom
  (treesit-auto-install 'prompt)
  :config
  (treesit-auto-add-to-auto-mode-alist 'all)
  (delete 'janet treesit-auto-langs) ;; fails to compile for some reason - yeet
  (global-treesit-auto-mode))

;; LSP
(use-package eglot
  :hook (prog-mode . eglot-ensure)
  :init
  (setq eglot-stay-out-of '(flymake))
  :config
  :bind (:map
	 eglot-mode-map
	 ("C-c c a" . eglot-code-actions)
	 ("C-c c o" . eglot-code-actions-organize-imports)
	 ("C-c c r" . eglot-rename)
	 ("C-c c f" . eglot-format))
  )

;; used once in a while for file nav
;(use-package treemacs :ensure :defer)
(use-package dired-sidebar
  :ensure
  :commands (dired-sidebar-toggle-sidebar))

;; tell me what's wrong
(add-hook 'prog-mode-hook #'flymake-mode)

(use-package eldoc-box
  :defer
  :config

  (setq eldoc-box-clear-with-C-g 't)
  ;; Function to darken a color slightly
  (defun my-darken-color (color)
    "Return a slightly darker version of COLOR (a hex string like \"#RRGGBB\")."
    (let* ((rgb (color-name-to-rgb color)) ; Convert to (R G B) list, 0.0-1.0
           (r (max 0 (- (nth 0 rgb) 0.08))) ; Reduce each by 0.08, clamp at 0
           (g (max 0 (- (nth 1 rgb) 0.08)))
           (b (max 0 (- (nth 2 rgb) 0.08))))
      (format "#%02x%02x%02x"
              (floor (* r 255))  ; Convert back to 0-255 range
              (floor (* g 255))
              (floor (* b 255)))))

  (defun my-darken-eldoc-box ()
    (interactive)
    (let ((darker-bg (my-darken-color (face-background 'default nil t))))
      (set-face-attribute 'eldoc-box-border nil :background darker-bg)
      (set-face-attribute 'eldoc-box-body nil :background darker-bg)
      )
    )
  
  ;; Hook into theme changes
  (add-hook 'enable-theme-functions #'my-darken-eldoc-box)
  
  ;; Run once to apply current state
  (my-darken-eldoc-box)
  )

(load "~/.config/emacs/llm.el")
(load "~/.config/emacs/org.el")

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
   "bw"  '(kill-buffer-and-window :wk "kill buffer / window")
   "bx"  '(kill-buffer :wk "kill buffer")
   "bs"  '(ido-switch-buffer :wk "switch buffer")
   "bb"  '(bury-buffer :wk "bury buffer")
   "bi"  '(ibuffer :wk "ibuffer")
   "bn"  '(next-buffer :wk "next buffer")
   "bp"  '(previous-buffer :wk "previous buffer")
   ;; Window
   "w"   '(:ignore t :wk "window")
   "w/"  '(split-window-right :wk "split right")
   "w-"  '(split-window-below :wk "split below")
   "w?"  '(split-root-window-right :wk "split root right")
   "w_"  '(split-root-window-below :wk "split root below")
   "wx"  '(delete-window :wk "delete window")
   "wc"  '(centered-window-mode :wk "centered-window-mode")
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
   "gg"  '(magit-status :wk "magit status")
   "gS"  '(magit-stage :wk "stage file")
   "gU"  '(magit-unstage :wk "unstage file")
   "gc"  '(magit-commit :wk "commit")
   "gd"  '(magit-file-dispatch :wk "dispatch")
   "gl"  '(magit-log :wk "log")
   "gb"  '(magit-blame :wk "blame")
   "gn"  '(diff-hl-next-hunk :wk "next hunk")
   "gp"  '(diff-hl-previous-hunk :wk "previous hunk")
   "gs"  '(diff-hl-stage-dwim :wk "stage hunk")
   "gr"  '(diff-hl-revert-hunk :wk "revert hunk")
   "gh"  '(diff-hl-show-hunk :wk "show hunk")
   ;; Avy
   "l"    '(evil-avy-goto-line :wk "goto-line")
   "c"    '(evil-avy-goto-char-timer :wk "goto-char")
   ;; Ace
   "a"    '(ace-select-window t :wk "ace")
   ;; Help
   "h"    '(:ignore t :wk "help and errors")
   "hh"   '(eldoc-box-help-at-point :wk "eldoc box help at point")
   "he"   '(eldoc :wk "eldoc")
   "hl"   '(display-local-help :wk "display-local-help")
   "ho"   '(describe-package :wk "describe-package")
   "hf"   '(describe-function :wk "describe-function")
   "hs"   '(describe-symbol :wk "describe-symbol")
   "hk"   '(describe-keymap :wk "describe-keymap")
   "hm"   '(describe-mode :wk "describe-mode")
   "hn"   '(flymake-goto-next-error :wk "next error")
   "hp"   '(flymake-goto-prev-error :wk "prev error")
   ;; Search
   "/"    '(:ignore t :wk "consult")
   "//"   '(consult-line :wk "line")
   "/r"   '(consult-ripgrep :wk "ripgrep")
   "/f"   '(consult-fd :wk "file")
   "/h"   '((lambda () (interactive)(consult-fd "~/")) :wk "file (from home)")
   "/o"   '(consult-outline :wk "outline")
   "/b"   '(consult-buffer :wk "buffer")
   "/t"   '(consult-theme :wk "theme")
   "/l"   '(consult-focus-lines :wk "focus lines")
   "/e"   '(consult-flymake :wk "consult-flymake")
   "p"   '(consult-project-extra-find :wk "project find")
   "P"   '(consult-project-extra-find-other-window :wk "project find ow")
   ;; gptel
   "."    '(:ignore t :wk "gptel")
   ".."   '(gptel-menu :wk "gptel-menu")
   ".a"   '(gptel-add :wk "gptel-add")
   ".c"   '(gptel :wk "chat")
   ".x"   '(gptel-abort :wk "abort")
   ;; embark
   ","   '(embark-act :wk "embark-dwim")
   ;; eat
   "t"   '(:ignore t "terminal")
   "tt"  '(eat :wk "open terminal")
   "tT"  '(eat-other-window :wk "open terminal ow")
   "tp"  '(eat-project :wk "project terminal")
   "tP"  '(eat-project-other-window :wk "project terminal o
w")
   "tn"  '((lambda () (interactive) (let ((current-prefix-arg '(4))) (call-interactively 'eat))) :wk "new terminal")
   ;; Other
   "i"   '((lambda () (interactive)(find-file "~/.config/emacs/init.el")) :wk "edit init.el")
   "n"   '((lambda () (interactive)(find-file "~/notebook.org")) :wk "edit notebook.org")
   "f"   '(find-file :wk "find file")
   "TAB" '(dired-sidebar-toggle-sidebar :wk "dired sidebar")
   "'"   '(comment-or-uncomment-region :wk "toggle comment")
   )
)

;; Navigation
(use-package avy :ensure :defer :diminish
  :bind (("C-;" . evil-avy-goto-char-timer))
  :custom
  (avy-timeout-seconds 0.2))

(use-package ace-window :ensure :defer :diminish)

;; Term
(use-package eat :ensure :defer
  :custom
  (eat-kill-buffer-on-exit t)
  (process-adaptive-read-buffering nil) ; makes EAT a lot quicker!
  :init
  (add-hook 'eat-mode-hook 'evil-insert-state)
  )

(use-package markdown-mode
  :ensure
  :defer
  :config
  (setq markdown-fontify-code-blocks-natively t)
  (add-hook 'markdown-mode-hook 'visual-line-mode)
  (add-hook 'markdown-mode-hook 'variable-pitch-mode)
  (add-hook 'markdown-mode-hook 'toggle-word-wrap)
  )

(use-package pdf-tools :ensure :defer)

(use-package doom-modeline
  :ensure t
  :hook (after-init . doom-modeline-mode)
  :custom
  (doom-modeline-minor-modes nil)
  (doom-modeline-buffer-state-icon t)
  (doom-modeline-buffer-file-name-style 'auto) ;'file-name-with-project)
  (doom-modeline-height 15)
  )

;; Better minibuffer that shows completion candidates
(use-package vertico
  :ensure
  :config
  (setq vertico-cycle t)
  (setq vertico-resize t)
  :init
  (vertico-mode 1)
  )

;; show marginalia for minibuffer completions
(use-package marginalia
  :ensure
  :config
  (marginalia-mode 1))

;; out-of-order fuzzy matching / complettion
(use-package orderless
  :ensure
  :config
  (setq completion-styles '(orderless basic))
  )

;; enhanced versions of built-in functionality such as search
(use-package consult
  :ensure
  :bind (("C-'" . consult-project-extra-find)))

;; consult integration with project.el
(use-package consult-project-extra
  :ensure)

;; consult integration with eglot
(use-package consult-eglot
  :ensure)

;; Perform actions!
(use-package embark
  :ensure
  :bind (("C-," . embark-act)
	 :map minibuffer-local-map
	 ("C-c C-c" . embark-collect)
	 ("C-c C-e" . embark-export))
  )

;; embark integration with consult
(use-package embark-consult
  :ensure)

;(use-package mlscroll
;  :ensure t
;  :config
;  (mlscroll-mode)
;  )

(use-package pet
  :config
  (add-hook 'python-base-mode-hook 'pet-mode -10))

(use-package popper :ensure
  :bind (("s-[" . popper-toggle)
	 ("s-]"   . popper-cycle)
	 ("M-p" . popper-toggle-type))
  :init
  (setq popper-reference-buffers
	'("\\*eldoc" help-mode)
	)
  (popper-mode)
  (popper-echo-mode)
  )

(use-package centered-window :ensure :defer)

(use-package tabspaces
  :vc (:url "https://github.com/mclear-tools/tabspaces"
	    :rev :newest)
  :ensure
  :hook (after-init . tabspaces-mode)
  :commands (tabspaces-switch-or-create-workspace
             tabspaces-open-or-create-project-and-workspace)
  :custom
  (tabspaces-use-filtered-buffers-as-default t)
  (tabspaces-default-tab "Default")
  (tabspaces-remove-to-default t)
  (tabspaces-include-buffers '("*scratch*"))
  ;; sessions
  (tabspaces-session nil)
  (tabspaces-session-auto-restore nil)
  (tab-bar-new-tab-choice "*scratch*"))

;; Filter Buffers for Consult-Buffer
(with-eval-after-load 'consult
  ;; hide full buffer list (still available with "b" prefix)
  (consult-customize consult--source-buffer :hidden t :default nil)
  ;; set consult-workspace buffer list
  (defvar consult--source-workspace
    (list :name     "Workspace Buffers"
          :narrow   ?w
          :history  'buffer-name-history
          :category 'buffer
          :state    #'consult--buffer-state
          :default  t
          :items    (lambda () (consult--buffer-query
                                :predicate #'tabspaces--local-buffer-p
                                :sort 'visibility
                                :as #'buffer-name))))

  ;; "Set workspace buffer list for consult-buffer."
  (add-to-list 'consult-buffer-sources 'consult--source-workspace))

;; Make text pulse when we do stuff to it
(use-package evil-goggles
  :diminish
  :config
  (setq evil-goggles-duration 0.03)
  (evil-goggles-mode)
  (evil-goggles-use-diff-faces))

;; Do this for yanked regions too.
(require 'pulse)
(setq pulse-flag t) ; enable animation for non-GUI frames
(setq pulse-iterations 5    ; number of fade steps
      pulse-delay      0.03) ; delay between steps (seconds)
;; Pulse copied regions
(defun my/pulse-copied-region (beg end &rest _)
  "Pulse the region that was just copied or yanked, from BEG to END."
  (pulse-momentary-highlight-region beg end))

;; Add the new pulsing behavior to kill-ring-save (M-w)
(advice-add 'kill-ring-save :after #'my/pulse-copied-region)

;; Add the new pulsing behavior to evil-yank
(advice-add 'evil-yank :after #'my/pulse-copied-region)

;;; Manually customized variables
(setq scroll-preserve-screen-position nil) ; fix scrolling?
(setq make-backup-files nil) ; stop creating backup~ files
(setq auto-save-default nil) ; stop creating #autosave# files
(setq create-lockfiles nil)  ; Temporary to make react dev server not puke...?

;; Custom variable for Esc clearing (if not already defined)
(defcustom eldoc-box-clear-with-esc nil
  "If non-nil, clear the eldoc-box childframe when pressing Esc."
  :type 'boolean
  :group 'eldoc-box)

;; Function to quit frame on Esc
(defun my-eldoc-box-quit-on-esc (&rest _)
  "Quit the eldoc-box frame if `eldoc-box-clear-with-esc' is non-nil."
  (when (fboundp 'eldoc-box-quit-frame)
    (eldoc-box-quit-frame)))

;; Advise evil-force-normal-state instead of keyboard-escape-quit
(advice-add 'evil-force-normal-state :before #'my-eldoc-box-quit-on-esc)

;; Enable Esc clearing by default (optional)
(setq eldoc-box-clear-with-esc t)

;; Compile customization
(setq compilation-scroll-output t
      compilation-auto-jump-to-first-error t
      compilation-max-output-line-length nil
      )

;; Fix console mouse behavior
(unless (display-graphic-p) (xterm-mouse-mode))

(put 'dired-find-alterate-file 'disabled nil)
(setq dired-kill-when-opening-new-dired-buffer t)

;; set gc-cons-threshold to something more reasonable now that packages are loaded
(setq gc-cons-threshold 80000000)

;; Start emacs server
(require 'server)

(unless (server-running-p)
  (server-start))

(desktop-save-mode 1)

(dtrt-indent-global-mode 1)

(provide 'init)
;;; init.el ends here
