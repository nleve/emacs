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
(tab-bar-mode 1)

;; vsync fix
(add-to-list 'default-frame-alist '(inhibit-double-buffering . t))

; Turn on some things I like.
(add-hook 'prog-mode-hook 'display-line-numbers-mode)
;(add-hook 'prog-mode-hook 'hl-line-mode)

; fix silly defaults
(setopt sentence-end-double-space nil)
(blink-cursor-mode -1)
(setq visible-bell t)
(setq ring-bell-function 'ignore)

; turn off middle-click paste
(global-unset-key [mouse-2])
(global-unset-key [mouse-3])
(global-set-key [mouse-3] 'eglot-code-actions-at-mouse)
(global-set-key [down-mouse-3] 'mouse-drag-drag)

; Performance tweaks
(setq gc-cons-threshold 100000000)
(setq read-process-output-max (* 1024 1024)) ;; 1mb
(setq max-lisp-eval-depth 10000)

;; UTF-8 files by default
(prefer-coding-system 'utf-8)

;; Font
;(set-frame-font (font-spec :family "JetBrainsMono Nerd Font" :size 15) nil t)
(set-frame-font (font-spec :family "Iosevka Nerd Font" :size 17) nil t)

; Eldoc tweaks
;(setq eldoc-echo-area-display-truncation-message nil)
;(setq eldoc-echo-area-use-multiline-p nil)
;(setq eldoc-display-functions '(eldoc-display-in-echo-area eldoc-display-in-buffer))
(setq eldoc-display-functions '(eldoc-display-in-buffer))
(setq eldoc-idle-delay 0.1)

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
  ;(setq doom-ir-black-brighter-comments 't)
  ;(load-theme 'doom-monokai-spectrum)
  (load-theme 'doom-Iosvkem)
  (custom-set-faces
   ;; Set a more legible background color for code blocks
   '(markdown-code-face ((t (:background "#292c33"))))
   '(org-code ((t (:background "#292c33"))))
   '(org-block ((t (:background "#292c33"))))
   '(org-block-begin-line ((t (:background "#292c33"))))
   '(org-block-end-line ((t (:background "#292c33"))))
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
  (ultra-scroll-mode 1))

(use-package all-the-icons :defer)

;; Env vars
(use-package exec-path-from-shell :ensure
  :init
  (exec-path-from-shell-copy-env "PATH")
  )

;; Add padding to make everything look more comfy
(use-package spacious-padding :ensure
  :init
  (spacious-padding-mode)
  )

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
  (define-key evil-normal-state-map (kbd "K") 'eldoc-box-help-at-point)
  (define-key evil-normal-state-map [mouse-2] nil)
  (define-key evil-insert-state-map [mouse-2] nil)
  (define-key evil-visual-state-map [mouse-2] nil)
  (define-key evil-motion-state-map [mouse-2] nil)

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

(use-package git-gutter
  :ensure
  :diminish
  :hook (prog-mode . git-gutter-mode)
  :config
  (setq git-gutter:update-interval 0.1)
  :custom
  (git-gutter:ask-p nil))

(use-package git-gutter-fringe
  :ensure
  :config
  (define-fringe-bitmap 'git-gutter-fr:added [224] nil nil '(center repeated))
  (define-fringe-bitmap 'git-gutter-fr:modified [224] nil nil '(center repeated))
  (define-fringe-bitmap 'git-gutter-fr:deleted [128 192 224 240] nil nil 'bottom)
  (setq-default left-fringe-width 10)
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
  (company-idle-delay 0.25)
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
  (which-key-idle-delay 0)
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
(use-package treemacs :ensure :defer)

;; tell me what's wrong
(use-package flycheck
  :ensure
  :hook (prog-mode . flycheck-mode)
  :bind (:map flycheck-mode-map
	      ("C-c ! n" . flycheck-goto-next-error)
	      ("C-c ! p" . flycheck-goto-prev-error)
	      ("C-c ! l" . flycheck-show-buffer-diagnostics)))

(use-package eldoc-box
  :defer
  :config
  (setq eldoc-box-clear-with-C-g 't)
  ;(set-face-attribute 'eldoc-box-border nil :background "gray30")

  ;; Fix eldoc-box + spacious-padding-mode
  ;; Function to darken a color slightly
  (defun my-darken-color (color)
    "Return a slightly darker version of COLOR (a hex string like \"#RRGGBB\")."
    (let* ((rgb (color-name-to-rgb color)) ; Convert to (R G B) list, 0.0-1.0
           (r (max 0 (- (nth 0 rgb) 0.04))) ; Reduce each by 0.04, clamp at 0
           (g (max 0 (- (nth 1 rgb) 0.04)))
           (b (max 0 (- (nth 2 rgb) 0.04))))
      (format "#%02x%02x%02x"
              (floor (* r 255))  ; Convert back to 0-255 range
              (floor (* g 255))
              (floor (* b 255)))))
  
  ;; Function to update eldoc-box-frame-parameters and faces
  (defun my-update-eldoc-box-padding (&rest _)
    "Update `eldoc-box-frame-parameters`, `eldoc-box-border`, and `eldoc-box-body`.
Ignore any arguments (for theme hook compatibility)."
    (let ((params (copy-sequence eldoc-box-frame-parameters))
          (default-bg (face-background 'default nil t)))
      (if spacious-padding-mode
          (let ((darker-bg (my-darken-color default-bg))) ; Darken the default background
            ;; Update frame parameters
            (setq params (assq-delete-all 'internal-border-width params))
            (setq params (assq-delete-all 'left-fringe params))
            (setq params (assq-delete-all 'right-fringe params))
            (setq params (append params
				 `((internal-border-width . ,(spacious-padding--get-internal-border-width))
                                   (left-fringe . ,(or (spacious-padding--get-left-fringe-width)
                                                       (spacious-padding--get-fringe-width)))
                                   (right-fringe . ,(or (spacious-padding--get-right-fringe-width)
							(spacious-padding--get-fringe-width))))))
            ;; Set border and body faces to the darker background
            (set-face-attribute 'eldoc-box-border nil :background darker-bg)
            (set-face-attribute 'eldoc-box-body nil :background darker-bg))
	;; Reset to original values when mode is off
	(setq params (assq-delete-all 'internal-border-width params))
	(setq params (assq-delete-all 'left-fringe params))
	(setq params (assq-delete-all 'right-fringe params))
	(setq params (append params '((internal-border-width . 1)
                                      (left-fringe . 3)
                                      (right-fringe . 3))))
	(set-face-attribute 'eldoc-box-border nil
                            :background (if (eq frame-background-mode 'dark) "white" "black"))
	(set-face-attribute 'eldoc-box-body nil :background nil)) ; Reset to default
      (setq eldoc-box-frame-parameters params)
      ;; Reset existing frame if it exists
      (when (and eldoc-box--frame (frame-live-p eldoc-box--frame))
	(delete-frame eldoc-box--frame)
	(setq eldoc-box--frame nil))))
  
  ;; Hook into spacious-padding-mode toggle
  (add-hook 'spacious-padding-mode-hook #'my-update-eldoc-box-padding)
  
  ;; Hook into theme changes
  (add-hook 'enable-theme-functions #'my-update-eldoc-box-padding)
  
  ;; Run once to apply current state
  (my-update-eldoc-box-padding)
  )

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
   "bb"  '(ido-switch-buffer :wk "switch buffer")
   "bm"  '(buffer-menu :wk "buffer menu")
   "bi"  '(ibuffer :wk "ibuffer")
   "bn"  '(next-buffer :wk "next buffer")
   "bp"  '(previous-buffer :wk "previous buffer")
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
   "gg"  '(magit-status :wk "magit status")
   "gS"  '(magit-stage :wk "stage file")
   "gU"  '(magit-unstage :wk "unstage file")
   "gc"  '(magit-commit :wk "commit")
   "gd"  '(magit-file-dispatch :wk "dispatch")
   "gl"  '(magit-log :wk "log")
   "gb"  '(magit-blame :wk "blame")
   "gn"  '(git-gutter:next-hunk :wk "next hunk")
   "gp"  '(git-gutter:previous-hunk :wk "previous hunk")
   "gs"  '(git-gutter:stage-hunk :wk "stage hunk")
   "gr"  '(git-gutter:revert-hunk :wk "revert hunk")
   "gh"  '(git-gutter:popup-hunk :wk "diff hunk")
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
   "/o"   '(consult-outline :wk "outline")
   "/b"   '(consult-buffer :wk "buffer")
   "/t"   '(consult-theme :wk "theme")
   "/l"   '(consult-focus-lines :wk "focus lines")
   ;; gptel
   "."    '(:ignore t :wk "gptel")
   ".."   '(gptel-menu :wk "gptel-menu")
   ".a"   '(gptel-add :wk "gptel-add")
   ".c"   '(gptel :wk "chat")
   ".f"   '(gptel-context-add-file :wk "add/remove file")
   ".x"   '(gptel-abort :wk "abort")
   ;; Other
   "i"   '((lambda () (interactive)(find-file "~/.config/emacs/init.el")) :wk "edit init.el")
   "y"   '(eat :wk "open terminal")
   "f"   '(find-file :wk "find file")
   "t"   '(treemacs :wk "treemacs")
   "'"   '(comment-or-uncomment-region :wk "toggle comment")
   )
)

;; Navigation
(use-package avy :ensure :defer :diminish
  :custom
  (avy-timeout-seconds 0.15))

(use-package ace-window :ensure :defer :diminish)

;; Term
(use-package vterm :ensure :defer
  :init
  (add-hook 'vterm-mode-hook 'evil-insert-state)
  )
(use-package eat :ensure :defer
  :init
  (add-hook 'eat-mode-hook 'evil-insert-state)
  )

(use-package markdown-mode
  :ensure
  :defer
  :config
  (setq markdown-fontify-code-blocks-natively t)
  )

(use-package pdf-tools :ensure :defer)

(use-package doom-modeline
  :ensure t
  :init
  (setq doom-modeline-minor-modes 1)
  (doom-modeline-mode 1)
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
  :ensure)

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

;; gptel
(use-package gptel
  :ensure
  :defer
  :config
  (setq gptel-confirm-tool-calls 't)
  (gptel-make-anthropic "Claude"
    :stream t
    :key (getenv "CLAUDE_API_KEY"))
  
  (setq gptel-model 'Qwen2.5-Coder-32B-Instruct-exl2-8bpw-8hb
        gptel-backend (gptel-make-openai "local"
			:host (getenv "LOCAL_API_URL")
			:protocol "http"
			:endpoint "/v1/chat/completions"
			:stream t
			:key (getenv "LOCAL_API_KEY")
			:models '("Qwen2.5-Coder-32B-Instruct-exl2-8bpw-8hb")))
  )

(use-package mlscroll
  :ensure t
  :config
  (mlscroll-mode 1))

(use-package pyvenv :ensure :defer)

;;; Manually customized variables
(setq scroll-preserve-screen-position 1) ; fix scrolling?
(setq make-backup-files nil) ; stop creating backup~ files
(setq auto-save-default nil) ; stop creating #autosave# files
(setq create-lockfiles nil)  ; Temporary to make react dev server not puke...?
(setq right-divider-width 3)


;; Custom variable for Esc clearing (if not already defined)
(defcustom eldoc-box-clear-with-esc nil
  "If non-nil, clear the eldoc-box childframe when pressing Esc."
  :type 'boolean
  :group 'eldoc-box)

;; Function to quit frame on Esc
(defun my-eldoc-box-quit-on-esc (&rest _)
  "Quit the eldoc-box frame if `eldoc-box-clear-with-esc' is non-nil."
  (when eldoc-box-clear-with-esc
    (eldoc-box-quit-frame)))

;; Advise evil-force-normal-state instead of keyboard-escape-quit
(advice-add 'evil-force-normal-state :before #'my-eldoc-box-quit-on-esc)

;; Enable Esc clearing by default (optional)
(setq eldoc-box-clear-with-esc t)

;; Load auto-customized vars & faces
(load custom-file)

(put 'dired-find-alterate-file 'disabled nil)
(setq dired-kill-when-opening-new-dired-buffer t)

(provide 'init)
;;; init.el ends here
