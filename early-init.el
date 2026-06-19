;; -*- lexical-binding: t; -*-
(setenv "LSP_USE_PLISTS" "true")

; Make it minimal-looking.
(menu-bar-mode -1)
(tool-bar-mode -1)
(tab-bar-mode -1)

;; vsync fix
;(add-to-list 'default-frame-alist '(inhibit-double-buffering . t))

(blink-cursor-mode -1)
(setq visible-bell t)
(setq ring-bell-function 'ignore)

;; Keep Evil's `cursor-type' changes visible in xterm-compatible TTY frames.
(setq xterm-update-cursor 'type)
(with-eval-after-load 'term/xterm
  (when (and (not (display-graphic-p))
             (fboundp 'xterm--init-update-cursor)
             (not (memq 'xterm--post-command-hook post-command-hook)))
    (xterm--init-update-cursor)))

; Performance tweaks
(setq gc-cons-threshold 100000000)
(setq read-process-output-max (* 1024 1024)) ;; 1mb
(setq max-lisp-eval-depth 10000)

;; make it a bigger window
(setq default-frame-alist
       '((width . 140)    ; characters in a line
        (height . 72))) ; number of lines
