(setenv "LSP_USE_PLISTS" "true")

; Make it minimal-looking.
(menu-bar-mode -1)
(tool-bar-mode -1)
(tab-bar-mode 1)

;; vsync fix
;(add-to-list 'default-frame-alist '(inhibit-double-buffering . t))

(blink-cursor-mode -1)
(setq visible-bell t)
(setq ring-bell-function 'ignore)

; Performance tweaks
(setq gc-cons-threshold 100000000)
(setq read-process-output-max (* 1024 1024)) ;; 1mb
(setq max-lisp-eval-depth 10000)

;; make it a bigger window
(setq default-frame-alist
       '((width . 256)    ; characters in a line
        (height . 72))) ; number of lines
