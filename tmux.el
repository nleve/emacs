(require 'windmove)
(defvar my/tmux--tty->pane (make-hash-table :test 'equal))
(defun my/tmux--default-socket ()
  (let* ((uid (number-to-string (user-uid)))
         (xdg (getenv "XDG_RUNTIME_DIR"))
         (p1 (and xdg (expand-file-name (format "tmux-%s/default" uid) xdg)))
         (p2 (format "/tmp/tmux-%s/default" uid)))
    (cond
     ((and p1 (file-exists-p p1)) p1)
     ((file-exists-p p2) p2)
     (t nil))))
(defun my/tmux--socket-args ()
  (let ((tmux (getenv "TMUX")))
    (cond
     (tmux (list "-S" (car (split-string tmux ","))))
     ((my/tmux--default-socket) (list "-S" (my/tmux--default-socket)))
     (t nil))))

(defun my/tmux--current-tty ()
  "Return /dev/pts/X for this terminal frame, or nil."
  (let ((tty (frame-parameter nil 'tty)))
    (when (and (stringp tty) (string-match-p "\\`/dev/" tty))
      tty)))

(defun my/tmux--pane-id ()
  (let ((tty (my/tmux--current-tty)))
    (when tty
      (or (gethash tty my/tmux--tty->pane)
          (let* ((args (append (my/tmux--socket-args)
                               (list "list-panes" "-a" "-F" "#{pane_id} #{pane_tty}")))
                 (lines (ignore-errors (apply #'process-lines "tmux" args)))
                 (pane nil))
            (dolist (l lines)
              (when (and (not pane)
                         (string-match "\\`\\(%[0-9]+\\) \\(.*\\)\\'" l)
                         (string= (match-string 2 l) tty))
                (setq pane (match-string 1 l))))
            (when pane (puthash tty pane my/tmux--tty->pane))
            pane)))))
(defun my/tmux-select-pane (flag)
  (let ((pane (my/tmux--pane-id)))
    (when pane
      (apply #'call-process "tmux" nil 0 nil
             (append (my/tmux--socket-args)
                     (list "select-pane" "-t" pane flag))))))
(defun my/tmux-resize-pane (flag amount)
  (let ((pane (my/tmux--pane-id)))
    (when pane
      (apply #'call-process "tmux" nil 0 nil
             (append (my/tmux--socket-args)
                     (list "resize-pane" "-t" pane flag (number-to-string amount)))))))

(defun my/windmove-or-tmux (dir)
  (let ((win (windmove-find-other-window dir)))
    (when (and win
               (window-minibuffer-p win)
               (not (eq win (active-minibuffer-window))))
      (setq win nil))
    (if win
        (select-window win)
      (pcase dir
        ('left  (my/tmux-select-pane "-L"))
        ('down  (my/tmux-select-pane "-D"))
        ('up    (my/tmux-select-pane "-U"))
        ('right (my/tmux-select-pane "-R"))))))

(defun my/window-neighbor (dir)
  ;; Avoid selecting the minibuffer when it's not active.
  (pcase dir
    ('left  (window-in-direction 'left  nil nil nil nil 'never))
    ('right (window-in-direction 'right nil nil nil nil 'never))
    ('up    (window-in-direction 'above nil nil nil nil 'never))
    ('down  (window-in-direction 'below nil nil nil nil 'never))))

(defun my/resize-emacs-tmuxish (dir n)
  (let ((w (selected-window))
        (wl (my/window-neighbor 'left))
        (wr (my/window-neighbor 'right))
        (wu (my/window-neighbor 'up))
        (wd (my/window-neighbor 'down)))
    (pcase dir
      ('right (cond (wr (adjust-window-trailing-edge w  n t))
                    (wl (adjust-window-trailing-edge wl n t))))
      ('left  (cond (wl (adjust-window-trailing-edge wl (- n) t))
                    (wr (adjust-window-trailing-edge w  (- n) t))))
      ('down  (cond (wd (adjust-window-trailing-edge w  n nil))
                    (wu (adjust-window-trailing-edge wu n nil))))
      ('up    (cond (wu (adjust-window-trailing-edge w (- n) nil))
                    (wd (adjust-window-trailing-edge w  (- n) nil)))))))

(defun my/resize-or-tmux (dir &optional amount)
  (let* ((n (or amount 1))
         (has-neighbor
          (if (memq dir '(left right))
              (or (window-in-direction 'left  nil nil nil nil 'never)
                  (window-in-direction 'right nil nil nil nil 'never))
            (or (window-in-direction 'above nil nil nil nil 'never)
                (window-in-direction 'below nil nil nil nil 'never)))))
    (if has-neighbor
        (condition-case _
            (pcase dir
              ('left  (my/resize-emacs-tmuxish 'left n))
              ('right (my/resize-emacs-tmuxish 'right n))
              ('down  (my/resize-emacs-tmuxish 'down n))
              ('up    (my/resize-emacs-tmuxish 'up n)))
          (error
           (pcase dir
             ('left  (my/tmux-resize-pane "-L" n))
             ('right (my/tmux-resize-pane "-R" n))
             ('down  (my/tmux-resize-pane "-D" n))
             ('up    (my/tmux-resize-pane "-U" n)))))
      (pcase dir
        ('left  (my/tmux-resize-pane "-L" n))
        ('right (my/tmux-resize-pane "-R" n))
        ('down  (my/tmux-resize-pane "-D" n))
        ('up    (my/tmux-resize-pane "-U" n))))))

(global-set-key (kbd "M-h") (lambda () (interactive) (my/windmove-or-tmux 'left)))
(global-set-key (kbd "M-j") (lambda () (interactive) (my/windmove-or-tmux 'down)))
(global-set-key (kbd "M-k") (lambda () (interactive) (my/windmove-or-tmux 'up)))
(global-set-key (kbd "M-l") (lambda () (interactive) (my/windmove-or-tmux 'right)))
(global-set-key (kbd "M-H") (lambda () (interactive) (my/resize-or-tmux 'left 1)))
(global-set-key (kbd "M-J") (lambda () (interactive) (my/resize-or-tmux 'down 1)))
(global-set-key (kbd "M-K") (lambda () (interactive) (my/resize-or-tmux 'up 1)))
(global-set-key (kbd "M-L") (lambda () (interactive) (my/resize-or-tmux 'right 1)))

;; copy/paste thru remote tmux
(use-package clipetty
  :ensure t
  :hook (after-init . global-clipetty-mode))
