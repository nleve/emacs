;;; diff.el --- fast staged/unstaged review UI  -*- lexical-binding: t; -*-

;;; Commentary:
;; A focused review layout for moving quickly through changed files:
;; changed-file tree on the left, staged/index diff in the middle, and
;; unstaged/worktree diff on the right.

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'subr-x)
(require 'magit)
(require 'magit-apply)
(require 'magit-diff)
(require 'magit-section)

(declare-function evil-define-key "evil-core" (state keymaps key def &rest bindings))
(declare-function evil-set-initial-state "evil-core" (mode state))

(defgroup n/diff-review nil
  "Fast staged/unstaged review UI."
  :group 'magit)

(defcustom n/diff-review-sidebar-width 36
  "Width of the changed-file tree in `n/diff-review'."
  :type 'integer
  :group 'n/diff-review)

(defcustom n/diff-review-diff-arguments '("--no-ext-diff")
  "Diff arguments used by the staged and unstaged review panes."
  :type '(repeat string)
  :group 'n/diff-review)

(defface n/diff-review-root-face
  '((t :inherit font-lock-comment-face :weight bold))
  "Face for the repository root heading."
  :group 'n/diff-review)

(defface n/diff-review-directory-face
  '((t :inherit dired-directory))
  "Face for directories in the changed-file tree."
  :group 'n/diff-review)

(defface n/diff-review-selected-face
  '((t :inherit highlight))
  "Face for the selected changed file."
  :group 'n/diff-review)

(defface n/diff-review-staged-face
  '((t :inherit magit-diff-added))
  "Face for staged status markers."
  :group 'n/diff-review)

(defface n/diff-review-unstaged-face
  '((t :inherit magit-diff-context-highlight))
  "Face for unstaged status markers."
  :group 'n/diff-review)

(defface n/diff-review-untracked-face
  '((t :inherit warning))
  "Face for untracked status markers."
  :group 'n/diff-review)

(defvar n/diff-review--root nil)
(defvar n/diff-review--files nil)
(defvar n/diff-review--selected-file nil)
(defvar n/diff-review--tree-window nil)
(defvar n/diff-review--staged-window nil)
(defvar n/diff-review--unstaged-window nil)
(defvar n/diff-review--previous-window-configuration nil)
(defvar n/diff-review--collapsed-dirs (make-hash-table :test 'equal))
(defvar n/diff-review--refresh-timer nil)

(defvar-local n/diff-review--buffer-role nil)
(defvar-local n/diff-review--buffer-file nil)

(defun n/diff-review--repo-name (root)
  "Return a short display name for ROOT."
  (file-name-nondirectory (directory-file-name root)))

(defun n/diff-review--buffer-name (role)
  "Return the review buffer name for ROLE."
  (format "*Diff Review %s: %s*"
          role
          (if n/diff-review--root
              (n/diff-review--repo-name n/diff-review--root)
            "repo")))

(defun n/diff-review--root ()
  "Return the current Git root or signal a user error."
  (or (magit-toplevel)
      (user-error "Not in a Git repository")))

(defun n/diff-review--entry-path (entry)
  "Return ENTRY's current file path."
  (plist-get entry :path))

(defun n/diff-review--entry-index-status (entry)
  "Return ENTRY's index status character."
  (or (plist-get entry :index) ?\s))

(defun n/diff-review--entry-worktree-status (entry)
  "Return ENTRY's worktree status character."
  (or (plist-get entry :worktree) ?\s))

(defun n/diff-review--entry-untracked-p (entry)
  "Return non-nil when ENTRY is untracked."
  (eq (n/diff-review--entry-index-status entry) ??))

(defun n/diff-review--entry-staged-p (entry)
  "Return non-nil when ENTRY has staged/index changes."
  (let ((x (n/diff-review--entry-index-status entry)))
    (and (not (eq x ?\s))
         (not (eq x ??)))))

(defun n/diff-review--entry-unstaged-p (entry)
  "Return non-nil when ENTRY has unstaged/worktree changes."
  (or (n/diff-review--entry-untracked-p entry)
      (not (eq (n/diff-review--entry-worktree-status entry) ?\s))))

(defun n/diff-review--entry-face (entry)
  "Return the tree status face for ENTRY."
  (cond
   ((n/diff-review--entry-untracked-p entry) 'n/diff-review-untracked-face)
   ((and (n/diff-review--entry-staged-p entry)
         (n/diff-review--entry-unstaged-p entry))
    'magit-diff-our)
   ((n/diff-review--entry-staged-p entry) 'n/diff-review-staged-face)
   (t 'n/diff-review-unstaged-face)))

(defun n/diff-review--entry-status-string (entry)
  "Return ENTRY's two-character Git status string."
  (format "%c%c"
          (n/diff-review--entry-index-status entry)
          (n/diff-review--entry-worktree-status entry)))

(defun n/diff-review--status-entries ()
  "Return changed-file entries for `n/diff-review--root'."
  (let ((default-directory n/diff-review--root))
    (sort
     (mapcar
      (pcase-lambda (`(,file ,source ,index ,worktree))
        (list :path file
              :source source
              :index index
              :worktree worktree))
      (magit-file-status "--untracked-files=all"))
     (lambda (a b)
       (string< (n/diff-review--entry-path a)
                (n/diff-review--entry-path b))))))

(defun n/diff-review--entry-for-file (file)
  "Return the status entry for FILE."
  (seq-find (lambda (entry)
              (string= file (n/diff-review--entry-path entry)))
            n/diff-review--files))

(defun n/diff-review--file-paths ()
  "Return all changed file paths."
  (mapcar #'n/diff-review--entry-path n/diff-review--files))

(defun n/diff-review--set-selected-file (file)
  "Select FILE in the review UI and redraw visible panes."
  (setq n/diff-review--selected-file file)
  (n/diff-review--render-tree)
  (n/diff-review--render-diffs file))

(defun n/diff-review--choose-selected-file (&optional preferred)
  "Choose a selected file, preferring PREFERRED when it still exists."
  (or (and preferred
           (member preferred (n/diff-review--file-paths))
           preferred)
      (and n/diff-review--selected-file
           (member n/diff-review--selected-file (n/diff-review--file-paths))
           n/diff-review--selected-file)
      (n/diff-review--entry-path
       (or (seq-find #'n/diff-review--entry-unstaged-p n/diff-review--files)
           (car n/diff-review--files)))))

(defun n/diff-review--path-parts (path)
  "Split PATH into non-empty components."
  (split-string path "/" t))

(defun n/diff-review--dir-collapsed-p (dir)
  "Return non-nil when DIR is collapsed."
  (gethash dir n/diff-review--collapsed-dirs))

(defun n/diff-review--toggle-dir (dir)
  "Toggle collapse state for DIR."
  (puthash dir
           (not (n/diff-review--dir-collapsed-p dir))
           n/diff-review--collapsed-dirs))

(defun n/diff-review--insert-tree-line (text node path &optional entry face)
  "Insert a tree line with TEXT and properties NODE, PATH, ENTRY, and FACE."
  (let ((start (point)))
    (insert text "\n")
    (add-text-properties
     start (point)
     `(n/diff-review-node ,node
       n/diff-review-path ,path
       n/diff-review-entry ,entry
       mouse-face highlight
       help-echo "RET/l: show  s: stage file  u: unstage file  TAB: fold"))
    (let ((end (max start (1- (point)))))
      (when face
        (add-face-text-property start end face t))
      (when (and entry
                 n/diff-review--selected-file
                 (string= path n/diff-review--selected-file))
        (add-face-text-property start end
                                'n/diff-review-selected-face t)))))

(defun n/diff-review--render-tree ()
  "Render the changed-file tree."
  (when n/diff-review--root
    (let ((buffer (get-buffer-create (n/diff-review--buffer-name "Files"))))
      (with-current-buffer buffer
        (unless (derived-mode-p 'n/diff-review-tree-mode)
          (n/diff-review-tree-mode))
        (setq-local default-directory n/diff-review--root)
        (let ((inhibit-read-only t)
              (printed-dirs (make-hash-table :test 'equal))
              (target n/diff-review--selected-file)
              (target-pos nil))
          (erase-buffer)
          (insert (propertize (format "%s\n" n/diff-review--root)
                              'face 'n/diff-review-root-face))
          (insert "\n")
          (if (not n/diff-review--files)
              (insert "No changed files\n")
            (dolist (entry n/diff-review--files)
              (let* ((path (n/diff-review--entry-path entry))
                     (parts (n/diff-review--path-parts path))
                     (dirs (butlast parts))
                     (base (car (last parts)))
                     (prefix "")
                     (hidden nil)
                     (depth 0))
                (dolist (dir dirs)
                  (setq prefix (concat prefix dir "/"))
                  (unless (or hidden (gethash prefix printed-dirs))
                    (puthash prefix t printed-dirs)
                    (n/diff-review--insert-tree-line
                     (format "%s%s %s"
                             (make-string (* 2 depth) ?\s)
                             (if (n/diff-review--dir-collapsed-p prefix)
                                 "[+]"
                               "[-]")
                             dir)
                     'dir prefix nil 'n/diff-review-directory-face))
                  (when (n/diff-review--dir-collapsed-p prefix)
                    (setq hidden t))
                  (setq depth (1+ depth)))
                (unless hidden
                  (let ((line-start (point)))
                    (n/diff-review--insert-tree-line
                     (format "%s[%s] %s"
                             (make-string (* 2 depth) ?\s)
                             (propertize (n/diff-review--entry-status-string entry)
                                         'face (n/diff-review--entry-face entry))
                             base)
                     'file path entry nil)
                    (when (and target (string= target path))
                      (setq target-pos line-start)))))))
          (goto-char (or target-pos (point-min))))))))

(defun n/diff-review--display-in-selected-window (buffer)
  "Display BUFFER in the selected window and return that window."
  (set-window-buffer (selected-window) buffer)
  (selected-window))

(defun n/diff-review--setup-magit-diff-buffer (role file window)
  "Create or refresh ROLE diff buffer for FILE in WINDOW."
  (let* ((role-name (pcase role
                      ('staged "Index")
                      ('unstaged "Worktree")))
         (buffer-name (n/diff-review--buffer-name role-name))
         (typearg (pcase role
                    ('staged "--cached")
                    ('unstaged nil)))
         (diff-type role))
    (with-selected-window window
      (let ((default-directory n/diff-review--root)
            (magit-display-buffer-noselect t)
            (magit-display-buffer-function
             #'n/diff-review--display-in-selected-window)
            (magit-diff-refine-hunk 'all)
            (magit-diff-fontify-hunk t)
            (diff-font-lock-syntax t))
        (let ((buffer
               (magit-setup-buffer #'magit-diff-mode nil
                 :buffer buffer-name
                 :directory n/diff-review--root
                 (magit-buffer-diff-range nil)
                 (magit-buffer-diff-typearg typearg)
                 (magit-buffer-diff-type diff-type)
                 (magit-buffer-diff-args n/diff-review-diff-arguments)
                 (magit-buffer-diff-files (and file (list file)))
                 (magit-buffer-diff-files-suspended nil))))
          (with-current-buffer buffer
            (setq-local n/diff-review--buffer-role role)
            (setq-local n/diff-review--buffer-file file)
            (setq-local magit-diff-refine-hunk 'all)
            (setq-local magit-diff-fontify-hunk t)
            (setq-local diff-font-lock-syntax t)
            (n/diff-review-diff-mode 1))
          (set-window-buffer window buffer)
          buffer)))))

(defun n/diff-review--empty-pane (role window message)
  "Show MESSAGE in ROLE pane WINDOW."
  (let ((buffer (get-buffer-create (n/diff-review--buffer-name role))))
    (with-current-buffer buffer
      (let ((inhibit-read-only t))
        (special-mode)
        (setq-local default-directory (or n/diff-review--root default-directory))
        (erase-buffer)
        (insert message "\n")))
    (set-window-buffer window buffer)))

(defun n/diff-review--render-diffs (file)
  "Render staged and unstaged diff panes for FILE."
  (if (and file
           (window-live-p n/diff-review--staged-window)
           (window-live-p n/diff-review--unstaged-window))
      (progn
        (n/diff-review--setup-magit-diff-buffer
         'staged file n/diff-review--staged-window)
        (n/diff-review--setup-magit-diff-buffer
         'unstaged file n/diff-review--unstaged-window))
    (when (and (window-live-p n/diff-review--staged-window)
               (window-live-p n/diff-review--unstaged-window))
      (n/diff-review--empty-pane "Index" n/diff-review--staged-window
                                 "No changed files")
      (n/diff-review--empty-pane "Worktree" n/diff-review--unstaged-window
                                 "No changed files"))))

(defun n/diff-review--ensure-layout ()
  "Create the review window layout."
  (unless n/diff-review--previous-window-configuration
    (setq n/diff-review--previous-window-configuration
          (current-window-configuration)))
  (delete-other-windows)
  (let* ((frame-width (frame-width))
         (sidebar-width
          (max 24
               (min n/diff-review-sidebar-width
                    (max 24 (/ frame-width 3)))))
         (tree-window (selected-window))
         (right-window (split-window-right sidebar-width))
         unstaged-window)
    (setq n/diff-review--tree-window tree-window)
    (setq n/diff-review--staged-window right-window)
    (select-window right-window)
    (setq unstaged-window (split-window-right))
    (setq n/diff-review--unstaged-window unstaged-window)
    (set-window-parameter tree-window 'n/diff-review-role 'tree)
    (set-window-parameter right-window 'n/diff-review-role 'staged)
    (set-window-parameter unstaged-window 'n/diff-review-role 'unstaged)
    (set-window-buffer tree-window
                       (get-buffer-create (n/diff-review--buffer-name "Files")))
    (select-window n/diff-review--tree-window)))

(defun n/diff-review-refresh (&optional keep-window)
  "Refresh the review UI.
When KEEP-WINDOW is non-nil, preserve the selected window."
  (interactive)
  (unless n/diff-review--root
    (setq n/diff-review--root (n/diff-review--root)))
  (let ((selected-window (selected-window))
        (preferred n/diff-review--selected-file))
    (setq n/diff-review--files (n/diff-review--status-entries)
          n/diff-review--selected-file
          (n/diff-review--choose-selected-file preferred))
    (n/diff-review--render-tree)
    (n/diff-review--render-diffs n/diff-review--selected-file)
    (when (and keep-window (window-live-p selected-window))
      (select-window selected-window))
    (unless n/diff-review--files
      (message "No changed files"))))

(defun n/diff-review--schedule-refresh (&rest _)
  "Schedule a review refresh after Magit applies a change."
  (when (and n/diff-review--root
             (or (get-buffer (n/diff-review--buffer-name "Files"))
                 (get-buffer (n/diff-review--buffer-name "Index"))
                 (get-buffer (n/diff-review--buffer-name "Worktree"))))
    (when (timerp n/diff-review--refresh-timer)
      (cancel-timer n/diff-review--refresh-timer))
    (setq n/diff-review--refresh-timer
          (run-at-time 0.05 nil
                       (lambda ()
                         (setq n/diff-review--refresh-timer nil)
                         (when n/diff-review--root
                           (n/diff-review-refresh t)))))))

(add-hook 'magit-after-apply-functions #'n/diff-review--schedule-refresh)

(defun n/diff-review ()
  "Open a fast staged/unstaged review layout."
  (interactive)
  (setq n/diff-review--root (n/diff-review--root))
  (unless (hash-table-p n/diff-review--collapsed-dirs)
    (setq n/diff-review--collapsed-dirs (make-hash-table :test 'equal)))
  (n/diff-review--ensure-layout)
  (n/diff-review-refresh t)
  (when (window-live-p n/diff-review--tree-window)
    (select-window n/diff-review--tree-window)))

(defun n/diff-review-quit ()
  "Quit the review layout and restore the previous window configuration."
  (interactive)
  (when (timerp n/diff-review--refresh-timer)
    (cancel-timer n/diff-review--refresh-timer)
    (setq n/diff-review--refresh-timer nil))
  (if (window-configuration-p n/diff-review--previous-window-configuration)
      (progn
        (set-window-configuration n/diff-review--previous-window-configuration)
        (setq n/diff-review--previous-window-configuration nil))
    (quit-window))
  (setq n/diff-review--tree-window nil
        n/diff-review--staged-window nil
        n/diff-review--unstaged-window nil))

(defun n/diff-review--context-file ()
  "Return the current review file from point or buffer state."
  (or (get-text-property (point) 'n/diff-review-path)
      n/diff-review--buffer-file
      n/diff-review--selected-file))

(defun n/diff-review--stage-file (file)
  "Stage all changes in FILE."
  (let ((default-directory n/diff-review--root)
        (this-command 'magit-stage-files))
    (magit-stage-files (list file))))

(defun n/diff-review--unstage-file (file)
  "Unstage all changes in FILE."
  (let ((default-directory n/diff-review--root)
        (this-command 'magit-unstage-files))
    (magit-unstage-files (list file))))

(defun n/diff-review--next-entry-after (file predicate)
  "Return the next entry after FILE matching PREDICATE."
  (let* ((files n/diff-review--files)
         (paths (mapcar #'n/diff-review--entry-path files))
         (idx (or (cl-position file paths :test #'string=) -1))
         (after (nthcdr (1+ idx) files))
         (before (cl-subseq files 0 (max 0 (1+ idx)))))
    (seq-find predicate (append after before))))

(defun n/diff-review--advance-after-stage (old-file)
  "Advance after staging changes from OLD-FILE."
  (unless (and old-file
               (let ((entry (n/diff-review--entry-for-file old-file)))
                 (and entry (n/diff-review--entry-unstaged-p entry))))
    (when-let* ((next (n/diff-review--next-entry-after
                       old-file #'n/diff-review--entry-unstaged-p)))
      (n/diff-review--set-selected-file
       (n/diff-review--entry-path next)))))

(defun n/diff-review-stage-dwim ()
  "Stage the hunk at point, or the current file when point is not on a hunk."
  (interactive)
  (let ((file (n/diff-review--context-file))
        (role n/diff-review--buffer-role)
        (selected-window (selected-window)))
    (condition-case err
        (call-interactively #'magit-stage)
      (user-error
       (if (and file (eq role 'unstaged))
           (n/diff-review--stage-file file)
         (signal (car err) (cdr err)))))
    (n/diff-review-refresh t)
    (n/diff-review--advance-after-stage file)
    (when (window-live-p selected-window)
      (select-window selected-window))))

(defun n/diff-review-unstage-dwim ()
  "Unstage the hunk at point, or the current file when point is not on a hunk."
  (interactive)
  (let ((file (n/diff-review--context-file))
        (role n/diff-review--buffer-role)
        (selected-window (selected-window)))
    (condition-case err
        (call-interactively #'magit-unstage)
      (user-error
       (if (and file (eq role 'staged))
           (n/diff-review--unstage-file file)
         (signal (car err) (cdr err)))))
    (n/diff-review-refresh t)
    (when (window-live-p selected-window)
      (select-window selected-window))))

(defun n/diff-review-stage-file (&optional advance)
  "Stage the current file.
With ADVANCE non-nil, select the next file with unstaged changes."
  (interactive)
  (let ((file (n/diff-review--context-file)))
    (unless file
      (user-error "No file at point"))
    (n/diff-review--stage-file file)
    (n/diff-review-refresh t)
    (when advance
      (n/diff-review--advance-after-stage file))))

(defun n/diff-review-stage-file-and-next ()
  "Stage the current file and select the next file."
  (interactive)
  (n/diff-review-stage-file t))

(defun n/diff-review-unstage-file ()
  "Unstage the current file."
  (interactive)
  (let ((file (n/diff-review--context-file)))
    (unless file
      (user-error "No file at point"))
    (n/diff-review--unstage-file file)
    (n/diff-review-refresh t)))

(defun n/diff-review--goto-file-line (direction)
  "Move point to the next file line in DIRECTION."
  (let ((start (point))
        found wrapped done)
    (while (and (not found) (not done))
      (forward-line direction)
      (when (or (eobp) (bobp))
        (if wrapped
            (setq done t)
          (setq wrapped t)
          (if (> direction 0)
              (goto-char (point-min))
            (goto-char (point-max)))))
      (when (= (point) start)
        (setq done t))
      (when (eq (get-text-property (point) 'n/diff-review-node) 'file)
        (setq found t)))
    found))

(defun n/diff-review-select-file ()
  "Select the file at point in the tree."
  (interactive)
  (pcase (get-text-property (point) 'n/diff-review-node)
    ('file
     (n/diff-review--set-selected-file
      (get-text-property (point) 'n/diff-review-path)))
    ('dir
     (n/diff-review-toggle-node))
    (_
     (user-error "No changed file at point"))))

(defun n/diff-review-next-file ()
  "Move to the next changed file and preview it."
  (interactive)
  (when n/diff-review--files
    (if (n/diff-review--goto-file-line 1)
        (n/diff-review-select-file)
      (message "No visible changed files"))))

(defun n/diff-review-previous-file ()
  "Move to the previous changed file and preview it."
  (interactive)
  (when n/diff-review--files
    (if (n/diff-review--goto-file-line -1)
        (n/diff-review-select-file)
      (message "No visible changed files"))))

(defun n/diff-review-toggle-node ()
  "Toggle the directory at point, or select the file at point."
  (interactive)
  (pcase (get-text-property (point) 'n/diff-review-node)
    ('dir
     (n/diff-review--toggle-dir
      (get-text-property (point) 'n/diff-review-path))
     (n/diff-review--render-tree))
    ('file
     (n/diff-review-select-file))
    (_
     (user-error "No tree node at point"))))

(defun n/diff-review-focus-tree ()
  "Focus the changed-file tree."
  (interactive)
  (if (window-live-p n/diff-review--tree-window)
      (select-window n/diff-review--tree-window)
    (user-error "No diff review tree window")))

(defun n/diff-review-focus-staged ()
  "Focus the staged/index pane."
  (interactive)
  (if (window-live-p n/diff-review--staged-window)
      (select-window n/diff-review--staged-window)
    (user-error "No diff review index window")))

(defun n/diff-review-focus-unstaged ()
  "Focus the unstaged/worktree pane."
  (interactive)
  (if (window-live-p n/diff-review--unstaged-window)
      (select-window n/diff-review--unstaged-window)
    (user-error "No diff review worktree window")))

(defun n/diff-review-focus-left ()
  "Move focus one review pane to the left."
  (interactive)
  (let ((role (window-parameter (selected-window) 'n/diff-review-role)))
    (pcase role
      ('unstaged (n/diff-review-focus-staged))
      ('staged (n/diff-review-focus-tree))
      (_ (n/diff-review-focus-tree)))))

(defun n/diff-review-focus-right ()
  "Move focus one review pane to the right."
  (interactive)
  (let ((role (window-parameter (selected-window) 'n/diff-review-role)))
    (pcase role
      ('tree (n/diff-review-focus-staged))
      ('staged (n/diff-review-focus-unstaged))
      (_ (n/diff-review-focus-unstaged)))))

(defvar-keymap n/diff-review-tree-mode-map
  :parent special-mode-map
  "g" #'n/diff-review-refresh
  "r" #'n/diff-review-refresh
  "q" #'n/diff-review-quit
  "j" #'n/diff-review-next-file
  "k" #'n/diff-review-previous-file
  "n" #'n/diff-review-next-file
  "p" #'n/diff-review-previous-file
  "RET" #'n/diff-review-select-file
  "TAB" #'n/diff-review-toggle-node
  "s" #'n/diff-review-stage-file
  "S" #'n/diff-review-stage-file-and-next
  "u" #'n/diff-review-unstage-file
  "h" #'n/diff-review-focus-left
  "l" #'n/diff-review-focus-right)

(define-derived-mode n/diff-review-tree-mode special-mode "Diff Review"
  "Major mode for the changed-file tree in `n/diff-review'."
  (setq-local truncate-lines t)
  (setq-local cursor-type nil))

(defvar-keymap n/diff-review-diff-mode-map
  "g" #'n/diff-review-refresh
  "r" #'n/diff-review-refresh
  "q" #'n/diff-review-quit
  "s" #'n/diff-review-stage-dwim
  "S" #'n/diff-review-stage-file-and-next
  "u" #'n/diff-review-unstage-dwim
  "U" #'n/diff-review-unstage-file
  "h" #'n/diff-review-focus-left
  "l" #'n/diff-review-focus-right
  "H" #'n/diff-review-focus-tree)

(define-minor-mode n/diff-review-diff-mode
  "Minor mode for Magit diff panes managed by `n/diff-review'."
  :lighter " Review"
  :keymap n/diff-review-diff-mode-map)

(with-eval-after-load 'evil
  (evil-set-initial-state 'n/diff-review-tree-mode 'normal)
  (evil-define-key 'normal n/diff-review-tree-mode-map
    "g" #'n/diff-review-refresh
    "r" #'n/diff-review-refresh
    "q" #'n/diff-review-quit
    "j" #'n/diff-review-next-file
    "k" #'n/diff-review-previous-file
    "n" #'n/diff-review-next-file
    "p" #'n/diff-review-previous-file
    (kbd "RET") #'n/diff-review-select-file
    (kbd "TAB") #'n/diff-review-toggle-node
    "s" #'n/diff-review-stage-file
    "S" #'n/diff-review-stage-file-and-next
    "u" #'n/diff-review-unstage-file
    "h" #'n/diff-review-focus-left
    "l" #'n/diff-review-focus-right)
  (evil-define-key 'normal n/diff-review-diff-mode-map
    "g" #'n/diff-review-refresh
    "r" #'n/diff-review-refresh
    "q" #'n/diff-review-quit
    "s" #'n/diff-review-stage-dwim
    "S" #'n/diff-review-stage-file-and-next
    "u" #'n/diff-review-unstage-dwim
    "U" #'n/diff-review-unstage-file
    "h" #'n/diff-review-focus-left
    "l" #'n/diff-review-focus-right
    "H" #'n/diff-review-focus-tree))

(provide 'diff)

;;; diff.el ends here
