;;; diff.el --- fast index/worktree Ediff review UI  -*- lexical-binding: t; -*-

;;; Commentary:
;; A focused review layout for moving quickly through changed files:
;; changed-file tree on the left, and a Magit/Ediff index-vs-worktree
;; session on the right for the selected file.

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'subr-x)
(require 'magit)
(require 'magit-apply)
(require 'magit-ediff)
(require 'ediff)
(require 'ediff-wind)

(declare-function ediff-really-quit "ediff-util" (reverse-default-keep-variants))
(declare-function ediff-setup-control-buffer "ediff-util" (control-buffer))
(declare-function evil-define-key "evil-core" (state keymaps key def &rest bindings))
(declare-function evil-set-initial-state "evil-core" (mode state))

(defgroup n/diff-review nil
  "Fast index/worktree review UI."
  :group 'magit)

(defcustom n/diff-review-sidebar-width 36
  "Width of the changed-file tree in `n/diff-review'."
  :type 'integer
  :group 'n/diff-review)

(defcustom n/diff-review-ediff-control-window-height 8
  "Height of the Ediff control window in `n/diff-review'."
  :type 'integer
  :group 'n/diff-review)

(defcustom n/diff-review-sync-scroll t
  "When non-nil, synchronize direct scrolling between Ediff A and B panes."
  :type 'boolean
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
(defvar n/diff-review--visible-files nil)
(defvar n/diff-review--selected-file nil)
(defvar n/diff-review--tree-window nil)
(defvar n/diff-review--staged-window nil)
(defvar n/diff-review--unstaged-window nil)
(defvar n/diff-review--control-window nil)
(defvar n/diff-review--previous-window-configuration nil)
(defvar n/diff-review--collapsed-dirs (make-hash-table :test 'equal))
(defvar n/diff-review--refresh-timer nil)
(defvar n/diff-review--tree-selection-overlay nil)
(defvar n/diff-review--ediff-control-buffer nil)
(defvar n/diff-review--ediff-index-buffer nil)
(defvar n/diff-review--ediff-worktree-buffer nil)
(defvar n/diff-review--ediff-file nil)
(defvar n/diff-review--ediff-cleanup-in-progress nil)
(defvar n/diff-review--syncing-scroll nil)

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

(defun n/diff-review--visible-file-paths ()
  "Return changed file paths currently visible in the tree."
  (or n/diff-review--visible-files
      (n/diff-review--file-paths)))

(defun n/diff-review--tree-buffer ()
  "Return the changed-file tree buffer."
  (get-buffer-create (n/diff-review--buffer-name "Files")))

(defun n/diff-review--mark-buffer-clean ()
  "Mark the current review buffer as generated, not user-modified."
  (setq-local buffer-offer-save nil)
  (setq-local kill-buffer-query-functions nil)
  (set-buffer-modified-p nil))

(defun n/diff-review--update-tree-selection ()
  "Move the tree selection overlay to `n/diff-review--selected-file'."
  (when-let* ((buffer (get-buffer (n/diff-review--buffer-name "Files"))))
    (with-current-buffer buffer
      (unless (overlayp n/diff-review--tree-selection-overlay)
        (setq n/diff-review--tree-selection-overlay
              (make-overlay (point-min) (point-min) buffer nil t))
        (overlay-put n/diff-review--tree-selection-overlay
                     'face 'n/diff-review-selected-face)
        (overlay-put n/diff-review--tree-selection-overlay
                     'evaporate t))
      (let ((found nil)
            (pos (point-min))
            (end (point-max)))
        (while (and (not found) (< pos end))
          (let ((path (get-text-property pos 'n/diff-review-path)))
            (when (and path
                       n/diff-review--selected-file
                       (string= path n/diff-review--selected-file)
                       (eq (get-text-property pos 'n/diff-review-node) 'file))
              (setq found pos)))
          (setq pos (or (next-single-property-change
                         pos 'n/diff-review-path nil end)
                        end)))
        (if found
            (move-overlay n/diff-review--tree-selection-overlay
                          (save-excursion
                            (goto-char found)
                            (line-beginning-position))
                          (save-excursion
                            (goto-char found)
                            (line-end-position)))
          (delete-overlay n/diff-review--tree-selection-overlay))))))

(defun n/diff-review--tree-position-for-file (file)
  "Return the tree buffer position for FILE, when visible."
  (when-let* ((buffer (get-buffer (n/diff-review--buffer-name "Files"))))
    (with-current-buffer buffer
      (let ((found nil)
            (pos (point-min))
            (end (point-max)))
        (while (and (not found) (< pos end))
          (when (and (eq (get-text-property pos 'n/diff-review-node) 'file)
                     (equal file (get-text-property pos 'n/diff-review-path)))
            (setq found pos))
          (setq pos (or (next-single-property-change
                         pos 'n/diff-review-path nil end)
                        end)))
        found))))

(defun n/diff-review--goto-tree-file (file)
  "Move the tree point/window to FILE when it is visible."
  (when-let* ((buffer (get-buffer (n/diff-review--buffer-name "Files")))
              (pos (n/diff-review--tree-position-for-file file)))
    (with-current-buffer buffer
      (goto-char pos))
    (when (and (window-live-p n/diff-review--tree-window)
               (eq (window-buffer n/diff-review--tree-window) buffer))
      (set-window-point n/diff-review--tree-window pos)
      (with-current-buffer buffer
        (set-window-start
         n/diff-review--tree-window
         (save-excursion
           (goto-char pos)
           (line-beginning-position))
         t)))))

(defun n/diff-review--set-selected-file (file)
  "Select FILE in the review UI and start its Ediff session."
  (let ((origin-role (window-parameter (selected-window) 'n/diff-review-role)))
    (setq n/diff-review--selected-file file)
    (n/diff-review--update-tree-selection)
    (n/diff-review--render-diffs file)
    (n/diff-review--update-tree-selection)
    (n/diff-review--goto-tree-file file)
    (pcase origin-role
      ('control
       (if (n/diff-review--ediff-live-p)
           (n/diff-review-focus-control)
         (n/diff-review-focus-tree)))
      ('staged (n/diff-review-focus-staged))
      ('unstaged (n/diff-review-focus-unstaged))
      (_ nil))))

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
        (add-face-text-property start end face t)))))

(defun n/diff-review--render-tree ()
  "Render the changed-file tree."
  (when n/diff-review--root
    (let ((buffer (n/diff-review--tree-buffer)))
      (with-current-buffer buffer
        (unless (derived-mode-p 'n/diff-review-tree-mode)
          (n/diff-review-tree-mode))
        (setq-local default-directory n/diff-review--root)
        (let ((inhibit-read-only t)
              (printed-dirs (make-hash-table :test 'equal))
              visible-files
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
                    (push path visible-files)
                    (when (and target (string= target path))
                      (setq target-pos line-start)))))))
          (setq n/diff-review--visible-files (nreverse visible-files))
          (goto-char (or target-pos (point-min)))
          (n/diff-review--update-tree-selection)
          (n/diff-review--mark-buffer-clean))))))

(defun n/diff-review--empty-pane (role window message)
  "Show MESSAGE in ROLE pane WINDOW."
  (when (window-live-p window)
    (let ((buffer (get-buffer-create (n/diff-review--buffer-name role))))
      (with-current-buffer buffer
        (let ((inhibit-read-only t))
          (special-mode)
          (setq-local default-directory (or n/diff-review--root default-directory))
          (erase-buffer)
          (insert message "\n")
          (n/diff-review--mark-buffer-clean)))
      (set-window-buffer window buffer))))

(defun n/diff-review--worktree-binary-file-p (file)
  "Return non-nil when FILE appears to be a binary worktree file."
  (let ((expanded (expand-file-name file n/diff-review--root)))
    (and (file-regular-p expanded)
         (with-temp-buffer
           (insert-file-contents-literally expanded nil 0 8192)
           (goto-char (point-min))
           (search-forward "\0" nil t)))))

(defun n/diff-review--numstat-binary-line-p (line)
  "Return non-nil when Git numstat LINE describes a binary diff."
  (string-prefix-p "-\t-" line))

(defun n/diff-review--binary-diff-p (file)
  "Return non-nil when FILE has a binary staged or worktree diff."
  (let ((default-directory n/diff-review--root))
    (or (seq-some #'n/diff-review--numstat-binary-line-p
                  (magit-git-lines "diff" "--numstat" "--" file))
        (seq-some #'n/diff-review--numstat-binary-line-p
                  (magit-git-lines "diff" "--cached" "--numstat" "--" file)))))

(defun n/diff-review--ediff-unavailable-message (file)
  "Return a message when FILE should not be opened with Ediff."
  (let ((entry (n/diff-review--entry-for-file file)))
    (cond
     ((and entry (n/diff-review--entry-untracked-p entry))
      (cond
       ((not (file-regular-p (expand-file-name file n/diff-review--root)))
        (format "Untracked non-regular file: %s\n\nEdiff review is available for regular text files.\nUse `s' in the tree to stage the whole file." file))
       ((n/diff-review--worktree-binary-file-p file)
        (format "Untracked binary file: %s\n\nEdiff review is available for text files.\nUse `s' in the tree to stage the whole file." file))))
     ((or (n/diff-review--worktree-binary-file-p file)
          (n/diff-review--binary-diff-p file))
      (format "Binary file: %s\n\nEdiff cannot review binary diffs.\nUse `s' or `u' in the tree for whole-file staging changes." file)))))

(defun n/diff-review--ediff-live-p ()
  "Return non-nil when an Ediff session is active for this review."
  (buffer-live-p n/diff-review--ediff-control-buffer))

(defun n/diff-review--sync-scroll (window start)
  "Synchronize the other Ediff pane with WINDOW at START."
  (when (and n/diff-review-sync-scroll
             (not n/diff-review--syncing-scroll)
             (memq (window-parameter window 'n/diff-review-role)
                   '(staged unstaged)))
    (let ((other (pcase (window-parameter window 'n/diff-review-role)
                   ('staged n/diff-review--unstaged-window)
                   ('unstaged n/diff-review--staged-window))))
      (when (and (window-live-p other)
                 (buffer-live-p (window-buffer other)))
        (let ((line (with-current-buffer (window-buffer window)
                      (line-number-at-pos start t)))
              (hscroll (window-hscroll window)))
          (setq n/diff-review--syncing-scroll t)
          (unwind-protect
              (with-current-buffer (window-buffer other)
                (save-excursion
                  (goto-char (point-min))
                  (forward-line (max 0 (1- line)))
                  (set-window-start other (point) t)
                  (set-window-hscroll other hscroll)))
            (setq n/diff-review--syncing-scroll nil)))))))

(defun n/diff-review--prepare-ediff-buffer (buffer role file)
  "Attach review metadata to Ediff BUFFER for ROLE and FILE."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (setq-local default-directory n/diff-review--root)
      (setq-local truncate-lines t)
      (setq-local n/diff-review--buffer-role role)
      (setq-local n/diff-review--buffer-file file)
      (add-hook 'window-scroll-functions #'n/diff-review--sync-scroll nil t))))

(defun n/diff-review--prepare-ediff-control-buffer (buffer file)
  "Attach review metadata and keys to Ediff control BUFFER for FILE."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (setq-local default-directory n/diff-review--root)
      (setq-local n/diff-review--buffer-role 'control)
      (setq-local n/diff-review--buffer-file file)
      (n/diff-review-control-mode 1))))

(defun n/diff-review--teardown-ediff-buffer (buffer)
  "Remove review metadata from Ediff BUFFER."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (remove-hook 'window-scroll-functions #'n/diff-review--sync-scroll t)
      (kill-local-variable 'n/diff-review--buffer-role)
      (kill-local-variable 'n/diff-review--buffer-file))))

(defun n/diff-review--teardown-ediff-control-buffer (buffer)
  "Remove review metadata and keys from Ediff control BUFFER."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (when (bound-and-true-p n/diff-review-control-mode)
        (n/diff-review-control-mode -1))
      (kill-local-variable 'n/diff-review--buffer-role)
      (kill-local-variable 'n/diff-review--buffer-file))))

(defun n/diff-review--ediff-window-setup
    (buffer-a buffer-b buffer-c control-buffer)
  "Set up Ediff windows with the review tree on the left."
  (if buffer-c
      (ediff-setup-windows-plain buffer-a buffer-b buffer-c control-buffer)
    (with-current-buffer control-buffer
      (setq ediff-multiframe nil))
    (when (window-live-p n/diff-review--tree-window)
      (select-window n/diff-review--tree-window))
    (delete-other-windows)
    (set-window-dedicated-p (selected-window) nil)
    (let* ((window-min-height 1)
           (window-combination-resize t)
           (frame-width (frame-width))
           (sidebar-width
            (max 24
                 (min n/diff-review-sidebar-width
                      (max 24 (/ frame-width 3)))))
           (tree-window (selected-window))
           (right-window (split-window-right sidebar-width))
           (tree-buffer (n/diff-review--tree-buffer)))
      (set-window-buffer tree-window tree-buffer)
      (set-window-parameter tree-window 'n/diff-review-role 'tree)
      (select-window right-window)
      (let* ((total-height (window-total-height right-window))
             (control-height
              (max 3
                   (min n/diff-review-ediff-control-window-height
                        (max 3 (/ total-height 4)))))
             (control-window
              (when (> total-height (+ control-height 6))
                (split-window right-window
                              (- total-height control-height)
                              'below)))
             (window-a right-window)
             window-b)
        (set-window-buffer window-a buffer-a)
        (setq window-b (split-window window-a nil 'right))
        (set-window-buffer window-b buffer-b)
        (set-window-parameter window-a 'n/diff-review-role 'staged)
        (set-window-parameter window-b 'n/diff-review-role 'unstaged)
        (setq n/diff-review--tree-window tree-window
              n/diff-review--staged-window window-a
              n/diff-review--unstaged-window window-b)
        (with-current-buffer control-buffer
          (setq ediff-window-A window-a
                ediff-window-B window-b
                ediff-window-C nil))
        (if (window-live-p control-window)
            (progn
              (set-window-buffer control-window control-buffer)
              (set-window-parameter control-window 'n/diff-review-role 'control)
              (setq n/diff-review--control-window control-window)
              (select-window control-window)
              (ediff-setup-control-buffer control-buffer))
          (setq n/diff-review--control-window
                (display-buffer control-buffer)))))))

(defun n/diff-review--untracked-index-buffer-name (file)
  "Return the generated empty-index buffer name for untracked FILE."
  (format "*Diff Review Empty Index: %s: %s*"
          (if n/diff-review--root
              (n/diff-review--repo-name n/diff-review--root)
            "repo")
          file))

(defun n/diff-review--make-untracked-index-buffer (file worktree-buffer)
  "Return an empty writable index BUFFER for untracked FILE."
  (let ((buffer (get-buffer-create
                 (n/diff-review--untracked-index-buffer-name file)))
        (mode (buffer-local-value 'major-mode worktree-buffer)))
    (with-current-buffer buffer
      (let ((inhibit-read-only t))
        (erase-buffer))
      (setq-local default-directory n/diff-review--root)
      (setq-local buffer-read-only nil)
      (setq-local buffer-offer-save nil)
      (setq-local kill-buffer-query-functions nil)
      (when (and (symbolp mode)
                 (fboundp mode)
                 (not (eq mode major-mode)))
        (delay-mode-hooks
          (funcall mode)))
      (set-buffer-modified-p nil))
    buffer))

(defun n/diff-review--git-file-mode (file)
  "Return the Git index mode to use for FILE."
  (if (file-executable-p (expand-file-name file n/diff-review--root))
      "100755"
    "100644"))

(defun n/diff-review--update-index-from-buffer (buffer file)
  "Update the Git index for FILE with BUFFER contents."
  (let ((index-file (make-temp-name
                     (expand-file-name "diff-review-update-index-"
                                       (magit-gitdir)))))
    (magit-run-before-change-functions file "stage")
    (unwind-protect
        (progn
          (with-current-buffer buffer
            (let ((coding-system-for-write buffer-file-coding-system))
              (write-region (point-min) (point-max) index-file nil 'silent)))
          (magit-call-git
           "update-index" "--add" "--cacheinfo"
           (n/diff-review--git-file-mode file)
           (magit-git-string "hash-object" "-t" "blob" "-w"
                             (concat "--path=" file)
                             "--"
                             (magit-convert-filename-for-git index-file))
           file))
      (ignore-errors (delete-file index-file)))
    (with-current-buffer buffer
      (set-buffer-modified-p nil))
    (magit-run-after-apply-functions file "stage")))

(defun n/diff-review--kill-generated-buffer (buffer)
  "Kill generated BUFFER without modified-buffer prompts."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (setq-local buffer-offer-save nil)
      (setq-local kill-buffer-query-functions nil)
      (set-buffer-modified-p nil))
    (let ((kill-buffer-query-functions nil))
      (kill-buffer buffer))))

(defun n/diff-review--finish-index-buffer (buffer lock mode file untracked)
  "Restore BUFFER state and write modified index contents if needed.
LOCK and MODE are the buffer's pre-Ediff read-only and blob-mode state.
When UNTRACKED is non-nil, write BUFFER to the index as a new FILE."
  (when (buffer-live-p buffer)
    (when lock
      (with-current-buffer buffer
        (if mode
            (magit-blob-mode 1)
          (setq-local buffer-read-only t)
          (setq-local read-only-mode--state t))))
    (when (buffer-modified-p buffer)
      (if untracked
          (n/diff-review--update-index-from-buffer buffer file)
        (with-current-buffer buffer
          (cl-letf* ((orig-y-or-n-p (symbol-function 'y-or-n-p))
                     ((symbol-function 'y-or-n-p)
                      (lambda (prompt)
                        (if (string-prefix-p
                             "Update index with contents of " prompt)
                            t
                          (funcall orig-y-or-n-p prompt)))))
            (magit-update-index)))))))

(defun n/diff-review--clear-ediff-state ()
  "Clear the current Ediff session state."
  (setq n/diff-review--ediff-control-buffer nil
        n/diff-review--ediff-index-buffer nil
        n/diff-review--ediff-worktree-buffer nil
        n/diff-review--ediff-file nil
        n/diff-review--control-window nil))

(defun n/diff-review--cleanup-ediff-session ()
  "Quit the active Ediff session without interactive prompts."
  (when (and (n/diff-review--ediff-live-p)
             (not n/diff-review--ediff-cleanup-in-progress))
    (let ((control-buffer n/diff-review--ediff-control-buffer))
      (setq n/diff-review--ediff-cleanup-in-progress t)
      (unwind-protect
          (when (buffer-live-p control-buffer)
            (with-current-buffer control-buffer
              (let ((ediff-keep-variants t))
                (ediff-really-quit nil))))
        (setq n/diff-review--ediff-cleanup-in-progress nil))))
  (n/diff-review--clear-ediff-state))

(defun n/diff-review--start-ediff-stage (file)
  "Start a Magit Ediff index-vs-worktree session for FILE."
  (require 'magit-ediff)
  (magit-with-toplevel
    (let* ((default-directory n/diff-review--root)
           (entry (n/diff-review--entry-for-file file))
           (untracked (and entry
                           (n/diff-review--entry-untracked-p entry)))
           (buffer-b (magit-find-file-noselect "{worktree}" file t))
           ;; Match `n/magit-ediff-stage': keep encodings aligned so index
           ;; updates do not rewrite the file unexpectedly.
           (coding-system-for-read
            (buffer-local-value 'buffer-file-coding-system buffer-b))
           (buffer-a (if untracked
                         (n/diff-review--make-untracked-index-buffer
                          file buffer-b)
                       (magit-find-file-noselect "{index}" file t 'ediff)))
           (lock-a (buffer-local-value 'buffer-read-only buffer-a))
           (mode-a (buffer-local-value 'magit-blob-mode buffer-a)))
      (unless untracked
        (with-current-buffer buffer-a
          (magit-blob-mode -1)))
      (when (window-live-p n/diff-review--tree-window)
        (select-window n/diff-review--tree-window))
      (let ((old-window-setup-function ediff-window-setup-function)
            (old-split-window-function ediff-split-window-function)
            (old-keep-variants ediff-keep-variants)
            (old-auto-refine ediff-auto-refine)
            (old-highlight-all-diffs ediff-highlight-all-diffs))
        (cl-labels
            ((restore-ediff-globals
              ()
              (setq ediff-window-setup-function old-window-setup-function
                    ediff-split-window-function old-split-window-function
                    ediff-keep-variants old-keep-variants
                    ediff-auto-refine old-auto-refine
                    ediff-highlight-all-diffs old-highlight-all-diffs)))
          (setq ediff-window-setup-function #'ediff-setup-windows-plain
                ediff-split-window-function #'split-window-horizontally
                ediff-keep-variants t
                ediff-auto-refine 'on
                ediff-highlight-all-diffs t)
          (condition-case err
              (magit-ediff-buffers
               buffer-a
               buffer-b
               nil
               (lambda ()
                 (restore-ediff-globals)
                 (let ((control-buffer (current-buffer)))
                   (n/diff-review--ediff-window-setup
                    buffer-a buffer-b nil control-buffer)
                   (setq n/diff-review--ediff-control-buffer control-buffer
                         n/diff-review--ediff-index-buffer buffer-a
                         n/diff-review--ediff-worktree-buffer buffer-b
                         n/diff-review--ediff-file file)
                   (n/diff-review--prepare-ediff-buffer buffer-a 'staged file)
                   (n/diff-review--prepare-ediff-buffer buffer-b 'unstaged file)
                   (n/diff-review--prepare-ediff-control-buffer
                    control-buffer file)))
               (lambda ()
                 (restore-ediff-globals)
                 (n/diff-review--finish-index-buffer
                  buffer-a lock-a mode-a file untracked)
                 (n/diff-review--teardown-ediff-buffer buffer-a)
                 (n/diff-review--teardown-ediff-buffer buffer-b)
                 (n/diff-review--teardown-ediff-control-buffer
                  ediff-control-buffer)
                 (when untracked
                   (n/diff-review--kill-generated-buffer buffer-a))
                 (n/diff-review--clear-ediff-state)))
            (error
             (restore-ediff-globals)
             (signal (car err) (cdr err)))))))))

(defun n/diff-review--show-ediff-error (file error)
  "Display ERROR for FILE in the review panes."
  (when (not (and (window-live-p n/diff-review--staged-window)
                  (window-live-p n/diff-review--unstaged-window)))
    (n/diff-review--ensure-layout))
  (let ((message (format "Could not open Ediff for %s:\n%s"
                         file (error-message-string error))))
    (n/diff-review--empty-pane "Index" n/diff-review--staged-window message)
    (n/diff-review--empty-pane "Worktree" n/diff-review--unstaged-window
                               message)))

(defun n/diff-review--render-diffs (file &optional force)
  "Render FILE by starting an index/worktree Ediff session.
When FORCE is non-nil, restart the session even if FILE is already active."
  (cond
   ((not file)
    (n/diff-review--cleanup-ediff-session)
    (when (and (window-live-p n/diff-review--staged-window)
               (window-live-p n/diff-review--unstaged-window))
      (n/diff-review--empty-pane "Index" n/diff-review--staged-window
                                 "No changed files")
      (n/diff-review--empty-pane "Worktree" n/diff-review--unstaged-window
                                 "No changed files")))
   ((and (not force)
         (n/diff-review--ediff-live-p)
         (equal file n/diff-review--ediff-file))
    nil)
   (t
    (n/diff-review--cleanup-ediff-session)
    (if-let* ((message (n/diff-review--ediff-unavailable-message file)))
        (progn
          (unless (and (window-live-p n/diff-review--staged-window)
                       (window-live-p n/diff-review--unstaged-window))
            (n/diff-review--ensure-layout))
          (n/diff-review--empty-pane "Index" n/diff-review--staged-window
                                     message)
          (n/diff-review--empty-pane "Worktree" n/diff-review--unstaged-window
                                     message))
      (condition-case err
          (progn
            (n/diff-review--start-ediff-stage file)
            (when (window-live-p n/diff-review--tree-window)
              (select-window n/diff-review--tree-window)))
        (error
         (n/diff-review--show-ediff-error file err)))))))

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
    (set-window-buffer tree-window (n/diff-review--tree-buffer))
    (select-window n/diff-review--tree-window)))

(defun n/diff-review-refresh (&optional keep-window force-diff)
  "Refresh the review UI.
When KEEP-WINDOW is non-nil, preserve the selected window.  When
FORCE-DIFF is non-nil, restart the selected file's Ediff session."
  (interactive (list nil t))
  (unless n/diff-review--root
    (setq n/diff-review--root (n/diff-review--root)))
  (let ((selected-window (selected-window))
        (preferred n/diff-review--selected-file))
    (setq n/diff-review--files (n/diff-review--status-entries)
          n/diff-review--selected-file
          (n/diff-review--choose-selected-file preferred))
    (n/diff-review--render-tree)
    (n/diff-review--render-diffs n/diff-review--selected-file force-diff)
    (when (and keep-window (window-live-p selected-window))
      (select-window selected-window))
    (unless n/diff-review--files
      (message "No changed files"))))

(defun n/diff-review--schedule-refresh (&rest _)
  "Schedule a review refresh after Magit applies a change."
  (when (and n/diff-review--root
             (or (get-buffer (n/diff-review--buffer-name "Files"))
                 (n/diff-review--ediff-live-p)))
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
  "Open a fast index/worktree Ediff review layout."
  (interactive)
  (setq n/diff-review--root (n/diff-review--root))
  (unless (hash-table-p n/diff-review--collapsed-dirs)
    (setq n/diff-review--collapsed-dirs (make-hash-table :test 'equal)))
  (n/diff-review--ensure-layout)
  (n/diff-review-refresh t t)
  (when (window-live-p n/diff-review--tree-window)
    (select-window n/diff-review--tree-window)))

(defun n/diff-review-quit ()
  "Quit the review layout and restore the previous window configuration."
  (interactive)
  (when (timerp n/diff-review--refresh-timer)
    (cancel-timer n/diff-review--refresh-timer)
    (setq n/diff-review--refresh-timer nil))
  (n/diff-review--cleanup-ediff-session)
  (if (window-configuration-p n/diff-review--previous-window-configuration)
      (progn
        (set-window-configuration n/diff-review--previous-window-configuration)
        (setq n/diff-review--previous-window-configuration nil))
    (quit-window))
  (setq n/diff-review--tree-window nil
        n/diff-review--staged-window nil
        n/diff-review--unstaged-window nil
        n/diff-review--control-window nil)
  (when (overlayp n/diff-review--tree-selection-overlay)
    (delete-overlay n/diff-review--tree-selection-overlay))
  (setq n/diff-review--tree-selection-overlay nil))

(defun n/diff-review--context-file ()
  "Return the current review file from point or buffer state."
  (or (get-text-property (point) 'n/diff-review-path)
      n/diff-review--buffer-file
      n/diff-review--selected-file))

(defun n/diff-review--stage-file (file)
  "Stage all changes in FILE."
  (n/diff-review--cleanup-ediff-session)
  (let ((default-directory n/diff-review--root)
        (this-command 'magit-stage-files))
    (magit-stage-files (list file))))

(defun n/diff-review--unstage-file (file)
  "Unstage all changes in FILE."
  (n/diff-review--cleanup-ediff-session)
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

(defun n/diff-review-stage-file (&optional advance)
  "Stage the current file.
With ADVANCE non-nil, select the next file with unstaged changes."
  (interactive)
  (let ((file (n/diff-review--context-file)))
    (unless file
      (user-error "No file at point"))
    (n/diff-review--stage-file file)
    (n/diff-review-refresh t t)
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
    (n/diff-review-refresh t t)))

(defun n/diff-review--adjacent-visible-file (direction)
  "Return the visible file DIRECTION steps from the selected file."
  (let* ((files (n/diff-review--visible-file-paths))
         (count (length files)))
    (when (> count 0)
      (let* ((current (or n/diff-review--selected-file
                          (car files)))
             (idx (or (cl-position current files :test #'string=)
                      (if (> direction 0) -1 0))))
        (nth (mod (+ idx direction) count) files)))))

(defun n/diff-review-select-file ()
  "Select the file at point in the tree."
  (interactive)
  (pcase (get-text-property (point) 'n/diff-review-node)
    ('file
     (n/diff-review--set-selected-file
      (get-text-property (point) 'n/diff-review-path))
     (n/diff-review-focus-control))
    ('dir
     (n/diff-review-toggle-node))
    (_
     (user-error "No changed file at point"))))

(defun n/diff-review-next-file ()
  "Move to the next changed file and preview it."
  (interactive)
  (if-let* ((file (n/diff-review--adjacent-visible-file 1)))
      (n/diff-review--set-selected-file file)
    (message "No visible changed files")))

(defun n/diff-review-previous-file ()
  "Move to the previous changed file and preview it."
  (interactive)
  (if-let* ((file (n/diff-review--adjacent-visible-file -1)))
      (n/diff-review--set-selected-file file)
    (message "No visible changed files")))

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
  "Focus the index pane."
  (interactive)
  (if (window-live-p n/diff-review--staged-window)
      (select-window n/diff-review--staged-window)
    (user-error "No diff review index window")))

(defun n/diff-review-focus-unstaged ()
  "Focus the worktree pane."
  (interactive)
  (if (window-live-p n/diff-review--unstaged-window)
      (select-window n/diff-review--unstaged-window)
    (user-error "No diff review worktree window")))

(defun n/diff-review-focus-control ()
  "Focus the Ediff control buffer."
  (interactive)
  (cond
   ((window-live-p n/diff-review--control-window)
    (select-window n/diff-review--control-window))
   ((buffer-live-p n/diff-review--ediff-control-buffer)
    (setq n/diff-review--control-window
          (display-buffer n/diff-review--ediff-control-buffer))
    (select-window n/diff-review--control-window))
   (t
    (user-error "No Ediff control buffer"))))

(defun n/diff-review-focus-left ()
  "Move focus one review pane to the left."
  (interactive)
  (let ((role (window-parameter (selected-window) 'n/diff-review-role)))
    (pcase role
      ('control (n/diff-review-focus-unstaged))
      ('unstaged (n/diff-review-focus-staged))
      ('staged (n/diff-review-focus-tree))
      (_ (n/diff-review-focus-tree)))))

(defun n/diff-review-focus-right ()
  "Move focus one review pane to the right."
  (interactive)
  (let ((role (window-parameter (selected-window) 'n/diff-review-role)))
    (pcase role
      ('control (n/diff-review-focus-unstaged))
      ('tree (n/diff-review-focus-staged))
      ('staged (n/diff-review-focus-unstaged))
      ('unstaged (n/diff-review-focus-control))
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
  "e" #'n/diff-review-focus-control
  "h" #'n/diff-review-focus-left
  "l" #'n/diff-review-focus-right)

(define-derived-mode n/diff-review-tree-mode special-mode "Diff Review"
  "Major mode for the changed-file tree in `n/diff-review'."
  (setq-local truncate-lines t)
  (setq-local cursor-type nil))

(defvar-keymap n/diff-review-control-mode-map
  "g" #'n/diff-review-refresh
  "r" #'n/diff-review-refresh
  "q" #'n/diff-review-quit
  "C-c C-n" #'n/diff-review-next-file
  "C-c C-p" #'n/diff-review-previous-file
  "C-c C-t" #'n/diff-review-focus-tree
  "C-c C-r" #'n/diff-review-refresh
  "C-c C-q" #'n/diff-review-quit)

(define-minor-mode n/diff-review-control-mode
  "Minor mode for the Ediff control buffer managed by `n/diff-review'."
  :lighter " Review"
  :keymap n/diff-review-control-mode-map)

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
    "e" #'n/diff-review-focus-control
    "h" #'n/diff-review-focus-left
    "l" #'n/diff-review-focus-right)
  (evil-define-key 'normal n/diff-review-control-mode-map
    "g" #'n/diff-review-refresh
    "r" #'n/diff-review-refresh
    "q" #'n/diff-review-quit
    "]f" #'n/diff-review-next-file
    "[f" #'n/diff-review-previous-file
    "T" #'n/diff-review-focus-tree))

(provide 'diff)

;;; diff.el ends here
