;;; llm.el --- my llm-related customizations  -*- lexical-binding: t; -*-

;; gptel
;; troubleshooting:
;; Instead of `gptel-send`, use `M-x gptel-send C-u M-x gptel--inspect-query RET`.
;; (setq gptel-log-level debug)
(require 'transient)

(use-package gptel
  :ensure
  :defer
  :bind (:map gptel-mode-map
         ("C-c C-n" . gptel-next-response)
         ("C-c C-p" . gptel-previous-response)
         ("C-c M-n" . gptel-end-of-response)
         ("C-c M-p" . gptel-beginning-of-response))
  :config

  (defun gptel-next-response (&optional arg)
    "Move to the beginning of the next gptel response."
    (interactive "p")
    (unless arg (setq arg 1))
    (let ((pos (point)))
      (when (gptel--in-response-p)
        (gptel-end-of-response nil nil 1))
      (if (text-property-search-forward 'gptel 'response t)
          (progn
            (gptel-beginning-of-response nil nil 1)
            (when (> arg 1)
              (gptel-next-response (1- arg))))
        (goto-char pos)
        (message "No more responses"))))

  (defun gptel-previous-response (&optional arg)
    "Move to the beginning of the previous gptel response."
    (interactive "p")
    (unless arg (setq arg 1))
    (let ((pos (point)))
      (when (gptel--in-response-p)
        (gptel-beginning-of-response nil nil 1))
      (if (text-property-search-backward 'gptel 'response t)
          (progn
            (gptel-beginning-of-response nil nil 1)
            (when (> arg 1)
              (gptel-previous-response (1- arg))))
        (goto-char pos)
        (message "No previous responses"))))

  (defun gptel--imenu-create-index ()
    "Create an imenu index for gptel responses."
    (let ((index nil) (count 0))
      (save-excursion
        (goto-char (point-min))
        (while (text-property-search-forward 'gptel 'response t)
          (let ((beg (prop-match-beginning (text-property-search-match))))
            (setq count (1+ count))
            (push (cons (format "Response %d" count) beg) index))))
      (nreverse index)))

  (add-hook 'gptel-mode-hook
            (lambda ()
              (setq-local imenu-create-index-function #'gptel--imenu-create-index)))
  
  (setq gptel-confirm-tool-calls 't)

  (setq n/gptel-tabbyAPI-models
    '((Qwen2.5-Coder-32B-Instruct-exl2-8bpw-8hb) ;; add more options here
      (QwQ-32B-exl2-8bpw-8hb :capabilities (nosystem tool-use reasoning))
      (QwQ-32B-exl2-6bpw-6hb :capabilities (nosystem tool-use reasoning))
      ))

  (setq n/gptel-tabbyAPI
    (gptel-make-openai "tabbyAPI"
      :host (getenv "LOCAL_API_URL")
      :protocol "http"
      :endpoint "/v1/chat/completions"
      :stream t
      :key (getenv "TABBYAPI_KEY")
      :models n/gptel-tabbyAPI-models))

  (setq n/gptel-openrouter-models
    '(
      (google/gemini-2.5-flash
       :description "Google's workhorse model"
       :capabilities (media tool-use cache reasoning)
       :mime-types ("image/jpeg" "image/png" "image/gif" "image/webp" "application/pdf")
       :context-window 1000000
       )
      (google/gemini-2.5-pro
       :description "Google's top model with thinking"
       :capabilities (media tool-use cache reasoning)
       :mime-types ("image/jpeg" "image/png" "image/gif" "image/webp" "application/pdf")
       :context-window 1000000
       )
      (deepseek/deepseek-r1-0528
       :capabilities (tool-use cache reasoning)
       :context-window 128000
       )
      (anthropic/claude-sonnet-4.5
       :capabilities (tool-use media cache)
       :mime-types ("image/jpeg" "image/png" "image/gif" "image/webp" "application/pdf")
       :context-window 1000000
       )
      (moonshotai/kimi-k2-0905
       :capabilities (tool-use reasoning)
       :context-window 256000
       )
      (qwen/qwen3-vl-235b-a22b-instruct
       :capabilities (tool-use reasoning cache)
       :context-window 131072
       )
      (qwen/qwen3-235b-a22b-thinking-2507
       :capabilities (tool-use reasoning cache)
       :context-window 131072
       )
      (x-ai/grok-4
       :capabilities (tool-use media cache)
       :context-window 256000
       :mime-types ("image/jpeg" "image/png" "image/gif" "image/webp" "application/pdf")
       )
      ))

  (setq n/gptel-openrouter
    (gptel-make-openai "openrouter"
      :host "openrouter.ai"
      :endpoint "/api/v1/chat/completions"
      :stream t
      :key (getenv "OPENROUTER_KEY")
      :models n/gptel-openrouter-models))

  (setq n/gptel-llamacpp
    (gptel-make-openai "llamacpp"
      :host "192.168.1.160:8000"
      :protocol "http"
      :endpoint "/v1/chat/completions"
      :stream t
      :key "asdf"
      :models '((Qwen3-32B :capabilities (nosystem tool-use reasoning))
		(Mistral-Small :capabilities (tool-use))
		)
    ))

  ;; add "us." prefix to enable cross-region inference (required for claude!)
  (setq gptel-bedrock--model-ids
	;; https://docs.aws.amazon.com/bedrock/latest/userguide/models-supported.html
	'((claude-sonnet-4-20250514    . "us.anthropic.claude-sonnet-4-20250514-v1:0")
	  (claude-opus-4-20250514      . "us.anthropic.claude-opus-4-20250514-v1:0"))
	)

  (setq n/gptel-bedrock
	(gptel-make-bedrock "bedrock"
	  :region "us-east-1"
	  :stream t
	  ))

  (setq gptel-temperature nil) ; do not override any temperature settings on the backend!!

  (defun n/gptel-set-temperature ()
    "Prompt the user for a number and set gptel-temperature to that number.
If the user inputs 'nil' or presses Enter without input, set gptel-temperature to nil."
    (interactive)
    (let ((input (read-string "Enter temperature (or 'nil' to unset): " nil nil nil)))
      (if (or (string= input "nil") (string-empty-p input))
	  (setq gptel-temperature nil)
	(let ((temp (string-to-number input)))
	  (if (numberp temp)
	      (setq gptel-temperature temp)
	    (message "Invalid input. Please enter a number or 'nil'."))))))
  
  (setopt gptel-model 'moonshotai/kimi-k2-0905
	  gptel-backend n/gptel-openrouter)

  ;; Get rid of the default backend, it clutters up the model selection
  (delete (assoc "ChatGPT" gptel--known-backends) gptel--known-backends)

  ;; Use my system prompts
  (setq gptel-directives
	`((default . "You are a helpful assistant.")
	  ))
  (setq gptel--system-message (alist-get 'default gptel-directives))

  ;; auto scroll and auto-move cursor
  (add-hook 'gptel-post-response-functions #'gptel-end-of-response)
  ;(add-hook 'gptel-post-stream-hook 'gptel-auto-scroll)

  ;; TODO: YOU CAN CHANGE THE TRANSIENT MENU BINDINGS!!!
  ;(transient-suffix-put 'gptel-menu (kbd "-m") :key "M")

  ;; Need to add custom system prompts and change the default.

  (setq gptel-default-mode 'markdown-mode)

  (defun n/gptel-display-strategy (buffer alist)
    "My custom window display strategy for gptel.
If there's only one window, split it to show BUFFER.
If there are multiple windows, use the selected one."
    (if (one-window-p t)
        ;; Rule 1: If only one window is open, split it horizontally
        ;; and display the gptel buffer in the new window below.
        (display-buffer buffer alist)
      ;; Rule 2: If more than one window is open, display the gptel
      ;; buffer in the currently active window.
      (display-buffer-same-window buffer alist)))

  ;; 2. Set gptel to use your custom function
  (setq gptel-display-buffer-action '(n/gptel-display-strategy))

  (defun n/gptel-request-wrapper (&rest args)
    "Wrapper around `gptel-request` that let-binds `gptel-backend', `gptel-model',
`gptel-use-tools', and `gptel-use-context' to isolate requests.

Args can be passed as keyword arguments:
:backend - the value for `gptel-backend', specifying the backend service
:model - the value for `gptel-model', specifying the language model
:use-tools - the value for `gptel-use-tools', controlling tool usage
:use-context - the value for `gptel-use-context', defining context handling

Any remaining arguments are passed to `gptel-request', including the optional
PROMPT and keyword arguments like :callback, :buffer, :position, etc.

This function ensures that asynchronous calls to multiple models do not interfere
by isolating the specified parameters for each request."
    (let* ((backend (plist-get args :backend))
           (model (plist-get args :model))
           (use-tools (plist-get args :use-tools))
           (use-context (plist-get args :use-context))
           ;; Remove our keyword args from the args list
           (gptel-args (let ((arg-copy (copy-sequence args)))
			 (remf arg-copy :backend)
			 (remf arg-copy :model)
			 (remf arg-copy :use-tools)
			 (remf arg-copy :use-context)
			 arg-copy)))
      (let ((gptel-backend (or backend gptel-backend))
            (gptel-model (or model gptel-model))
            (gptel-use-tools (or use-tools gptel-use-tools))
            (gptel-use-context (or use-context gptel-use-context)))
	(apply #'gptel-request gptel-args))))


;;;###autoload
  (defun n/gptel-switch-model ()
    "Switch GPT backend and model with completion support."
    (interactive)
    (let* ((models-alist
	    (cl-loop
	     for (name . backend) in gptel--known-backends
	     nconc (cl-loop for model in (gptel-backend-models backend)
			    collect (list (concat name ":" (gptel--model-name model))
					  backend model))
	     into models-alist
	     finally return models-alist))
	   (completion-extra-properties
	    `(:annotation-function
	      ,(lambda (comp)
		 (let* ((model-data (assoc comp models-alist))
			(model (nth 2 model-data))
			(desc (get model :description))
			(caps (get model :capabilities))
			(context (get model :context-window))
			(input-cost (get model :input-cost))
			(output-cost (get model :output-cost))
			(cutoff (get model :cutoff-date)))
		   (when (or desc caps context input-cost output-cost cutoff)
		     (concat
		      (propertize " " 'display `(space :align-to 40))
		      (when desc (truncate-string-to-width desc 70 nil ? t t))
		      " " (propertize " " 'display `(space :align-to 112))
		      (when caps (truncate-string-to-width (prin1-to-string caps) 21 nil ? t t))
		      " " (propertize " " 'display `(space :align-to 134))
		      (when context (format "%5dk" context))
		      " " (propertize " " 'display `(space :align-to 142))
		      (when input-cost (format "$%5.2f in" input-cost))
		      (if (and input-cost output-cost) "," " ")
		      " " (propertize " " 'display `(space :align-to 153))
		      (when output-cost (format "$%6.2f out" output-cost))
		      " " (propertize " " 'display `(space :align-to 166))
		      cutoff))))))
	   (default-model (concat (gptel-backend-name gptel-backend) ":"
				  (gptel--model-name gptel-model)))
	   (selected (completing-read "Select GPT model: " models-alist nil t nil nil default-model))
	   (selected-data (assoc selected models-alist)))
      (when selected-data
	(let ((backend (nth 1 selected-data))
	      (model (nth 2 selected-data)))
	  (setq gptel-backend backend
		gptel-model model)
	  (message "Switched to %s" selected)))))

  ;; get rid of prefix and change face of gptel responses instead
  (setf (alist-get 'markdown-mode gptel-prompt-prefix-alist) "")
  (setf (alist-get 'text-mode gptel-prompt-prefix-alist) "")
  )

;;; llm.el ends here


;; ---------------------------------------------------------------------------
;;  gptel-fringe – fringe indicators for response/reasoning lines
;; ---------------------------------------------------------------------------
(defvar-local gptel--fringe-overlays nil
  "List of fringe overlays in current buffer for cleanup.")

;; Define custom fringe bitmaps that fill the entire line height
(define-fringe-bitmap 'gptel-fringe-bar
  (vector #b01100000
          #b01100000
          #b01100000
          #b01100000
          #b01100000
          #b01100000
          #b01100000
          #b01100000
          #b01100000
          #b01100000
          #b01100000
          #b01100000
          #b01100000
          #b01100000
          #b01100000
          #b01100000
          #b01100000
          #b01100000
          #b01100000
          #b01100000
          #b01100000
          #b01100000
          #b01100000
          #b01100000
          #b01100000
          #b01100000
          #b01100000
          #b01100000)
  nil nil 'center)

(defun gptel--fringe--make-prefix (type)
  "Create fringe prefix string for TYPE ('response or 'ignore)."
  (propertize " " 'display
              `(left-fringe gptel-fringe-bar
                           ,(pcase type
                              ('response 'outline-1)
                              ('ignore 'outline-2)
                              (_ 'outline-1)))))

(defun gptel--fringe--refresh (beg end)
  "JIT-lock function: mark lines containing gptel response/reasoning.
BEG and END delimit the region to refresh."
  (save-excursion
    (save-restriction
      (widen)  ; Ensure we see the whole buffer
      (goto-char beg)
      (beginning-of-line)
      (setq beg (point))

      ;; Extend end to include the full last line
      (goto-char end)
      (end-of-line)
      (setq end (min (1+ (point)) (point-max)))

      ;; Process region by property changes for efficiency
      (goto-char beg)
      (let ((pos beg))
        (while (< pos end)
          (let* ((next-change (or (next-single-property-change pos 'gptel nil end) end))
                 (val (get-text-property pos 'gptel)))
            (when (memq val '(response ignore))
              ;; Process each line in this property region
              (save-excursion
                (goto-char pos)
                (while (and (< (point) next-change) (< (point) end))
                  (let* ((bol (line-beginning-position))
                         (eol (line-end-position))
                         ;; Overlay must extend past EOL to catch line-prefix
                         (overlay-end (min (1+ eol) (point-max)))
                         ;; Check for existing overlay at BOL
                         (existing-ov (cl-find-if
                                      (lambda (ov)
                                        (and (overlay-get ov 'gptel-fringe)
                                             (= (overlay-start ov) bol)))
                                      (overlays-in bol (min (+ bol 2) (point-max))))))
                    (if existing-ov
                        ;; Update existing overlay
                        (progn
                          (move-overlay existing-ov bol overlay-end)
                          ;; Update prefix in case type changed
                          (let ((prefix (gptel--fringe--make-prefix val)))
                            (overlay-put existing-ov 'line-prefix prefix)
                            (overlay-put existing-ov 'wrap-prefix prefix)))
                      ;; Create new overlay
                      (let ((ov (make-overlay bol overlay-end nil t nil))
                            (prefix (gptel--fringe--make-prefix val)))
                        (overlay-put ov 'gptel-fringe t)
                        (overlay-put ov 'gptel-fringe-type val)
                        (overlay-put ov 'line-prefix prefix)
                        (overlay-put ov 'wrap-prefix prefix)
                        (overlay-put ov 'evaporate t)
                        (push ov gptel--fringe-overlays))))
                  (forward-line 1))))
            (setq pos next-change)))))
  
  ;; Return proper jit-lock bounds (list, not cons)
  (list 'jit-lock-bounds beg end)))

(defun gptel--fringe--clear ()
  "Remove all gptel fringe overlays in current buffer."
  (mapc #'delete-overlay gptel--fringe-overlays)
  (setq gptel--fringe-overlays nil)
  ;; Also remove any orphaned overlays
  (remove-overlays (point-min) (point-max) 'gptel-fringe t))

(defun gptel--fringe--cleanup-removed-properties (beg end)
  "Remove fringe overlays where gptel property was removed.
Called after text changes to clean up stale overlays."
  (dolist (ov (overlays-in beg end))
    (when (overlay-get ov 'gptel-fringe)
      (let ((ov-start (overlay-start ov)))
        ;; Check if overlay still covers text with gptel property
        (unless (and ov-start
                     (memq (get-text-property ov-start 'gptel)
                           '(response ignore)))
          (delete-overlay ov)
          (setq gptel--fringe-overlays
                (delq ov gptel--fringe-overlays)))))))

(defun gptel--fringe--force-refresh ()
  "Force refresh of all fringe indicators."
  (when gptel-fringe-mode
    (gptel--fringe--clear)
    (jit-lock-refontify (point-min) (point-max))))

(defun gptel--fringe--stream-update ()
  "Update fringe indicators during streaming response.
Uses jit-lock for efficient incremental updates."
  (when gptel-fringe-mode
    ;; Let jit-lock handle the refresh efficiently
    ;; This will trigger gptel--fringe--refresh for visible regions
    (jit-lock-refontify)))

(defun gptel--fringe--after-change (beg end _len)
  "After-change function to clean up stale overlays.
BEG and END delimit the changed region."
  (when gptel-fringe-mode
    ;; Clean up any overlays in changed region that lost their property
    (gptel--fringe--cleanup-removed-properties beg end)))

(defun gptel--fringe--kill-buffer-cleanup ()
  "Clean up fringe overlays when buffer is killed."
  (when gptel-fringe-mode
    (gptel--fringe--clear)))

(define-minor-mode gptel-fringe-mode
  "Show fringe indicators for AI response/reasoning lines.

This mode adds visual indicators in the left fringe for lines
that contain AI responses or internal reasoning. Lines with the
'response property show in one color, 'ignore in another.

The indicators appear on both the main line and any visual
continuation lines when text wraps."
  :lighter " ⊞"
  (if gptel-fringe-mode
      (progn
        ;; Register with jit-lock for efficient fontification
        (jit-lock-register #'gptel--fringe--refresh)
        
        ;; Initial refresh of entire buffer
        (gptel--fringe--refresh (point-min) (point-max))
        
        ;; Hook into streaming updates
        (add-hook 'gptel-post-stream-hook #'gptel--fringe--stream-update nil t)
        
        ;; Clean up stale overlays after changes
        (add-hook 'after-change-functions #'gptel--fringe--after-change nil t)
        
        ;; Clean up on buffer kill
        (add-hook 'kill-buffer-hook #'gptel--fringe--kill-buffer-cleanup nil t))
    
    ;; Cleanup when mode is disabled
    (jit-lock-unregister #'gptel--fringe--refresh)
    (remove-hook 'gptel-post-stream-hook #'gptel--fringe--stream-update t)
    (remove-hook 'after-change-functions #'gptel--fringe--after-change t)
    (remove-hook 'kill-buffer-hook #'gptel--fringe--kill-buffer-cleanup t)
    (gptel--fringe--clear)))

(add-hook 'gptel-mode-hook #'gptel-fringe-mode)

;(gptel-make-preset 'thepreset
;  :description "default" :backend "openrouter" :model
;  'anthropic/claude-sonnet-4.5 :system 'default :tools 'nil :stream t
;  :temperature nil :max-tokens nil :use-context 'user :track-media nil
;  :include-reasoning 'ignore)
