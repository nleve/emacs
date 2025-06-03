;;; llm.el --- my llm-related customizations

;; gptel
(use-package gptel
  :ensure
  :defer
  :config
  (setq gptel-confirm-tool-calls 't)

  ;; unclutter the claude models
  (defvar n/gptel-anthropic-models
	'((claude-3-7-sonnet-20250219
           :description "Hybrid model capable of standard thinking and extended thinking modes"
           :capabilities (media tool-use cache)
           :mime-types ("image/jpeg" "image/png" "image/gif" "image/webp" "application/pdf")
           :context-window 200
           :input-cost 3
           :output-cost 15
           :cutoff-date "2025-02")
          (claude-3-5-sonnet-20241022
           :description "Highest level of intelligence and capability"
           :capabilities (media tool-use cache)
           :mime-types ("image/jpeg" "image/png" "image/gif" "image/webp" "application/pdf")
           :context-window 200
           :input-cost 3
           :output-cost 15
           :cutoff-date "2024-04")))

  (defvar n/gptel-anthropic
    (gptel-make-anthropic "Claude"
      :stream t
      :key (getenv "CLAUDE_API_KEY")
      :models n/gptel-anthropic-models)
    )

  (defvar n/gptel-tabbyAPI-models
    '((Qwen2.5-Coder-32B-Instruct-exl2-8bpw-8hb) ;; add more options here
      (QwQ-32B-exl2-8bpw-8hb :capabilities (nosystem tool-use reasoning))
      (QwQ-32B-exl2-6bpw-6hb :capabilities (nosystem tool-use reasoning))
      ))

  (defvar n/gptel-tabbyAPI
    (gptel-make-openai "tabbyAPI"
      :host (getenv "LOCAL_API_URL")
      :protocol "http"
      :endpoint "/v1/chat/completions"
      :stream t
      :key (getenv "TABBYAPI_KEY")
      :models n/gptel-tabbyAPI-models))

  (defvar n/gptel-openrouter-models
    '(
      (google/gemini-2.5-flash-preview-05-20
       :description "Google's latest workhorse model"
       :capabilities (media tool-use cache)
       :mime-types ("image/jpeg" "image/png" "image/gif" "image/webp" "application/pdf")
       :context-window 1000000
       )
      (google/gemini-2.5-flash-preview-05-20:thinking
       :description "Google's latest workhorse model with advanced reasoning"
       :capabilities (media tool-use cache)
       :mime-types ("image/jpeg" "image/png" "image/gif" "image/webp" "application/pdf")
       :context-window 1000000
       )
      (google/gemini-2.5-pro-preview
       :description "Google's top model with thinking"
       :capabilities (media tool-use cache)
       :mime-types ("image/jpeg" "image/png" "image/gif" "image/webp" "application/pdf")
       :context-window 1000000
       )
      ))

  (defvar n/gptel-openrouter
    (gptel-make-openai "openrouter"
      :host "openrouter.ai"
      :endpoint "/api/v1/chat/completions"
      :stream t
      :key (getenv "OPENROUTER_KEY")
      :models n/gptel-openrouter-models))

  (defvar n/gptel-llamacpp
    (gptel-make-openai "llamacpp"
      :host "192.168.1.160:8000"
      :protocol "http"
      :endpoint "/v1/chat/completions"
      :stream t
      :key "asdf"
      :models '((Qwen3-32B :capabilities (nosystem tool-use reasoning))
		)
    ))

  (setq gptel-temperature nil) ; do not override any temperature settings on the backend!!
  
  (setopt gptel-model 'google/gemini-2.5-flash-preview-05-20:thinking
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

;;   (defun gptel-request-wrapper (backend model use-tools use-context &rest gptel-args)
;;     "Wrapper around `gptel-request` that let-binds `gptel-backend', `gptel-model',
;; `gptel-use-tools', and `gptel-use-context' to isolate requests.

;; BACKEND is the value for `gptel-backend', specifying the backend service.
;; MODEL is the value for `gptel-model', specifying the language model.
;; USE-TOOLS is the value for `gptel-use-tools', controlling tool usage.
;; USE-CONTEXT is the value for `gptel-use-context', defining context handling.

;; GPTEL-ARGS are the arguments to pass to `gptel-request', including the optional
;; PROMPT and keyword arguments like :callback, :buffer, :position, etc.

;; This function ensures that asynchronous calls to multiple models do not interfere
;; by isolating the specified parameters for each request."
;;     (let ((gptel-backend backend)
;;           (gptel-model model)
;;           (gptel-use-tools use-tools)
;;           (gptel-use-context use-context))
;;       (apply 'gptel-request gptel-args)))

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

  (defun n/gptel-context-insert-buffer-string (buffer contexts)
    "Insert at point a context string from all CONTEXTS in BUFFER."
    (dolist (context contexts)
      (let* ((start (overlay-start context))
	     (end (overlay-end context))
	     (content (with-current-buffer buffer
			(buffer-substring-no-properties start end)))
	     (start-line (with-current-buffer buffer
			   (line-number-at-pos start t)))
	     (end-line (with-current-buffer buffer
			 (line-number-at-pos end t)))
	     (mode-name (gptel--strip-mode-suffix 
			 (buffer-local-value 'major-mode buffer))))
	
	;; Insert header with filename and line numbers
	(insert (format "```%s %s:%d-%d\n"
			mode-name
			(buffer-name buffer)
			start-line
			end-line))
	
	;; Insert the content
	(insert content)
	
	;; Close the code block
	(insert "\n```\n\n"))))

  (defun n/gptel-context-insert-file-string (path)
    "Insert at point the contents of the file at PATH as context."
    (insert (format "%s:" (file-name-nondirectory path))
            "\n\n```\n")
    (insert-file-contents path)
    (goto-char (point-max))
    (insert "\n```\n"))

  (defun n/gptel-context-string (context-alist)
    "Format the aggregated gptel context as annotated markdown fragments.

Returns a string.  CONTEXT-ALIST is a structure containing
context overlays, see `gptel-context--alist'."
    (with-temp-buffer
      (cl-loop for (buf . ovs) in context-alist
               if (bufferp buf)
               do (n/gptel-context-insert-buffer-string buf ovs)
               else if (not (plist-get ovs :mime))
               do (n/gptel-context-insert-file-string buf) end
               do (insert "\n\n")
               finally do
               (skip-chars-backward "\n\t\r ")
               (delete-region (point) (point-max))
               (unless (bobp)
		 (goto-char (point-min))
		 (insert "Request context:\n\n"))
               finally return
               (and (> (buffer-size) 0)
                    (buffer-string)))))

  (defun n/gptel-context-wrap-function (message contexts)
    "Add CONTEXTS to MESSAGE.

MESSAGE is usually either the system message or the user prompt.
The accumulated context from CONTEXTS is appended or prepended to
it, respectively."
    ;; Append context before/after system message.
    (let ((context-string (gptel-context--string contexts)))
      (if (> (length context-string) 0)
          (pcase-exhaustive gptel-use-context
            ('system (concat message "\n\n" context-string))
            ('user   (concat context-string "\n\n" message))
            ('nil    message))
	message)))

  (setopt gptel-context-wrap-function #'n/gptel-context-wrap-function)
  )

;; get rid of prefix and change face of gptel responses instead
(setf (alist-get 'markdown-mode gptel-prompt-prefix-alist) "")
(setf (alist-get 'text-mode gptel-prompt-prefix-alist) "")

;; Change face of gptel responses
(defface gptel-response-face
  '((((background dark)  (min-colors 88)) :background "gray9" :extend t)
    (((background light) (min-colors 88)) :background "alice blue" :extend t)
    (t :inherit mode-line))
  "Face used to highlight gptel responses."
  :group 'custom-faces)

;; Font-lock matcher for 'gptel property
(defun gptel-font-lock-matcher (limit)
  "Font-lock matcher for text with 'gptel property up to LIMIT."
  (let (start end)
    (while (and (< (point) limit)
                ;; Match any non-nil 'gptel property
                (setq start (next-single-property-change (point) 'gptel nil limit))
                (get-text-property start 'gptel))
      (setq end (or (next-single-property-change start 'gptel nil limit) limit))
      (put-text-property start end 'font-lock-face 'gptel-response-face)
      (goto-char end))
    nil))

;; Add the matcher to font-lock globally
(add-hook 'font-lock-mode-hook
          (lambda ()
            (font-lock-add-keywords nil
                                    '((gptel-font-lock-matcher . gptel-response-face))
                                    'append)))

(add-hook 'gptel-post-stream-hook 'font-lock-update)



;;; llm.el ends here
