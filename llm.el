;;; llm.el --- my llm-related customizations  -*- lexical-binding: t; -*-

;; gptel
(require 'transient)

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
	  (claude-opus-4-20250514      . "us.anthropic.claude-opus-4-20250514-v1:0")
	  (claude-3-7-sonnet-20250219  . "us.anthropic.claude-3-7-sonnet-20250219-v1:0")
	  (claude-3-5-sonnet-20241022  . "us.anthropic.claude-3-5-sonnet-20241022-v2:0")
	  (claude-3-5-sonnet-20240620  . "us.anthropic.claude-3-5-sonnet-20240620-v1:0")
	  (claude-3-5-haiku-20241022   . "us.anthropic.claude-3-5-haiku-20241022-v1:0")
	  (claude-3-opus-20240229      . "us.anthropic.claude-3-opus-20240229-v1:0")
	  (claude-3-sonnet-20240229    . "us.anthropic.claude-3-sonnet-20240229-v1:0")
	  (claude-3-haiku-20240307     . "us.anthropic.claude-3-haiku-20240307-v1:0")
	  (mistral-7b                  . "mistral.mistral-7b-instruct-v0:2")
	  (mistral-8x7b                . "mistral.mixtral-8x7b-instruct-v0:1")
	  (mistral-large-2402          . "mistral.mistral-large-2402-v1:0")
	  (mistral-large-2407          . "mistral.mistral-large-2407-v1:0")
	  (mistral-small-2402          . "mistral.mistral-small-2402-v1:0")
	  (llama-3-8b                  . "meta.llama3-8b-instruct-v1:0")
	  (llama-3-70b                 . "meta.llama3-70b-instruct-v1:0")
	  (llama-3-1-8b                . "meta.llama3-1-8b-instruct-v1:0")
	  (llama-3-1-70b               . "meta.llama3-1-70b-instruct-v1:0")
	  (llama-3-1-405b              . "meta.llama3-1-405b-instruct-v1:0")
	  (llama-3-2-1b                . "meta.llama3-2-1b-instruct-v1:0")
	  (llama-3-2-3b                . "meta.llama3-2-3b-instruct-v1:0")
	  (llama-3-2-11b               . "meta.llama3-2-11b-instruct-v1:0")
	  (llama-3-2-90b               . "meta.llama3-2-90b-instruct-v1:0")
	  (llama-3-3-70b               . "meta.llama3-3-70b-instruct-v1:0"))
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

  ;; get rid of prefix and change face of gptel responses instead
  (setf (alist-get 'markdown-mode gptel-prompt-prefix-alist) "")
  (setf (alist-get 'text-mode gptel-prompt-prefix-alist) "")
  )

;;; llm.el ends here
