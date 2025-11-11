;;; llm.el --- my llm-related customizations  -*- lexical-binding: t; -*-

;; gptel
;; troubleshooting:
;; Instead of `gptel-send`, use `M-x gptel-send C-u M-x gptel--inspect-query RET`.
;; (setq gptel-log-level debug)
(require 'transient)

(use-package gptel
  :ensure
  :bind (:map gptel-mode-map
         ("C-c C-n" . gptel-next-response)
         ("C-c C-p" . gptel-previous-response)
         ("C-c M-n" . gptel-end-of-response)
         ("C-c M-p" . gptel-beginning-of-response))
  :config


  (add-hook 'gptel-mode-hook #'gptel-highlight-mode)

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
       :request-params '(:thinking (:type "enabled" :budget_tokens 2048))
       :context-window 1000000
       )
      (moonshotai/kimi-k2-0905
       :capabilities (tool-use reasoning)
       :context-window 256000
       )
      (moonshotai/kimi-k2-thinking
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
      (z-ai/glm-4.6
       :capabilities (tool-use cache reasoning)
       :context-window 200000
       )
      (openai/gpt-5
       :capabilities (tool-use cache reasoning media)
       :mime-types ("image/jpeg" "image/png" "image/gif" "image/webp" "application/pdf")
       :context-window 400000
      )
      (openai/gpt-5-codex
       :capabilities (tool-use cache reasoning media)
       :mime-types ("image/jpeg" "image/png" "image/gif" "image/webp" "application/pdf")
       :context-window 400000
      ))
    )

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
  (setq gptel-include-reasoning 'ignore)
  (setq gptel-highlight-methods '(fringe))

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
  
  (setopt gptel-model 'anthropic/claude-sonnet-4.5
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

(use-package ragmacs
   :ensure
   :vc (:url "https://github.com/positron-solutions/ragmacs"
	    :rev :newest)
   :after gptel
   :defer
   :init
   (gptel-make-preset 'introspect
     :pre (lambda () (require 'ragmacs))
     :system
     "You are pair programming with the user in Emacs and on Emacs.
 
 Your job is to dive into Elisp code and understand the APIs and
 structure of elisp libraries and Emacs.  Use the provided tools to do
 so, but do not make duplicate tool calls for information already
 available in the chat.
 
 <tone>
 1. Be terse and to the point.  Speak directly.
 2. Explain your reasoning.
 3. Do NOT hedge or qualify.
 4. If you don't know, say you don't know.
 5. Do not offer unprompted advice or clarifications.
 6. Never apologize.
 7. Do NOT summarize your answers.
 </tone>
 
 <code_generation>
 When generating code:
 1. Always check that functions or variables you use in your code exist.
 2. Also check their calling convention and function-arity before you use them.
 3. Write code that can be tested by evaluation, and offer to evaluate
 code using the `elisp_eval` tool.
 </code_generation>
 
 <formatting>
 1. When referring to code symbols (variables, functions, tags etc) enclose them in markdown quotes.
    Examples: `read_file`, `getResponse(url, callback)`
    Example: `<details>...</details>`
 2. If you use LaTeX notation, enclose math in \( and \), or \[ and \] delimiters.
 </formatting>"
     :tools '("introspection")))

;;; llm.el ends here

;(gptel-make-preset 'thepreset
;  :description "default" :backend "openrouter" :model
;  'anthropic/claude-sonnet-4.5 :system 'default :tools 'nil :stream t
;  :temperature nil :max-tokens nil :use-context 'user :track-media nil
;  :include-reasoning 'ignore)
