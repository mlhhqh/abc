;; -*- lexical-binding: t; -*-
(defun prot-simple-keyboard-quit-dwim ()
  "Do-What-I-Mean behaviour for a general `keyboard-quit'.

The generic `keyboard-quit' does not do the expected thing when
the minibuffer is open.  Whereas we want it to close the
minibuffer, even without explicitly focusing it.

The DWIM behaviour of this command is as follows:

- When the region is active, disable it.
- When a minibuffer is open, but not focused, close the minibuffer.
- When the Completions buffer is selected, close it.
- In every other case use the regular `keyboard-quit'."
  (interactive)
  (cond
   ((region-active-p)
    (keyboard-quit))
   ((derived-mode-p 'completion-list-mode)
    (delete-completion-window))
   ((> (minibuffer-depth) 0)
    (abort-recursive-edit))
   (t
    (keyboard-quit))))

(global-set-key (kbd "C-g") 'prot-simple-keyboard-quit-dwim)


(require 'gptel)
;; Llama.cpp offers an OpenAI compatible API
(gptel-make-openai "llama-cpp"          ;Any name
  :stream t                             ;Stream responses
  :protocol "http"
  :host "localhost:8081"                ;Llama.cpp server location
  :models '(test))                    ;Any names, doesn't matter for Llama
;; OPTIONAL configuration
(setq
 gptel-model   'test
 gptel-backend (gptel-make-openai "llama-cpp"
                 :stream t
                 :protocol "http"
                 :host "localhost:8081"
                 :models '(test)))

(gptel-make-ollama "Ollama"             ;Any name of your choosing
  :host "localhost:11434"               ;Where it's running
  :stream t                             ;Stream responses
  :models '(deepseek-r1-qwen-14b-q2:latest qwen3:14b qwen3:8b hf.co/unsloth/DeepSeek-R1-0528-Qwen3-8B-GGUF:IQ2_M))          ;List of models
(gptel-make-ollama "Ollama"             ;Any name of your choosing
  :host "localhost:11434"               ;Where it's running
  :stream t                             ;Stream responses
  :models '())
;; OPTIONAL configuration
(setq
 gptel-model 'gemma3:12b-it-qat
 gptel-backend (gptel-make-ollama "Ollama"
                 :host "localhost:11434"
                 :stream t
                 :models '(gemma3:12b-it-qat)))

(setq
 gptel-model 'qwen3:14b
 gptel-backend (gptel-make-ollama "Ollama"
                 :host "localhost:11434"
                 :stream t
                 :models '(qwen3:14b)))

(add-hook 'gptel-post-stream-hook 'gptel-auto-scroll)
