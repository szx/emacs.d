
(request "http://localhost:8888/api/health"
  :parser 'json-read
  :success (cl-function
            (lambda (&key data &allow-other-keys)
              (message "I sent: %S"  data))))


(setq gptel-model 'test
      gptel-backend
      (gptel-make-openai "llama-cpp"          ;Any name
        :stream t                             ;Stream responses
        :protocol "http"
        :host "localhost:36243"                ;Llama.cpp server location
        :models '((test :capabilities (media tool json url)
                        :mime-types
                        ("image/jpeg" "image/png" "image/gif")))))

(make-comint "llama-server" "/home/sszczyrb/.unsloth/llama.cpp/build/bin/llama-server" nil "-m" "/home/sszczyrb/.cache/huggingface/hub/models--unsloth--qwen3.5-4b-gguf/snapshots/e87f176479d0855a907a41277aca2f8ee7a09523/Qwen3.5-4B-UD-Q4_K_XL.gguf" "--port" "36243" "-c" "0" "--parallel" "1" "--flash-attn" "on" "--jinja" "--chat-template-kwargs" "{\"enable_thinking\": false}" "--mmproj" "/home/sszczyrb/.cache/huggingface/hub/models--unsloth--qwen3.5-4b-gguf/snapshots/e87f176479d0855a907a41277aca2f8ee7a09523/mmproj-BF16.gguf")

