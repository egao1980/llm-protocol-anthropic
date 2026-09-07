# llm-protocol-anthropic

Anthropic **Messages** HTTP backend for [`llm-protocol`](https://github.com/egao1980/llm-protocol). Same class for `api.anthropic.com`, [vLLM](https://docs.vllm.ai/) `POST /v1/messages`, and [llama.cpp server](https://github.com/ggml-org/llama.cpp) `/v1/messages`. Not the protocol. Not CFFI `llm-backend-llama-cpp`.

Transport is `http-protocol` — bind [`http-backend-async`](https://github.com/egao1980/http-backend-async) × [`event-backend-libuv`](https://github.com/egao1980/event-backend-libuv).

```lisp
(asdf:load-system "llm-protocol-anthropic")
(asdf:load-system "event-backend-libuv")
(asdf:load-system "http-backend-async")

(setf http-backend-async:*event-backend-maker*
      #'event-backend-libuv:make-libuv-backend)
(setf http-protocol:*http-backend*
      (http-backend-async:make-async-backend))

(let ((b (stack-llm-anthropic:make-anthropic-backend)))
  (stack-llm:llm-response-text
   (stack-llm:generate b "ping" :settings '(:temperature 0 :max-tokens 32)))
  (stack-llm:stream-generate b "ping" :on-part #'print))
```

vLLM / llama-server (same wire, different base):

```lisp
(stack-llm-anthropic:make-vllm-anthropic-backend)          ; http://127.0.0.1:8000/v1
(stack-llm-anthropic:make-llama-server-anthropic-backend)  ; http://127.0.0.1:8080/v1
```

vLLM tools need `--enable-auto-tool-choice`. Official default: `https://api.anthropic.com/v1`, model `claude-sonnet-4-20250514`. Env: `ANTHROPIC_API_KEY` / `ANTHROPIC_AUTH_TOKEN` / `ANTHROPIC_BASE_URL` / `ANTHROPIC_MODEL` / `ANTHROPIC_VERSION`. Headers: `x-api-key` **and** `Authorization: Bearer` (vLLM wants bearer) + `anthropic-version: 2023-06-01`. `max_tokens` is required — default **4096**.

System turns → top-level `system`. Tool turns → user `tool_result` blocks. Thinking + `signature` round-trip on `llm-thinking-part`. `tool_choice`: `:auto` / `:none` / `:required` → `any` / string → `{type:tool,name}`. `list-models` `GET /models`; **404 → default model**. `backend-supports-p`: `:tools` `:stream` `:vision` `:thinking`. No `:embeddings`. Default `respond` = items→turns→`generate`.

Register in the protocol catalog (llm-protocol **0.2.1**):

```lisp
(let ((cat (stack-llm:make-in-memory-provider-catalog))
      (b (stack-llm-anthropic:make-anthropic-backend)))
  (stack-llm:register-provider cat "anthropic" b)
  (stack-llm:register-provider cat "vllm"
                               (stack-llm-anthropic:make-vllm-anthropic-backend))
  (stack-llm:resolve-backend cat "anthropic:claude-sonnet-4-20250514"))
```

Tests inject `request-fn`. Live: bind HTTP + set `ANTHROPIC_API_KEY`. CI needs published `llm-protocol` on GHCR.

Part of [cl-stack](https://github.com/egao1980/cl-stack) ([#195](https://github.com/egao1980/cl-stack/issues/195)).

## License

MIT — see [LICENSE](LICENSE).
