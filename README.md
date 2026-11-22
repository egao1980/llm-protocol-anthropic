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

vLLM / llama-server (same class, **`:dialect :compat`**):

```lisp
(stack-llm-anthropic:make-vllm-anthropic-backend)          ; http://127.0.0.1:8000/v1
(stack-llm-anthropic:make-llama-server-anthropic-backend)  ; http://127.0.0.1:8080/v1
```

Compat **flattens** Anthropic-native tools (`web_search_20260209`, …) to `{name, input_schema}` — vLLM / llama-server do not speak official tool types. Server-side `server_tool_use` is rewritten to `tool_use` on the way out. `:native-tools` is **official only**. vLLM tools need `--enable-auto-tool-choice`. Official default: `https://api.anthropic.com/v1`, model `claude-sonnet-4-20250514`. Env: `ANTHROPIC_API_KEY` / `ANTHROPIC_AUTH_TOKEN` / `ANTHROPIC_BASE_URL` / `ANTHROPIC_MODEL` / `ANTHROPIC_VERSION` / `ANTHROPIC_BETA`. Headers: `x-api-key` **and** `Authorization: Bearer` (vLLM wants bearer) + `anthropic-version: 2023-06-01`. `max_tokens` is required — default **4096**.

**Timeouts** (seconds; `nil`, `0` or env `none` = no limit). Left out, each reads its env var, then the default:

| Initarg | Env | Default | Bounds |
|---|---|---|---|
| `:request-timeout` | `ANTHROPIC_TIMEOUT` | 600 | a non-streaming request, in all (the server says nothing until the whole answer is ready) |
| `:stream-idle-timeout` | `ANTHROPIC_STREAM_IDLE_TIMEOUT` | 300 | silence on a streaming response. Streams have no total cap: one that keeps producing tokens runs to the end |
| `:connect-timeout` | `ANTHROPIC_CONNECT_TIMEOUT` | 30 | TCP + TLS setup |

They map onto `http-protocol` phases: `(:connect C :read IDLE)` for streams and `(:connect C :total T)` otherwise. The read (idle) phase needs `http-backend-async` 0.2.25 or later. A timeout signals `http-protocol:http-timeout-error` ("read timed out: no data for N s …"). It is not retried: model calls are POSTs, and `http-retry` only retries idempotent methods. A custom `:request-fn` owns its own timeouts. Readers: `anthropic-request-timeout`, `anthropic-stream-idle-timeout`, `anthropic-connect-timeout`.

**API key** (`:api-key`): left out, or `:env`, reads `ANTHROPIC_API_KEY` then `ANTHROPIC_AUTH_TOKEN`. `nil`, `:none` or `""` means no key: no `x-api-key` or `Authorization` header, and the environment is not read. A string or an `http-protocol:secret` is used as given. The backend keeps the key as a `secret`, and both header values are secrets, so backtraces, condition reports and `describe` print `#<SECRET [redacted]>`. `anthropic-api-key` returns the plain string. A custom `:request-fn` gets header values that may be secrets: call `http-protocol:reveal` on them.

Native tools (`make-web-search-tool`, `:bash`, …): official wire uses Anthropic `type`s; client tools (`bash` / `text_editor` / `computer` / `memory`) → you execute `tool_use`; server tools (`web_search` / `web_fetch` / `code_execution` / `tool_search`) → `server_tool_use` + `*_tool_result` in the same assistant message (`stop_reason` usually `end_turn`, finish `:stop`). Stream finish is `:tool-use` only when a **client** tool call is present.

System turns → top-level `system`. Tool turns → user `tool_result` blocks. Thinking + `signature` round-trip on `llm-thinking-part`. `tool_choice`: `:auto` / `:none` / `:required` → `any` / string → `{type:tool,name}`. `list-models` `GET /models`; **404 → default model**. `backend-supports-p`: `:tools` `:stream` `:vision` `:thinking` + `:native-tools` (official). No `:embeddings`. Default `respond` = items→turns→`generate`.

**Prompt cache.** On by default for the official API (`:prompt-cache :auto`; `t` / `nil` force it; per call settings `:extra (:prompt-cache nil)`). Each request marks 4 breakpoints — the last tool, the system prompt and the last two messages — so a long agent run re-reads tools, system and history from cache. Thinking blocks are never marked; the caller's blocks are copied, not mutated. `llm-usage` reports `cache-read-tokens` / `cache-write-tokens`.

**API-side compaction** (beta `compact-2026-01-12`): `:server-compaction` on the backend or per call — `t` (API default trigger, 150K input tokens), an integer trigger (≥ 50000), or `(:trigger n :instructions "…" :pause-after-compaction t)`. The request gets a `compact_20260112` edit plus the beta header (`*request-betas*`; any `context_management` you pass in `:extra` gets its beta too). The `compaction` block comes back as an `anthropic-block-part` (`compaction-part-p` is true), is sent back verbatim, and `llm-usage-total-tokens` includes the compaction iteration. Streaming handles `compaction_delta`. Stop reason `compaction` → finish `:compaction`.

**Compaction on demand** (beta `compact-2026-09-04`, the API's recommended mode): `(llm-protocol:server-compact backend turns :instructions "…" :tools ts)` sends `"compaction": {"type": "summarize"}` and returns `(values turns compacted-p response)`: the system turns, then an assistant turn holding the signed `compaction` block. Send that history on; the block is passed back byte for byte, every request that carries it gets the beta header, and threshold `:server-compaction` is left off those requests (the API refuses both). No summary (`max_tokens`, `refusal`, …: see the response's finish reason) → the turns back and NIL. Refused before sending: a tool call with no result in the last assistant turn, blank or > 16384-char instructions. `backend-supports-p :compaction` is true for the `:official` dialect; with `llm-protocol/capability` it is the `:llm-compaction` capability. For an agent: `conversation-protocol:make-provider-compaction-memory` as settings extra `:compaction`. Anthropic parts (blocks, cited text, server tool calls) have `part-state` methods, so a journal replays them exactly.

**Errors.** `400`/`413` "prompt is too long" → `llm-context-overflow`. `Retry-After` reaches `llm-http-error-retry-after` (a `request-fn` may return it as a third value). A mid-stream `overloaded_error` is a retryable 529.

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
