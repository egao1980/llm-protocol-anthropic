(in-package #:llm-protocol-anthropic/tests)

(defun %ht (&rest kvs)
  (let ((h (make-hash-table :test 'equal)))
    (loop for (k v) on kvs by #'cddr
          unless (or (null k) (eq v :omit) (null v))
            do (setf (gethash k h) v))
    h))

(defun %sse-block (data &optional event)
  (with-output-to-string (s)
    (when event
      (format s "event: ~a~%" event))
    (format s "data: ~a~%~%" data)))

(defun %messages-sse (text &key model tools thinking signature)
  (with-output-to-string (s)
    (write-string
     (%sse-block
      (stack-json:encode
       (%ht "type" "message_start"
            "message" (%ht "id" "msg_1"
                           "model" model
                           "usage" (%ht "input_tokens" 3 "output_tokens" 0))))
      "message_start")
     s)
    (let ((idx 0))
      (when thinking
        (write-string
         (%sse-block
          (stack-json:encode
           (%ht "type" "content_block_start"
                "index" idx
                "content_block" (%ht "type" "thinking" "thinking" "")))
          "content_block_start")
         s)
        (write-string
         (%sse-block
          (stack-json:encode
           (%ht "type" "content_block_delta"
                "index" idx
                "delta" (%ht "type" "thinking_delta" "thinking" thinking)))
          "content_block_delta")
         s)
        (when signature
          (write-string
           (%sse-block
            (stack-json:encode
             (%ht "type" "content_block_delta"
                  "index" idx
                  "delta" (%ht "type" "signature_delta" "signature" signature)))
            "content_block_delta")
           s))
        (write-string
         (%sse-block
          (stack-json:encode (%ht "type" "content_block_stop" "index" idx))
          "content_block_stop")
         s)
        (incf idx))
      (if tools
          (progn
            (write-string
             (%sse-block
              (stack-json:encode
               (%ht "type" "content_block_start"
                    "index" idx
                    "content_block" (%ht "type" "tool_use"
                                         "id" "toolu_1"
                                         "name" "sum"
                                         "input" (%ht))))
              "content_block_start")
             s)
            (write-string
             (%sse-block
              (stack-json:encode
               (%ht "type" "content_block_delta"
                    "index" idx
                    "delta" (%ht "type" "input_json_delta"
                                 "partial_json" "{\"a\":1}")))
              "content_block_delta")
             s)
            (write-string
             (%sse-block
              (stack-json:encode (%ht "type" "content_block_stop" "index" idx))
              "content_block_stop")
             s))
          (progn
            (write-string
             (%sse-block
              (stack-json:encode
               (%ht "type" "content_block_start"
                    "index" idx
                    "content_block" (%ht "type" "text" "text" "")))
              "content_block_start")
             s)
            (let* ((full (format nil "ok:~a" text))
                   (cut (min 3 (length full))))
              (write-string
               (%sse-block
                (stack-json:encode
                 (%ht "type" "content_block_delta"
                      "index" idx
                      "delta" (%ht "type" "text_delta"
                                   "text" (subseq full 0 cut))))
                "content_block_delta")
               s)
              (when (< cut (length full))
                (write-string
                 (%sse-block
                  (stack-json:encode
                   (%ht "type" "content_block_delta"
                        "index" idx
                        "delta" (%ht "type" "text_delta"
                                     "text" (subseq full cut))))
                  "content_block_delta")
                 s)))
            (write-string
             (%sse-block
              (stack-json:encode (%ht "type" "content_block_stop" "index" idx))
              "content_block_stop")
             s))))
    (write-string
     (%sse-block
      (stack-json:encode
       (%ht "type" "message_delta"
            "delta" (%ht "stop_reason" (if tools "tool_use" "end_turn"))
            "usage" (%ht "output_tokens" 2)))
      "message_delta")
     s)
    (write-string
     (%sse-block (stack-json:encode (%ht "type" "message_stop"))
                 "message_stop")
     s)))

(defun %block-text (content)
  "Text of CONTENT: a string, or the first text block of a block vector
(prompt-cache breakpoints turn a string into one text block)."
  (cond
    ((stringp content) content)
    ((and (vectorp content) (plusp (length content))
          (hash-table-p (elt content 0))
          (equal "text" (gethash "type" (elt content 0))))
     (gethash "text" (elt content 0)))
    (t content)))

(defun %last-user-content (body)
  (let ((msgs (gethash "messages" body)))
    (when (and msgs (plusp (length msgs)))
      (%block-text (gethash "content" (elt msgs (1- (length msgs))))))))

(defun %fake-anthropic (method url &key headers content want-stream)
  (declare (ignore headers))
  (cond
    ((and (eq method :post) (search "/messages" url))
     (let* ((body (stack-json:decode content))
            (text (%last-user-content body))
            (text (if (stringp text) text "hi"))
            (tools (gethash "tools" body))
            (model (or (gethash "model" body) "claude-sonnet-4-20250514")))
       (if (or want-stream (gethash "stream" body))
           (values 200 (%messages-sse text :model model :tools tools))
           (values 200
                   (stack-json:encode
                    (%ht "id" "msg_1"
                         "model" model
                         "stop_reason" (if tools "tool_use" "end_turn")
                         "usage" (%ht "input_tokens" 3 "output_tokens" 2)
                         "content"
                         (if tools
                             (vector (%ht "type" "tool_use"
                                          "id" "toolu_1"
                                          "name" "sum"
                                          "input" (%ht "a" 1)))
                             (vector (%ht "type" "text"
                                          "text" (format nil "ok:~a" text))))))))))
    ((search "/models" url)
     (values 200 (stack-json:encode
                  (%ht "data" (vector (%ht "id" "claude-sonnet-4-20250514"
                                           "owned_by" "anthropic"))))))
    (t (values 404 "{}"))))

(defun %fake-anthropic-thinking (method url &key headers content want-stream)
  (declare (ignore headers url method want-stream))
  (let ((body (stack-json:decode content)))
    (if (gethash "stream" body)
        (values 200 (%messages-sse "hi" :model "claude-sonnet-4-20250514"
                                   :thinking "reason" :signature "sig_1"))
        (values 200
                (stack-json:encode
                 (%ht "id" "msg_think"
                      "model" "claude-sonnet-4-20250514"
                      "stop_reason" "end_turn"
                      "usage" (%ht "input_tokens" 4 "output_tokens" 3)
                      "content"
                      (vector (%ht "type" "thinking"
                                   "thinking" "reason"
                                   "signature" "sig_1")
                              (%ht "type" "text" "text" "ok:hi"))))))))

(defun %fake-anthropic-error (method url &key headers content want-stream)
  (declare (ignore method url headers content want-stream))
  (values 401 (stack-json:encode
               (%ht "error" (%ht "message" "invalid api key" "type" "auth")))))

(defun %fake-anthropic-429 (method url &key headers content want-stream)
  (declare (ignore method url headers content want-stream))
  (values 429 (stack-json:encode
               (%ht "error" (%ht "message" "rate limited" "type" "rate")))))

(defun %fake-models-404 (method url &key headers content want-stream)
  (declare (ignore method headers content want-stream))
  (if (search "/models" url)
      (values 404 "{\"error\":{\"message\":\"not found\"}}")
      (%fake-anthropic method url :content content)))

(deftest anthropic-generate-mock-http
  (let* ((backend (llm-protocol-anthropic:make-anthropic-backend
                   :base-url "http://example.invalid/v1"
                   :api-key "sk-test"
                   :request-fn #'%fake-anthropic))
         (r (llm-protocol:generate backend "hi" :model "claude-sonnet-4-20250514")))
    (ok (equal "ok:hi" (llm-protocol:llm-response-text r)))
    (ok (equal "claude-sonnet-4-20250514" (llm-protocol:llm-response-model r)))
    (ok (eq :stop (llm-protocol:llm-response-finish-reason r)))
    (ok (= 5 (llm-protocol:llm-usage-total-tokens (llm-protocol:llm-response-usage r))))))

(deftest anthropic-system-and-max-tokens-on-wire
  (let ((seen nil)
        (seen-headers nil))
    (flet ((capture (method url &key headers content &allow-other-keys)
             (declare (ignore method url))
             (setf seen-headers headers)
             (setf seen (stack-json:decode content))
             (%fake-anthropic :post "http://x/v1/messages" :content content)))
      (llm-protocol:generate
       (llm-protocol-anthropic:make-anthropic-backend
        :api-key "sk-test" :request-fn #'capture)
       (list (llm-protocol:system-turn "be brief")
             (llm-protocol:user-turn "hi"))
       :settings '(:temperature 0 :max-tokens 16))
      (ok (equal "be brief" (%block-text (gethash "system" seen))))
      (ok (zerop (gethash "temperature" seen)))
      (ok (= 16 (gethash "max_tokens" seen)))
      (ok (equal "sk-test" (http-protocol:reveal
                            (cdr (assoc "x-api-key" seen-headers :test #'string-equal)))))
      (ok (equal "Bearer sk-test"
                 (http-protocol:reveal
                  (cdr (assoc "authorization" seen-headers :test #'string-equal)))))
      (ok (equal "2023-06-01"
                 (cdr (assoc "anthropic-version" seen-headers :test #'string-equal)))))))

(deftest anthropic-default-max-tokens
  (let ((seen nil))
    (flet ((capture (method url &key headers content &allow-other-keys)
             (declare (ignore method url headers))
             (setf seen (stack-json:decode content))
             (%fake-anthropic :post "http://x/v1/messages" :content content)))
      (llm-protocol:generate
       (llm-protocol-anthropic:make-anthropic-backend :request-fn #'capture)
       "hi")
      (ok (= 4096 (gethash "max_tokens" seen))))))

(deftest anthropic-tools-mock-http
  (let* ((backend (llm-protocol-anthropic:make-anthropic-backend
                   :request-fn #'%fake-anthropic))
         (r (llm-protocol:generate backend "add"
                                   :tools (list (llm-protocol:make-llm-tool :name "sum")))))
    (ok (eq :tool-use (llm-protocol:llm-response-finish-reason r)))
    (ok (equal "sum" (llm-protocol:llm-tool-call-part-name
                      (first (llm-protocol:llm-response-tool-calls r)))))))

(deftest anthropic-tool-choice-and-tool-result-on-wire
  (let ((seen nil))
    (flet ((capture (method url &key headers content &allow-other-keys)
             (declare (ignore method url headers))
             (setf seen (stack-json:decode content))
             (%fake-anthropic :post "http://x/v1/messages" :content content)))
      (llm-protocol:generate
       (llm-protocol-anthropic:make-anthropic-backend :request-fn #'capture)
       (list (llm-protocol:user-turn "add")
             (llm-protocol:assistant-turn
              nil
              :tool-calls (list (llm-protocol:make-llm-tool-call-part
                                 :id "toolu_1" :name "sum" :arguments "{\"a\":1}")))
             (llm-protocol:tool-turn "toolu_1" "2"))
       :tools (list (llm-protocol:make-llm-tool :name "sum"))
       :tool-choice :required)
      (ok (equal "any" (gethash "type" (gethash "tool_choice" seen))))
      (ok (equal "sum" (gethash "name" (elt (gethash "tools" seen) 0))))
      (let* ((last (elt (gethash "messages" seen)
                        (1- (length (gethash "messages" seen)))))
             (block (elt (gethash "content" last) 0)))
        (ok (equal "user" (gethash "role" last)))
        (ok (equal "tool_result" (gethash "type" block)))
        (ok (equal "toolu_1" (gethash "tool_use_id" block)))
        (ok (equal "2" (gethash "content" block)))))))

(deftest anthropic-thinking-signature-roundtrip
  (let* ((backend (llm-protocol-anthropic:make-anthropic-backend
                   :request-fn #'%fake-anthropic-thinking))
         (r (llm-protocol:generate backend "hi")))
    (ok (equal "ok:hi" (llm-protocol:llm-response-text r)))
    (ok (equal "reason" (llm-protocol:llm-response-thinking r)))
    (ok (equal "sig_1"
               (llm-protocol:llm-thinking-part-signature
                (find-if #'llm-protocol:llm-thinking-part-p
                         (llm-protocol:llm-response-parts r)))))))

(deftest anthropic-list-models-mock-http
  (let ((models (llm-protocol:list-models
                 (llm-protocol-anthropic:make-anthropic-backend
                  :request-fn #'%fake-anthropic))))
    (ok (equal "claude-sonnet-4-20250514"
               (llm-protocol:llm-model-info-id (first models))))
    (ok (equal "anthropic"
               (llm-protocol:llm-model-info-owned-by (first models))))))

(deftest anthropic-list-models-404-falls-back
  (let ((models (llm-protocol:list-models
                 (llm-protocol-anthropic:make-anthropic-backend
                  :default-model "local-llama"
                  :request-fn #'%fake-models-404))))
    (ok (equal "local-llama" (llm-protocol:llm-model-info-id (first models))))))

(deftest anthropic-http-error
  (ok (signals (llm-protocol:generate
                (llm-protocol-anthropic:make-anthropic-backend
                 :request-fn #'%fake-anthropic-error)
                "hi")
               'llm-protocol:llm-http-error)))

(deftest anthropic-http-retryable-slot
  (handler-case
      (llm-protocol:generate
       (llm-protocol-anthropic:make-anthropic-backend
        :request-fn #'%fake-anthropic-429)
       "hi")
    (llm-protocol:llm-http-error (c)
      (ok (eql 429 (llm-protocol:llm-http-error-status c)))
      (ok (llm-protocol:llm-http-error-retryable-p c))))
  (handler-case
      (llm-protocol:generate
       (llm-protocol-anthropic:make-anthropic-backend
        :request-fn #'%fake-anthropic-error)
       "hi")
    (llm-protocol:llm-http-error (c)
      (ok (eql 401 (llm-protocol:llm-http-error-status c)))
      (ng (llm-protocol:llm-http-error-retryable-p c)))))

(deftest anthropic-stream-generate-mock
  (let* ((seen nil)
         (backend (llm-protocol-anthropic:make-anthropic-backend
                   :request-fn #'%fake-anthropic))
         (r (llm-protocol:stream-generate
             backend "hi" :model "claude-sonnet-4-20250514"
             :on-part (lambda (p) (push p seen)))))
    (ok (equal "ok:hi" (llm-protocol:llm-response-text r)))
    (ok (equal "claude-sonnet-4-20250514" (llm-protocol:llm-response-model r)))
    (ok (eq :stop (llm-protocol:llm-response-finish-reason r)))
    (ok (= 2 (llm-protocol:llm-usage-output-tokens
              (llm-protocol:llm-response-usage r))))
    (ok (>= (length (remove-if-not #'llm-protocol:llm-text-part-p seen)) 2))))

(deftest anthropic-stream-generate-tools
  (let* ((backend (llm-protocol-anthropic:make-anthropic-backend
                   :request-fn #'%fake-anthropic))
         (r (llm-protocol:stream-generate
             backend "add"
             :tools (list (llm-protocol:make-llm-tool :name "sum")))))
    (ok (eq :tool-use (llm-protocol:llm-response-finish-reason r)))
    (ok (equal "sum" (llm-protocol:llm-tool-call-part-name
                      (first (llm-protocol:llm-response-tool-calls r)))))
    (ok (equal "{\"a\":1}" (llm-protocol:llm-tool-call-part-arguments
                            (first (llm-protocol:llm-response-tool-calls r)))))))

(deftest anthropic-stream-generate-thinking
  (let* ((backend (llm-protocol-anthropic:make-anthropic-backend
                   :request-fn #'%fake-anthropic-thinking))
         (r (llm-protocol:stream-generate backend "hi")))
    (ok (equal "reason" (llm-protocol:llm-response-thinking r)))
    (ok (equal "sig_1"
               (llm-protocol:llm-thinking-part-signature
                (find-if #'llm-protocol:llm-thinking-part-p
                         (llm-protocol:llm-response-parts r)))))))

(deftest anthropic-stream-generate-wire
  (let ((seen nil))
    (flet ((capture (method url &key headers content &allow-other-keys)
             (declare (ignore method headers))
             (ok (search "/messages" url))
             (setf seen (stack-json:decode content))
             (%fake-anthropic :post url :content content :want-stream t)))
      (llm-protocol:stream-generate
       (llm-protocol-anthropic:make-anthropic-backend :request-fn #'capture)
       "hi")
      (ok (eq t (gethash "stream" seen))))))

(deftest anthropic-vllm-llama-server-defaults
  (let ((v (llm-protocol-anthropic:make-vllm-anthropic-backend
            :base-url "http://127.0.0.1:8000/v1"
            :default-model "qwen"))
        (l (llm-protocol-anthropic:make-llama-server-anthropic-backend
            :base-url "http://127.0.0.1:8080/v1"
            :default-model "local")))
    (ok (equal "http://127.0.0.1:8000/v1"
               (llm-protocol-anthropic:anthropic-base-url v)))
    (ok (equal "qwen" (llm-protocol-anthropic:anthropic-default-model v)))
    (ok (eq :compat (llm-protocol-anthropic:anthropic-dialect v)))
    (ok (equal "http://127.0.0.1:8080/v1"
               (llm-protocol-anthropic:anthropic-base-url l)))
    (ok (equal "local" (llm-protocol-anthropic:anthropic-default-model l)))
    (ok (eq :compat (llm-protocol-anthropic:anthropic-dialect l)))
    (ok (equal "http://127.0.0.1:8000/v1"
               llm-protocol-anthropic:+default-vllm-base-url+))
    (ok (equal "http://127.0.0.1:8080/v1"
               llm-protocol-anthropic:+default-llama-server-base-url+))))

(deftest anthropic-supports
  (let ((b (llm-protocol-anthropic:make-anthropic-backend
            :request-fn #'%fake-anthropic))
        (v (llm-protocol-anthropic:make-vllm-anthropic-backend
            :request-fn #'%fake-anthropic)))
    (ok (llm-protocol:backend-supports-p b :tools))
    (ok (llm-protocol:backend-supports-p b :stream))
    (ok (llm-protocol:backend-supports-p b :vision))
    (ok (llm-protocol:backend-supports-p b :thinking))
    (ok (llm-protocol:backend-supports-p b :native-tools))
    (ng (llm-protocol:backend-supports-p b :embeddings))
    (ok (llm-protocol:backend-supports-p v :tools))
    (ng (llm-protocol:backend-supports-p v :native-tools))))

(deftest anthropic-native-tool-official-wire
  (let ((seen nil))
    (flet ((capture (method url &key headers content &allow-other-keys)
             (declare (ignore method url headers))
             (setf seen (stack-json:decode content))
             (%fake-anthropic :post "http://x/v1/messages" :content content)))
      (llm-protocol:generate
       (llm-protocol-anthropic:make-anthropic-backend :request-fn #'capture)
       "search"
       :tools (list (llm-protocol-anthropic:make-web-search-tool :max-uses 3)
                    :bash))
      (let ((t0 (elt (gethash "tools" seen) 0))
            (t1 (elt (gethash "tools" seen) 1)))
        (ok (equal "web_search_20260209" (gethash "type" t0)))
        (ok (equal "web_search" (gethash "name" t0)))
        (ok (= 3 (gethash "max_uses" t0)))
        (ok (equal "bash_20250124" (gethash "type" t1)))
        (ok (equal "bash" (gethash "name" t1)))))))

(deftest anthropic-native-tool-compat-flatten
  (let ((seen nil))
    (flet ((capture (method url &key headers content &allow-other-keys)
             (declare (ignore method url headers))
             (setf seen (stack-json:decode content))
             (%fake-anthropic :post "http://x/v1/messages" :content content)))
      (llm-protocol:generate
       (llm-protocol-anthropic:make-vllm-anthropic-backend :request-fn #'capture)
       "search"
       :tools (list (llm-protocol-anthropic:make-web-search-tool :max-uses 3)
                    :bash))
      (let ((t0 (elt (gethash "tools" seen) 0))
            (t1 (elt (gethash "tools" seen) 1)))
        (ok (equal "web_search" (gethash "name" t0)))
        (ok (hash-table-p (gethash "input_schema" t0)))
        (ok (equal "object" (gethash "type" (gethash "input_schema" t0))))
        (ok (gethash "query" (gethash "properties" (gethash "input_schema" t0))))
        (ng (equal "web_search_20260209" (gethash "type" t0)))
        (ng (gethash "max_uses" t0))
        (ok (equal "bash" (gethash "name" t1)))
        (ok (hash-table-p (gethash "input_schema" t1)))
        (ng (equal "bash_20250124" (gethash "type" t1)))))))

(defun %fake-server-search (method url &key headers content want-stream)
  (declare (ignore method url headers))
  (let ((payload
          (stack-json:encode
           (%ht "id" "msg_srv"
                "model" "claude-sonnet-4-20250514"
                "stop_reason" "end_turn"
                "usage" (%ht "input_tokens" 4 "output_tokens" 6)
                "content"
                (vector (%ht "type" "text" "text" "found")
                        (%ht "type" "server_tool_use"
                             "id" "srv_1"
                             "name" "web_search"
                             "input" (%ht "query" "cl"))
                        (%ht "type" "web_search_tool_result"
                             "tool_use_id" "srv_1"
                             "content" (vector (%ht "url" "https://x"))))))))
    (if want-stream
        (values 200
                (with-output-to-string (s)
                  (write-string
                   (%sse-block
                    (stack-json:encode
                     (%ht "type" "message_start"
                          "message" (%ht "id" "msg_srv"
                                         "model" "claude-sonnet-4-20250514"
                                         "usage" (%ht "input_tokens" 4
                                                      "output_tokens" 0))))
                    "message_start")
                   s)
                  (write-string
                   (%sse-block
                    (stack-json:encode
                     (%ht "type" "content_block_start"
                          "index" 0
                          "content_block" (%ht "type" "text" "text" "")))
                    "content_block_start")
                   s)
                  (write-string
                   (%sse-block
                    (stack-json:encode
                     (%ht "type" "content_block_delta"
                          "index" 0
                          "delta" (%ht "type" "text_delta" "text" "found")))
                    "content_block_delta")
                   s)
                  (write-string
                   (%sse-block
                    (stack-json:encode (%ht "type" "content_block_stop" "index" 0))
                    "content_block_stop")
                   s)
                  (write-string
                   (%sse-block
                    (stack-json:encode
                     (%ht "type" "content_block_start"
                          "index" 1
                          "content_block" (%ht "type" "server_tool_use"
                                               "id" "srv_1"
                                               "name" "web_search"
                                               "input" (%ht))))
                    "content_block_start")
                   s)
                  (write-string
                   (%sse-block
                    (stack-json:encode
                     (%ht "type" "content_block_delta"
                          "index" 1
                          "delta" (%ht "type" "input_json_delta"
                                       "partial_json" "{\"query\":\"cl\"}")))
                    "content_block_delta")
                   s)
                  (write-string
                   (%sse-block
                    (stack-json:encode (%ht "type" "content_block_stop" "index" 1))
                    "content_block_stop")
                   s)
                  (write-string
                   (%sse-block
                    (stack-json:encode
                     (%ht "type" "content_block_start"
                          "index" 2
                          "content_block"
                          (%ht "type" "web_search_tool_result"
                               "tool_use_id" "srv_1"
                               "content" (vector (%ht "url" "https://x")))))
                    "content_block_start")
                   s)
                  (write-string
                   (%sse-block
                    (stack-json:encode (%ht "type" "content_block_stop" "index" 2))
                    "content_block_stop")
                   s)
                  (write-string
                   (%sse-block
                    (stack-json:encode
                     (%ht "type" "message_delta"
                          "delta" (%ht "stop_reason" "end_turn")
                          "usage" (%ht "output_tokens" 6)))
                    "message_delta")
                   s)
                  (write-string
                   (%sse-block (stack-json:encode (%ht "type" "message_stop"))
                               "message_stop")
                   s)))
        (values 200 payload))))

(deftest anthropic-server-tool-use-parse
  (let* ((backend (llm-protocol-anthropic:make-anthropic-backend
                   :request-fn #'%fake-server-search))
         (r (llm-protocol:generate backend "search"
                                   :tools (list (llm-protocol-anthropic:make-web-search-tool)))))
    (ok (eq :stop (llm-protocol:llm-response-finish-reason r)))
    (ok (equal "found" (llm-protocol:llm-response-text r)))
    (let ((call (first (llm-protocol:llm-response-tool-calls r))))
      (ok (llm-protocol-anthropic:anthropic-tool-call-part-p call))
      (ok (llm-protocol-anthropic:anthropic-tool-call-server-p call))
      (ok (equal "web_search" (llm-protocol:llm-tool-call-part-name call)))
      (ok (search "cl" (llm-protocol:llm-tool-call-part-arguments call))))
    (ok (find-if #'llm-protocol-anthropic:anthropic-block-part-p
                 (llm-protocol:llm-response-parts r)))))

(deftest anthropic-stream-server-tool-use
  (let* ((backend (llm-protocol-anthropic:make-anthropic-backend
                   :request-fn #'%fake-server-search))
         (r (llm-protocol:stream-generate
             backend "search"
             :tools (list (llm-protocol-anthropic:make-web-search-tool)))))
    (ok (eq :stop (llm-protocol:llm-response-finish-reason r)))
    (ok (equal "found" (llm-protocol:llm-response-text r)))
    (let ((call (first (llm-protocol:llm-response-tool-calls r))))
      (ok (llm-protocol-anthropic:anthropic-tool-call-server-p call))
      (ok (equal "web_search" (llm-protocol:llm-tool-call-part-name call)))
      (ok (equal "{\"query\":\"cl\"}"
                 (llm-protocol:llm-tool-call-part-arguments call))))
    (ok (find-if #'llm-protocol-anthropic:anthropic-block-part-p
                 (llm-protocol:llm-response-parts r)))))

(deftest anthropic-compat-rewrites-server-tool-use
  (let ((seen nil))
    (flet ((capture (method url &key headers content &allow-other-keys)
             (declare (ignore method url headers))
             (setf seen (stack-json:decode content))
             (%fake-anthropic :post "http://x/v1/messages" :content content)))
      (llm-protocol:generate
       (llm-protocol-anthropic:make-llama-server-anthropic-backend
        :request-fn #'capture)
       (list (llm-protocol:user-turn "search")
             (llm-protocol:assistant-turn
              nil
              :tool-calls
              (list (make-instance 'llm-protocol-anthropic:anthropic-tool-call-part
                                   :id "srv_1" :name "web_search"
                                   :arguments "{\"query\":\"cl\"}"
                                   :server-p t))))
       :tools (list (llm-protocol-anthropic:make-web-search-tool)))
      (let* ((asst (elt (gethash "messages" seen) 1))
             (block (elt (gethash "content" asst) 0)))
        (ok (equal "assistant" (gethash "role" asst)))
        (ok (equal "tool_use" (gethash "type" block)))
        (ok (equal "web_search" (gethash "name" block)))
        (ng (equal "server_tool_use" (gethash "type" block)))))))

(deftest anthropic-beta-header
  (let ((seen-headers nil))
    (flet ((capture (method url &key headers content &allow-other-keys)
             (declare (ignore method url))
             (setf seen-headers headers)
             (%fake-anthropic :post "http://x/v1/messages" :content content)))
      (llm-protocol:generate
       (llm-protocol-anthropic:make-anthropic-backend
        :api-key "sk-test"
        :betas '("computer-use-2025-01-24" "mcp-client-2025-11-20")
        :request-fn #'capture)
       "hi")
      (ok (equal "computer-use-2025-01-24,mcp-client-2025-11-20"
                 (cdr (assoc "anthropic-beta" seen-headers :test #'string-equal)))))))

(deftest encode-image-part-url
  (let* ((h (llm-protocol-anthropic:encode-image-part
             (llm-protocol:make-llm-image-part :url "https://ex.test/a.png")))
         (src (gethash "source" h)))
    (ok (equal "image" (gethash "type" h)))
    (ok (equal "url" (gethash "type" src)))
    (ok (equal "https://ex.test/a.png" (gethash "url" src)))))

(deftest encode-image-part-base64-octets
  (let* ((octets (make-array 3 :element-type '(unsigned-byte 8)
                             :initial-contents '(1 2 3)))
         (h (llm-protocol-anthropic:encode-image-part
             (llm-protocol:make-llm-image-part
              :data octets :media-type "image/jpeg")))
         (src (gethash "source" h)))
    (ok (equal "image" (gethash "type" h)))
    (ok (equal "base64" (gethash "type" src)))
    (ok (equal "image/jpeg" (gethash "media_type" src)))
    (ok (equal "AQID" (gethash "data" src)))))

(deftest anthropic-image-part-on-wire
  (let ((seen nil))
    (flet ((capture (method url &key headers content &allow-other-keys)
             (declare (ignore method url headers))
             (setf seen (stack-json:decode content))
             (%fake-anthropic :post "http://x/v1/messages" :content content)))
      (llm-protocol:generate
       (llm-protocol-anthropic:make-anthropic-backend :request-fn #'capture)
       (llm-protocol:make-llm-turn
        :role :user
        :parts (list (llm-protocol:make-llm-text-part :text "see")
                     (llm-protocol:make-llm-image-part
                      :url "https://ex.test/a.png"))))
      (let* ((msgs (gethash "messages" seen))
             (content (gethash "content" (elt msgs 0)))
             (img (elt content 1)))
        (ok (vectorp content))
        (ok (equal "image" (gethash "type" img)))
        (ok (equal "url" (gethash "type" (gethash "source" img))))
        (ok (equal "https://ex.test/a.png"
                   (gethash "url" (gethash "source" img))))))))

(defun %capture-body (backend turns &rest args)
  (let ((seen nil))
    (setf (llm-protocol-anthropic::anthropic-request-fn backend)
          (lambda (method url &key content &allow-other-keys)
            (declare (ignore method url))
            (setf seen (stack-json:decode content))
            (%fake-anthropic :post "http://x/v1/messages" :content content)))
    (apply #'llm-protocol:generate backend turns args)
    seen))

(defun %cached-p (h)
  (and (hash-table-p h) (hash-table-p (gethash "cache_control" h))
       (equal "ephemeral" (gethash "type" (gethash "cache_control" h)))))

(defun %last-block (content)
  (and (vectorp content) (plusp (length content))
       (elt content (1- (length content)))))

(deftest anthropic-prompt-cache-breakpoints
  (let* ((turns (list (llm-protocol:system-turn "be brief")
                      (llm-protocol:user-turn "one")
                      (llm-protocol:assistant-turn "two")
                      (llm-protocol:user-turn "three")))
         (tools (list (llm-protocol:make-llm-tool :name "a" :description "A")
                      (llm-protocol:make-llm-tool :name "b" :description "B")))
         (body (%capture-body (llm-protocol-anthropic:make-anthropic-backend)
                              turns :tools tools))
         (msgs (gethash "messages" body)))
    (testing "last tool, system and the last two messages carry cache_control"
      (ok (%cached-p (elt (gethash "tools" body) 1)))
      (ng (%cached-p (elt (gethash "tools" body) 0)))
      (ok (%cached-p (%last-block (gethash "system" body))))
      (ok (equal "be brief" (%block-text (gethash "system" body))))
      (ok (= 3 (length msgs)))
      (ok (equal "one" (gethash "content" (elt msgs 0))))
      (ok (%cached-p (%last-block (gethash "content" (elt msgs 1)))))
      (ok (%cached-p (%last-block (gethash "content" (elt msgs 2)))))
      (ng (gethash "prompt_cache" body))))
  (testing "settings extra :prompt-cache nil and the compat dialect leave the wire alone"
    (let ((body (%capture-body (llm-protocol-anthropic:make-anthropic-backend)
                               (list (llm-protocol:system-turn "s")
                                     (llm-protocol:user-turn "u"))
                               :settings '(:extra (:prompt-cache nil)))))
      (ok (equal "s" (gethash "system" body)))
      (ng (gethash "prompt_cache" body)))
    (let ((body (%capture-body (llm-protocol-anthropic:make-anthropic-backend
                                :dialect :compat)
                               (list (llm-protocol:system-turn "s")
                                     (llm-protocol:user-turn "u")))))
      (ok (equal "s" (gethash "system" body)))
      (ok (equal "u" (gethash "content" (elt (gethash "messages" body) 0)))))))

(deftest anthropic-cache-usage-and-stream-usage-merge
  (let ((usage (llm-protocol-anthropic::%usage
                (%ht "input_tokens" 5 "output_tokens" 7
                     "cache_read_input_tokens" 1000
                     "cache_creation_input_tokens" 200))))
    (ok (= 1000 (llm-protocol:llm-usage-cache-read-tokens usage)))
    (ok (= 200 (llm-protocol:llm-usage-cache-write-tokens usage)))
    (ok (= 1205 (llm-protocol:llm-usage-prompt-tokens usage))))
  ;; message_start has input_tokens 3, message_delta only output_tokens 2.
  (let ((r (llm-protocol:stream-generate
            (llm-protocol-anthropic:make-anthropic-backend :request-fn #'%fake-anthropic)
            "hi")))
    (ok (= 3 (llm-protocol:llm-usage-input-tokens (llm-protocol:llm-response-usage r))))
    (ok (= 2 (llm-protocol:llm-usage-output-tokens (llm-protocol:llm-response-usage r))))))

(deftest anthropic-overflow-and-retry-after
  (flet ((backend (status body &optional retry-after)
           (llm-protocol-anthropic:make-anthropic-backend
            :request-fn (lambda (method url &key &allow-other-keys)
                          (declare (ignore method url))
                          (values status body retry-after)))))
    (ok (signals (llm-protocol:generate
                  (backend 400 "{\"type\":\"error\",\"error\":{\"type\":\"invalid_request_error\",\"message\":\"prompt is too long: 250000 tokens > 200000 maximum\"}}")
                  "hi")
                 'llm-protocol:llm-context-overflow))
    (let ((c (handler-case (llm-protocol:generate
                            (backend 429 "{\"error\":{\"message\":\"slow down\"}}" 12)
                            "hi")
               (llm-protocol:llm-http-error (e) e))))
      (ok (= 12 (llm-protocol:llm-http-error-retry-after c)))
      (ok (eq :rate-limit (llm-protocol:llm-retry-class c))))
    (ok (eq :overloaded
            (llm-protocol:llm-retry-class
             (handler-case (llm-protocol:generate
                            (backend 529 "{\"error\":{\"type\":\"overloaded_error\"}}")
                            "hi")
               (llm-protocol:llm-http-error (e) e)))))
    (ok (= 7 (llm-protocol-anthropic::%retry-after "7")))
    (ng (llm-protocol-anthropic::%retry-after "Wed, 21 Oct 2015 07:28:00 GMT"))))

(deftest anthropic-server-compaction
  (let ((seen nil) (seen-headers nil))
    (flet ((capture (method url &key headers content &allow-other-keys)
             (declare (ignore method url))
             (setf seen (stack-json:decode content) seen-headers headers)
             (values 200
                     (stack-json:encode
                      (%ht "id" "msg_c" "model" "m" "stop_reason" "end_turn"
                           "usage" (%ht "input_tokens" 10 "output_tokens" 2
                                        "iterations"
                                        (vector (%ht "type" "compaction"
                                                     "input_tokens" 100
                                                     "output_tokens" 20)
                                                (%ht "type" "message"
                                                     "input_tokens" 10
                                                     "output_tokens" 2)))
                           "content" (vector (%ht "type" "compaction"
                                                  "content" "SUMMARY")
                                             (%ht "type" "text" "text" "ok")))))))
      (let* ((b (llm-protocol-anthropic:make-anthropic-backend
                 :request-fn #'capture :betas '("other-beta")
                 :server-compaction '(:trigger 60000 :instructions "keep paths")))
             (r (llm-protocol:generate b "hi"))
             (edit (elt (gethash "edits" (gethash "context_management" seen)) 0))
             (block (find-if #'llm-protocol:compaction-part-p
                             (llm-protocol:llm-response-parts r))))
        (testing "request carries the edit and the beta"
          (ok (equal "compact_20260112" (gethash "type" edit)))
          (ok (= 60000 (gethash "value" (gethash "trigger" edit))))
          (ok (equal "keep paths" (gethash "instructions" edit)))
          (ng (gethash "server_compaction" seen))
          (ok (equal "other-beta,compact-2026-01-12"
                     (cdr (assoc "anthropic-beta" seen-headers :test #'string-equal)))))
        (testing "the compaction block is a part, billed tokens include it"
          (ok block)
          (ok (equal "ok" (llm-protocol:llm-response-text r)))
          (ok (= 132 (llm-protocol:llm-usage-total-tokens
                      (llm-protocol:llm-response-usage r)))))
        (testing "the block goes back verbatim on the next request"
          (llm-protocol:generate
           b (list (llm-protocol:user-turn "hi")
                   (llm-protocol:make-llm-turn :role :assistant
                                               :parts (llm-protocol:llm-response-parts r))
                   (llm-protocol:user-turn "more")))
          (let ((content (gethash "content" (elt (gethash "messages" seen) 1))))
            (ok (equal "compaction" (gethash "type" (elt content 0))))
            (ok (equal "SUMMARY" (gethash "content" (elt content 0))))))
        (testing "off by default and NIL per call"
          (llm-protocol:generate b "hi" :settings '(:extra (:server-compaction nil)))
          (ng (gethash "context_management" seen))
          (ok (equal "other-beta"
                     (cdr (assoc "anthropic-beta" seen-headers :test #'string-equal)))))
        (ok (signals (llm-protocol:generate b "hi" :settings '(:extra (:server-compaction 10)))
                     'type-error))))))

(deftest anthropic-stream-compaction-block
  (flet ((sse (method url &key &allow-other-keys)
           (declare (ignore method url))
           (values 200
                   (concatenate
                    'string
                    (%sse-block (stack-json:encode
                                 (%ht "type" "message_start"
                                      "message" (%ht "id" "m1" "usage" (%ht "input_tokens" 4))))
                                "message_start")
                    (%sse-block (stack-json:encode
                                 (%ht "type" "content_block_start" "index" 0
                                      "content_block" (%ht "type" "compaction")))
                                "content_block_start")
                    (%sse-block (stack-json:encode
                                 (%ht "type" "content_block_delta" "index" 0
                                      "delta" (%ht "type" "compaction_delta"
                                                   "content" "SUM")))
                                "content_block_delta")
                    (%sse-block (stack-json:encode (%ht "type" "content_block_stop" "index" 0))
                                "content_block_stop")
                    (%sse-block (stack-json:encode
                                 (%ht "type" "message_delta"
                                      "delta" (%ht "stop_reason" "end_turn")
                                      "usage" (%ht "output_tokens" 1)))
                                "message_delta")
                    (%sse-block (stack-json:encode (%ht "type" "message_stop"))
                                "message_stop")))))
    (let* ((r (llm-protocol:stream-generate
               (llm-protocol-anthropic:make-anthropic-backend :request-fn #'sse) "hi"))
           (part (find-if #'llm-protocol:compaction-part-p (llm-protocol:llm-response-parts r))))
      (ok part)
      (ok (equal "SUM" (gethash "content"
                                (llm-protocol-anthropic::anthropic-block-part-block part)))))))

;;; --- compaction on demand (compact-2026-09-04) --------------------------------

(defun %compacting-backend (log &key (stop "compaction") (summary "SUMMARY")
                                  server-compaction)
  "Backend whose request-fn pushes (body . headers) onto LOG's car and answers
a compaction request with a signed block (or, with SUMMARY NIL, nothing)."
  (llm-protocol-anthropic:make-anthropic-backend
   :server-compaction server-compaction
   :request-fn (lambda (method url &key headers content &allow-other-keys)
                 (declare (ignore method url))
                 (let ((body (stack-json:decode content)))
                   (push (cons body headers) (car log))
                   (values 200
                           (stack-json:encode
                            (if (gethash "compaction" body)
                                (%ht "id" "msg_s" "model" "m" "stop_reason" stop
                                     "usage" (%ht "input_tokens" 0 "output_tokens" 0)
                                     "content" (if summary
                                                   (vector (%ht "type" "compaction"
                                                                "content" summary
                                                                "signature" "sig-1"))
                                                   (vector)))
                                (%ht "id" "msg_r" "model" "m" "stop_reason" "end_turn"
                                     "usage" (%ht "input_tokens" 5 "output_tokens" 1)
                                     "content" (vector (%ht "type" "text" "text" "ok"))))))))))

(defun %beta-header (headers)
  (cdr (assoc "anthropic-beta" headers :test #'string-equal)))

(defun %chat ()
  (list (llm-protocol:system-turn "be brief")
        (llm-protocol:user-turn "q1")
        (llm-protocol:assistant-turn "a1")
        (llm-protocol:user-turn "q2")))

(deftest anthropic-compaction-on-demand
  (let* ((log (list nil))
         (b (%compacting-backend log :server-compaction 60000)))
    (ok (llm-protocol:backend-supports-p b :compaction))
    (ng (llm-protocol:backend-supports-p
         (llm-protocol-anthropic:make-vllm-anthropic-backend :base-url "http://x")
         :compaction)
        "compat dialects have no compaction")
    (multiple-value-bind (turns did response)
        (llm-protocol:server-compact b (%chat) :instructions "keep names")
      (destructuring-bind (body . headers) (first (car log))
        (testing "the summarize request"
          (ok (equal "summarize" (gethash "type" (gethash "compaction" body))))
          (ok (equal "keep names" (gethash "instructions" (gethash "compaction" body))))
          (ng (gethash "context_management" body)
              "threshold compaction is not combined with it")
          (ok (let ((system (gethash "system" body)))
                ;; A string, or text blocks once prompt-cache marks it.
                (if (stringp system)
                    (equal "be brief" system)
                    (some (lambda (b) (equal "be brief" (gethash "text" b)))
                          (coerce system 'list))))
              "same system prompt")
          (ok (search "compact-2026-09-04" (%beta-header headers)))))
      (testing "→ the system turn, then the signed block"
        (ok did)
        (ok (eq :compaction (llm-protocol:llm-response-finish-reason response)))
        (ok (= 2 (length turns)))
        (ok (eq :system (llm-protocol:llm-turn-role (first turns))))
        (let ((part (first (llm-protocol:llm-turn-parts (second turns)))))
          (ok (llm-protocol:compaction-part-p part))
          (ok (equal "SUMMARY" (llm-protocol:compaction-part-text part)))))
      (testing "the next request opens with the block, byte for byte, with the beta"
        (llm-protocol:generate b (append turns (list (llm-protocol:user-turn "q3"))))
        (destructuring-bind (body . headers) (first (car log))
          (let* ((first-msg (elt (gethash "messages" body) 0))
                 (block (elt (gethash "content" first-msg) 0)))
            (ok (equal "assistant" (gethash "role" first-msg)))
            (ok (equal "compaction" (gethash "type" block)))
            (ok (equal "sig-1" (gethash "signature" block)))
            (ok (equal "SUMMARY" (gethash "content" block))))
          (ok (search "compact-2026-09-04" (%beta-header headers)))
          (ng (gethash "context_management" body)
              "no threshold edit next to a signed block")))
      (testing "the block survives part state (journals)"
        (let* ((part (first (llm-protocol:llm-turn-parts (second turns))))
               (back (llm-protocol:part-from-state (llm-protocol:part-state part))))
          (ok (llm-protocol:compaction-part-p back))
          (ok (equal "sig-1" (gethash "signature"
                                      (llm-protocol-anthropic:anthropic-block-part-block
                                       back)))))))))

(deftest anthropic-compaction-without-a-summary
  (let ((log (list nil)))
    (multiple-value-bind (turns did response)
        (llm-protocol:server-compact (%compacting-backend log :stop "max_tokens"
                                                              :summary nil)
                                     (%chat))
      (ng did)
      (ok (= 4 (length turns)) "the turns come back unchanged")
      (ok (eq :length (llm-protocol:llm-response-finish-reason response)))))
  (testing "a pending tool call is refused before the request"
    (let ((log (list nil)))
      (ok (signals (llm-protocol:server-compact
                    (%compacting-backend log)
                    (list (llm-protocol:user-turn "q")
                          (llm-protocol:make-llm-turn
                           :role :assistant
                           :parts (list (llm-protocol:make-llm-tool-call-part
                                         :id "t1" :name "f")))))
                   'llm-protocol:llm-error))
      (ng (car log) "nothing was sent")))
  (testing "blank or oversized instructions are refused"
    (ok (signals (llm-protocol:server-compact (%compacting-backend (list nil)) (%chat)
                                              :instructions "  ")
                 'llm-protocol:llm-error))
    (ok (signals (llm-protocol:server-compact (%compacting-backend (list nil)) (%chat)
                                              :instructions (make-string 16385
                                                                         :initial-element #\a))
                 'llm-protocol:llm-error))))

(deftest anthropic-parts-survive-part-state
  (let* ((call (make-instance 'llm-protocol-anthropic::anthropic-tool-call-part
                              :id "srv_1" :name "web_search" :arguments "{\"q\":\"x\"}"
                              :server-p t :extras '(:caller "direct")))
         (back (llm-protocol:part-from-state (llm-protocol:part-state call))))
    (ok (llm-protocol-anthropic::anthropic-tool-call-server-p back)
        "a server tool call stays a server tool call")
    (ok (equal "direct" (getf (llm-protocol-anthropic:anthropic-tool-call-extras back)
                              :caller)))
    (ok (equal "{\"q\":\"x\"}" (llm-protocol:llm-tool-call-part-arguments back)))))
