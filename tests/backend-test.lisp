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

(defun %last-user-content (body)
  (let ((msgs (gethash "messages" body)))
    (when (and msgs (plusp (length msgs)))
      (gethash "content" (elt msgs (1- (length msgs)))))))

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
      (ok (equal "be brief" (gethash "system" seen)))
      (ok (zerop (gethash "temperature" seen)))
      (ok (= 16 (gethash "max_tokens" seen)))
      (ok (equal "sk-test" (cdr (assoc "x-api-key" seen-headers :test #'string-equal))))
      (ok (equal "Bearer sk-test"
                 (cdr (assoc "authorization" seen-headers :test #'string-equal))))
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
    (ok (equal "http://127.0.0.1:8080/v1"
               (llm-protocol-anthropic:anthropic-base-url l)))
    (ok (equal "local" (llm-protocol-anthropic:anthropic-default-model l)))
    (ok (equal "http://127.0.0.1:8000/v1"
               llm-protocol-anthropic:+default-vllm-base-url+))
    (ok (equal "http://127.0.0.1:8080/v1"
               llm-protocol-anthropic:+default-llama-server-base-url+))))

(deftest anthropic-supports
  (let ((b (llm-protocol-anthropic:make-anthropic-backend
            :request-fn #'%fake-anthropic)))
    (ok (llm-protocol:backend-supports-p b :tools))
    (ok (llm-protocol:backend-supports-p b :stream))
    (ok (llm-protocol:backend-supports-p b :vision))
    (ok (llm-protocol:backend-supports-p b :thinking))
    (ng (llm-protocol:backend-supports-p b :embeddings))))
