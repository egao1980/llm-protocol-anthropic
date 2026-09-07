(defpackage #:llm-protocol-anthropic
  (:use #:cl #:llm-protocol)
  (:nicknames #:stack-llm-anthropic)
  (:export #:anthropic-backend
           #:make-anthropic-backend
           #:use-anthropic-backend
           #:make-vllm-anthropic-backend
           #:make-llama-server-anthropic-backend
           #:anthropic-base-url
           #:anthropic-api-key
           #:anthropic-default-model
           #:anthropic-version
           #:+default-anthropic-base-url+
           #:+default-vllm-base-url+
           #:+default-llama-server-base-url+
           #:+default-anthropic-version+
           #:anthropic-tool
           #:anthropic-tool-p
           #:make-anthropic-tool
           #:anthropic-tool-kind
           #:anthropic-tool-type
           #:anthropic-tool-name
           #:anthropic-tool-exec
           #:anthropic-tool-options
           #:make-web-search-tool
           #:make-web-fetch-tool
           #:make-code-execution-tool
           #:make-bash-tool
           #:make-text-editor-tool
           #:make-computer-tool
           #:make-memory-tool
           #:anthropic-tool-call-part
           #:anthropic-tool-call-part-p
           #:anthropic-tool-call-extras
           #:anthropic-tool-call-server-p
           #:anthropic-block-part
           #:anthropic-block-part-p
           #:anthropic-block-part-block
           #:make-anthropic-block-part
           #:anthropic-text-part
           #:anthropic-text-part-p
           #:anthropic-text-citations
           #:anthropic-betas
           #:anthropic-dialect
           #:+anthropic-native-tools+))

(in-package #:llm-protocol-anthropic)
