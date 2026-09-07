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
           #:+default-anthropic-version+))

(in-package #:llm-protocol-anthropic)
