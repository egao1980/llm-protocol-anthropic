(defsystem "llm-protocol-anthropic"
  :version "0.3.0"
  :description "Anthropic Messages API backend for llm-protocol (official + vLLM / llama-server)"
  :author "egao1980"
  :license "MIT"
  :depends-on ((:version "llm-protocol" "0.5.0") "http-protocol" "json-protocol" "json-backend-jzon"
               "sse-protocol" "babel")
  :properties
  (:cl-repo
   (:ci (:with ("http-backend-async"
                "event-backend-libuv"))))
  :serial t
  :pathname "src"
  :components ((:file "package")
               (:file "tools")
               (:file "backend")
               (:file "stream")
               (:file "compaction"))
  :in-order-to ((test-op (test-op "llm-protocol-anthropic/tests"))))

(defsystem "llm-protocol-anthropic/tests"
  :depends-on ("llm-protocol-anthropic" "rove")
  :pathname "tests"
  :serial t
  :components ((:file "package")
               (:file "backend-test"))
  :perform (test-op (o c)
             (unless (symbol-call :rove :run c)
               (error "tests failed for ~A" (component-name c)))))
