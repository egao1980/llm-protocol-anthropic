(defsystem "llm-protocol-anthropic"
  :version "0.5.0"
  :description "Anthropic Messages API backend for llm-protocol (official + vLLM / llama-server)"
  :author "egao1980"
  :license "MIT"
  :depends-on ((:version "llm-protocol" "0.5.0") (:version "http-protocol" "0.4.0") "json-protocol" "json-backend-jzon"
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
  :depends-on ("llm-protocol-anthropic"
               (:version "http-backend-async" "0.2.25") "event-backend-libuv"
               "usocket" "bordeaux-threads" "rove")
  :pathname "tests"
  :serial t
  :components ((:file "package")
               (:file "backend-test")
               (:file "keys-test")
               (:file "timeout-test"))
  :perform (test-op (o c)
             (unless (symbol-call :rove :run c)
               (error "tests failed for ~A" (component-name c)))))
