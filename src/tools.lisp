(in-package #:llm-protocol-anthropic)

;;; Anthropic-defined tools. Custom JSON-schema tools stay llm-tool.
;;; Client tools (bash / text_editor / computer / memory) → tool_use + you execute.
;;; Server tools (web_search / web_fetch / code_execution / tool_search) →
;;; server_tool_use + *_tool_result in the same assistant message.

(defparameter +anthropic-native-tools+
  '((:web-search :type "web_search_20260209" :name "web_search" :exec :server)
    (:web-fetch :type "web_fetch_20260209" :name "web_fetch" :exec :server)
    (:code-execution :type "code_execution_20250825" :name "code_execution"
     :exec :server)
    (:tool-search :type "tool_search_tool_regex_20251119"
     :name "tool_search_tool_regex" :exec :server)
    (:memory :type "memory_20250818" :name "memory" :exec :client)
    (:bash :type "bash_20250124" :name "bash" :exec :client)
    (:text-editor :type "text_editor_20250728" :name "str_replace_based_edit_tool"
     :exec :client)
    (:computer :type "computer_toolset_20260801" :name nil :exec :client)
    (:computer-20251124 :type "computer_20251124" :name "computer" :exec :client)))

(defun anthropic-native-tool-spec (kind)
  (find kind +anthropic-native-tools+ :key #'first))

(defclass anthropic-tool ()
  ((kind :initarg :kind :accessor anthropic-tool-kind)
   (type :initarg :type :accessor anthropic-tool-type)
   (name :initarg :name :accessor anthropic-tool-name :initform nil)
   (exec :initarg :exec :accessor anthropic-tool-exec :initform :client)
   (options :initarg :options :accessor anthropic-tool-options :initform nil)))

(defun anthropic-tool-p (x)
  (typep x 'anthropic-tool))

(defun make-anthropic-tool (kind &rest options &key type name exec
                            &allow-other-keys)
  (let ((spec (or (anthropic-native-tool-spec kind)
                  (and (stringp kind)
                       (find kind +anthropic-native-tools+
                             :key (lambda (s) (getf (rest s) :name))
                             :test #'string-equal)))))
    (unless (or spec type)
      (error 'llm-error
             :message (format nil "unknown Anthropic native tool ~s" kind)))
    (let ((opts (copy-list options)))
      (dolist (k '(:type :name :exec))
        (remf opts k))
      (make-instance 'anthropic-tool
                     :kind (or (and spec (first spec)) kind)
                     :type (or type (and spec (getf (rest spec) :type)))
                     :name (or name (and spec (getf (rest spec) :name)))
                     :exec (or exec (and spec (getf (rest spec) :exec)) :client)
                     :options opts))))

(defun make-web-search-tool (&rest options)
  (apply #'make-anthropic-tool :web-search options))

(defun make-web-fetch-tool (&rest options)
  (apply #'make-anthropic-tool :web-fetch options))

(defun make-code-execution-tool (&rest options)
  (apply #'make-anthropic-tool :code-execution options))

(defun make-bash-tool (&rest options)
  (apply #'make-anthropic-tool :bash options))

(defun make-text-editor-tool (&rest options)
  (apply #'make-anthropic-tool :text-editor options))

(defun make-computer-tool (&rest options)
  (apply #'make-anthropic-tool :computer options))

(defun make-memory-tool (&rest options)
  (apply #'make-anthropic-tool :memory options))

(defclass anthropic-tool-call-part (llm-tool-call-part)
  ((extras :initarg :extras :accessor anthropic-tool-call-extras :initform nil)
   (server-p :initarg :server-p :accessor anthropic-tool-call-server-p
             :initform nil)))

(defun anthropic-tool-call-part-p (x)
  (typep x 'anthropic-tool-call-part))

(defclass anthropic-block-part (llm-part)
  ((block :initarg :block :accessor anthropic-block-part-block))
  (:documentation "Opaque Anthropic content block (server results, citations extras)."))

(defun anthropic-block-part-p (x)
  (typep x 'anthropic-block-part))

(defun make-anthropic-block-part (block)
  (make-instance 'anthropic-block-part :block block))

(defclass anthropic-text-part (llm-text-part)
  ((citations :initarg :citations :accessor anthropic-text-citations :initform nil)))

(defun anthropic-text-part-p (x)
  (typep x 'anthropic-text-part))

(defun %option-key (k)
  (if (stringp k)
      k
      (substitute #\_ #\- (string-downcase (symbol-name k)))))

(defun %put-options (h options)
  (cond
    ((null options) h)
    ((hash-table-p options)
     (maphash (lambda (k v)
                (unless (or (null v) (eq v :omit))
                  (setf (gethash (%option-key k) h) v)))
              options)
     h)
    ((and (consp options) (or (keywordp (car options)) (stringp (car options))))
     (loop for (k v) on options by #'cddr
           unless (or (null v) (eq v :omit)
                      (member k '(:type :name :exec :kind) :test #'equal))
             do (setf (gethash (%option-key k) h)
                      (if (keywordp v)
                          (string-downcase (symbol-name v))
                          v)))
     h)
    (t h)))

(defun %wire-native-tool (tool)
  (let ((h (%ht "type" (anthropic-tool-type tool)
                "name" (or (anthropic-tool-name tool) :omit))))
    (%put-options h (anthropic-tool-options tool))
    h))

(defun %compat-tool-name (tool)
  (or (anthropic-tool-name tool)
      (substitute #\_ #\- (string-downcase (symbol-name (anthropic-tool-kind tool))))))

(defun %native-tool-schema (kind)
  "JSON Schema stand-in so vLLM / llama-server can emit the same names."
  (ecase kind
    (:web-search
     (%ht "type" "object"
          "properties" (%ht "query" (%ht "type" "string"))
          "required" #("query")))
    (:web-fetch
     (%ht "type" "object"
          "properties" (%ht "url" (%ht "type" "string"))
          "required" #("url")))
    (:code-execution
     (%ht "type" "object"
          "properties" (%ht "code" (%ht "type" "string")
                            "command" (%ht "type" "string"))
          "required" #("code")))
    (:tool-search
     (%ht "type" "object"
          "properties" (%ht "query" (%ht "type" "string"))
          "required" #("query")))
    (:memory
     (%ht "type" "object"
          "properties" (%ht "command" (%ht "type" "string")
                            "path" (%ht "type" "string")
                            "file_text" (%ht "type" "string"))
          "required" #("command")))
    (:bash
     (%ht "type" "object"
          "properties" (%ht "command" (%ht "type" "string")
                            "restart" (%ht "type" "boolean"))
          "required" #("command")))
    (:text-editor
     (%ht "type" "object"
          "properties" (%ht "command" (%ht "type" "string")
                            "path" (%ht "type" "string")
                            "old_str" (%ht "type" "string")
                            "new_str" (%ht "type" "string")
                            "file_text" (%ht "type" "string")
                            "insert_line" (%ht "type" "integer"))
          "required" #("command" "path")))
    ((:computer :computer-20251124)
     (%ht "type" "object"
          "properties" (%ht "action" (%ht "type" "string")
                            "coordinate" (%ht "type" "array"
                                              "items" (%ht "type" "integer"))
                            "text" (%ht "type" "string"))
          "required" #("action")))))

(defun %native-tool-description (tool)
  (format nil "Anthropic ~a (compat flatten for vLLM / llama-server)"
          (%compat-tool-name tool)))

(defun %wire-compat-tool (tool)
  (%ht "name" (%compat-tool-name tool)
       "description" (%native-tool-description tool)
       "input_schema" (%native-tool-schema (anthropic-tool-kind tool))))

(defun %server-result-type-p (type)
  (and (stringp type)
       (or (search "_tool_result" type)
           (string-equal type "mcp_tool_result"))))

(defun %server-call-type-p (type)
  (or (string-equal type "server_tool_use")
      (string-equal type "mcp_tool_use")))
