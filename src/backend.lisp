(in-package #:llm-protocol-anthropic)

;;; Anthropic Messages API. Same class for api.anthropic.com, vLLM, llama-server.
;;; Native GGUF stays llm-backend-llama-cpp (CFFI) — not this wire.

(defparameter +default-anthropic-base-url+ "https://api.anthropic.com/v1")
(defparameter +default-vllm-base-url+ "http://127.0.0.1:8000/v1")
(defparameter +default-llama-server-base-url+ "http://127.0.0.1:8080/v1")
(defparameter +default-anthropic-version+ "2023-06-01")

(defun %env (name)
  (let ((v (uiop:getenv name)))
    (and v (plusp (length v)) v)))

(defvar *anthropic-dialect* :official
  "Bound while wiring a request. :official = Anthropic-native tool types.
   :compat = flatten to name + input_schema (vLLM / llama-server).")

(defclass anthropic-backend (llm-backend)
  ((base-url :initarg :base-url :accessor anthropic-base-url
             :initform +default-anthropic-base-url+)
   (api-key :initarg :api-key :accessor anthropic-api-key :initform nil)
   (default-model :initarg :default-model :accessor anthropic-default-model
                  :initform "claude-sonnet-4-20250514")
   (version :initarg :version :accessor anthropic-version
            :initform +default-anthropic-version+)
   (betas :initarg :betas :accessor anthropic-betas :initform nil)
   (dialect :initarg :dialect :accessor anthropic-dialect :initform :official)
   (request-fn :initarg :request-fn :accessor anthropic-request-fn :initform nil)))

(defun make-anthropic-backend (&key base-url api-key default-model version
                                 betas dialect request-fn)
  (make-instance 'anthropic-backend
                 :base-url (or base-url (%env "ANTHROPIC_BASE_URL")
                               +default-anthropic-base-url+)
                 :api-key (or api-key (%env "ANTHROPIC_API_KEY")
                              (%env "ANTHROPIC_AUTH_TOKEN"))
                 :default-model (or default-model (%env "ANTHROPIC_MODEL")
                                    "claude-sonnet-4-20250514")
                 :version (or version (%env "ANTHROPIC_VERSION")
                              +default-anthropic-version+)
                 :betas (or betas
                            (let ((v (%env "ANTHROPIC_BETA")))
                              (and v (uiop:split-string v :separator ","))))
                 :dialect (or dialect :official)
                 :request-fn request-fn))

(defun make-vllm-anthropic-backend (&rest args &key base-url dialect
                                    &allow-other-keys)
  (let ((args (copy-list args)))
    (remf args :base-url)
    (remf args :dialect)
    (apply #'make-anthropic-backend
           :base-url (or base-url (%env "ANTHROPIC_BASE_URL")
                         +default-vllm-base-url+)
           :dialect (or dialect :compat)
           args)))

(defun make-llama-server-anthropic-backend (&rest args &key base-url dialect
                                            &allow-other-keys)
  (let ((args (copy-list args)))
    (remf args :base-url)
    (remf args :dialect)
    (apply #'make-anthropic-backend
           :base-url (or base-url (%env "ANTHROPIC_BASE_URL")
                         +default-llama-server-base-url+)
           :dialect (or dialect :compat)
           args)))

(defun use-anthropic-backend (&rest args &key &allow-other-keys)
  (setf *llm-backend* (apply #'make-anthropic-backend args)))

(defmethod backend-model ((backend anthropic-backend))
  (anthropic-default-model backend))

(defmethod backend-supports-p ((backend anthropic-backend) (feature (eql :tools)))
  t)

(defmethod backend-supports-p ((backend anthropic-backend) (feature (eql :stream)))
  t)

(defmethod backend-supports-p ((backend anthropic-backend) (feature (eql :vision)))
  t)

(defmethod backend-supports-p ((backend anthropic-backend)
                               (feature (eql :thinking)))
  t)

(defmethod backend-supports-p ((backend anthropic-backend)
                               (feature (eql :native-tools)))
  (not (eq (anthropic-dialect backend) :compat)))

(defun %ht (&rest kvs)
  (let ((h (make-hash-table :test 'equal)))
    (loop for (k v) on kvs by #'cddr
          unless (or (null k) (eq v :omit) (null v))
            do (setf (gethash k h) v))
    h))

(defun %join (base path)
  (format nil "~a~a" (string-right-trim "/" (or base "")) path))

(defun %headers (backend &key (accept "application/json"))
  (let ((h `(("content-type" . "application/json")
             ("accept" . ,accept)
             ("anthropic-version" . ,(or (anthropic-version backend)
                                         +default-anthropic-version+)))))
    (when (and (anthropic-api-key backend)
               (plusp (length (anthropic-api-key backend))))
      (push (cons "x-api-key" (anthropic-api-key backend)) h)
      (push (cons "authorization"
                  (format nil "Bearer ~a" (anthropic-api-key backend)))
            h))
    (let ((betas (anthropic-betas backend)))
      (when betas
        (push (cons "anthropic-beta"
                    (if (stringp betas)
                        betas
                        (format nil "~{~a~^,~}" (llm-protocol::%as-list betas))))
              h)))
    h))

(defun %body-string (response)
  (let ((b (http-protocol:response-body response)))
    (cond
      ((stringp b) b)
      ((and (vectorp b) (not (stringp b)))
       (babel:octets-to-string b :encoding :utf-8))
      (t ""))))

(defun %http-request (method url &key headers content want-stream)
  (unless http-protocol:*http-backend*
    (error 'llm-error
           :message "*http-backend* is nil — bind an http-protocol backend"))
  (let ((res (apply #'http:request method url
                    :headers headers
                    :timeout 180
                    :want-stream (and want-stream t)
                    (and content (list :content content)))))
    (values (http-protocol:response-status res)
            (if want-stream
                (http-protocol:response-body res)
                (%body-string res)))))

(defun %request (backend method path &optional object &key want-stream accept)
  (let* ((fn (or (anthropic-request-fn backend) #'%http-request))
         (url (%join (anthropic-base-url backend) path))
         (content (and object (stack-json:encode object)))
         (headers (%headers backend
                            :accept (or accept
                                        (if want-stream
                                            "text/event-stream"
                                            "application/json")))))
    (multiple-value-bind (status body)
        (funcall fn method url :headers headers :content content
                 :want-stream want-stream)
      (values status body))))

(defun %error-message (obj fallback)
  (cond
    ((and (hash-table-p obj) (hash-table-p (gethash "error" obj)))
     (or (gethash "message" (gethash "error" obj)) fallback))
    ((and (hash-table-p obj) (gethash "error" obj))
     (let ((err (gethash "error" obj)))
       (if (stringp err) err (princ-to-string err))))
    (t fallback)))

(defun %decode (status body)
  (let ((obj (ignore-errors (stack-json:decode body))))
    (cond
      ((<= 200 status 299) (or obj (error 'llm-error :message "empty JSON body")))
      (t
       (restart-case
           (error 'llm-http-error
                  :status status
                  :body body
                  :retryable-p (http-status-retryable-p status)
                  :message (%error-message obj (format nil "HTTP ~a" status)))
         (retry ()
           :report "Retry the HTTP request"
           (llm-protocol::%invoke-retry))
         (use-value (value)
           :report "Use a supplied decoded object"
           value))))))

(defun %str (x)
  (cond
    ((or (null x) (eq x :null)) "")
    ((stringp x) x)
    (t (princ-to-string x))))

(defun %json-args (arguments)
  (cond
    ((null arguments) (%ht))
    ((hash-table-p arguments) arguments)
    ((stringp arguments)
     (or (ignore-errors (stack-json:decode arguments)) (%ht)))
    (t arguments)))

(defun %wire-tool-call (part)
  (let ((h (%ht "type" (if (and (not (eq *anthropic-dialect* :compat))
                                (anthropic-tool-call-part-p part)
                                (anthropic-tool-call-server-p part))
                           "server_tool_use"
                           "tool_use")
                "id" (or (llm-tool-call-part-id part) "toolu_0")
                "name" (llm-tool-call-part-name part)
                "input" (%json-args (llm-tool-call-part-arguments part)))))
    (when (anthropic-tool-call-part-p part)
      (%put-options h (anthropic-tool-call-extras part)))
    h))

(defun %wire-part (part)
  (etypecase part
    (anthropic-block-part
     (let ((b (anthropic-block-part-block part)))
       (if (hash-table-p b) b nil)))
    (anthropic-text-part
     (let ((h (%ht "type" "text" "text" (or (llm-text-part-text part) ""))))
       (when (anthropic-text-citations part)
         (setf (gethash "citations" h) (anthropic-text-citations part)))
       h))
    (llm-text-part
     (%ht "type" "text" "text" (or (llm-text-part-text part) "")))
    (llm-image-part
     (%ht "type" "image"
          "source" (if (llm-image-part-url part)
                       (%ht "type" "url" "url" (llm-image-part-url part))
                       (%ht "type" "base64"
                            "media_type" (or (llm-image-part-media-type part)
                                             "image/png")
                            "data" (or (llm-image-part-data part) "")))))
    (llm-thinking-part
     (let ((h (%ht "type" "thinking"
                   "thinking" (or (llm-thinking-part-text part) ""))))
       (when (llm-thinking-part-signature part)
         (setf (gethash "signature" h) (llm-thinking-part-signature part)))
       h))
    (llm-tool-call-part
     (%wire-tool-call part))
    (llm-tool-result-part
     (%ht "type" "tool_result"
          "tool_use_id" (llm-tool-result-part-id part)
          "content" (or (llm-tool-result-part-content part) "")
          "is_error" (if (llm-tool-result-part-error-p part) t :omit)))
    (llm-part nil)))

(defun %content-blocks (parts)
  (let ((blocks (remove nil (mapcar #'%wire-part parts))))
    (cond
      ((null blocks) (vector))
      ((and (null (rest blocks))
            (equal (gethash "type" (first blocks)) "text"))
       (gethash "text" (first blocks)))
      (t (map 'vector #'identity blocks)))))

(defun %wire-messages (turns)
  "→ (values system messages-vector). Tool turns become user tool_result blocks."
  (let ((system nil)
        (msgs '())
        (pending-tools '()))
    (labels ((flush-tools ()
               (when pending-tools
                 (push (%ht "role" "user"
                            "content" (map 'vector #'identity
                                           (nreverse pending-tools)))
                       msgs)
                 (setf pending-tools nil)))
             (push-msg (role content)
               (flush-tools)
               (push (%ht "role" role "content" content) msgs)))
      (dolist (turn (coerce-turns turns))
        (ecase (llm-turn-role turn)
          (:system
           (let ((tx (turn-text turn)))
             (when (plusp (length tx))
               (setf system (if system (format nil "~a~%~a" system tx) tx)))))
          (:user
           (push-msg "user" (%content-blocks (llm-turn-parts turn))))
          (:assistant
           (push-msg "assistant" (%content-blocks (llm-turn-parts turn))))
          (:tool
           (dolist (p (llm-turn-parts turn))
             (let ((b (%wire-part p)))
               (when b (push b pending-tools)))))))
      (flush-tools)
      (values system (map 'vector #'identity (nreverse msgs))))))

(defun %wire-tool (tool)
  (cond
    ((anthropic-tool-p tool)
     (if (eq *anthropic-dialect* :compat)
         (%wire-compat-tool tool)
         (%wire-native-tool tool)))
    ((keywordp tool)
     (%wire-tool (make-anthropic-tool tool)))
    ((stringp tool)
     (%wire-tool (make-anthropic-tool tool)))
    ((llm-tool-p tool)
     (%ht "name" (llm-tool-name tool)
          "description" (or (llm-tool-description tool) "")
          "input_schema" (or (llm-tool-parameters tool)
                             (%ht "type" "object" "properties" (%ht)))))
    ((hash-table-p tool) tool)
    ((and (consp tool) (keywordp (car tool)) (or (getf tool :type) (getf tool :kind)))
     (%wire-tool (apply #'make-anthropic-tool
                        (or (getf tool :kind) :web-search)
                        tool)))
    ((and (consp tool) (keywordp (car tool)))
     (%wire-tool (make-llm-tool :name (getf tool :name)
                                :description (getf tool :description)
                                :parameters (getf tool :parameters))))
    (t (error 'llm-error :message (format nil "not a tool: ~s" tool)))))

(defun %wire-tool-choice (choice)
  (etypecase choice
    (null nil)
    ((eql :auto) (%ht "type" "auto"))
    ((eql :none) (%ht "type" "none"))
    ((eql :required) (%ht "type" "any"))
    (string (%ht "type" "tool" "name" choice))
    (hash-table choice)))

(defun %apply-settings-extra (body settings)
  (let ((extra (and settings (llm-settings-extra settings))))
    (when extra
      (flet ((put (k v)
               (let ((key (if (stringp k)
                              k
                              (substitute #\_ #\- (string-downcase (symbol-name k))))))
                 (unless (nth-value 1 (gethash key body))
                   (setf (gethash key body)
                         (if (keywordp v)
                             (string-downcase (symbol-name v))
                             v))))))
        (cond
          ((hash-table-p extra)
           (maphash #'put extra))
          ((and (consp extra) (or (keywordp (car extra)) (stringp (car extra))))
           (loop for (k v) on extra by #'cddr
                 do (put k v))))))
    body))

(defun %messages-body (backend turns &key model settings tools tool-choice stream)
  (let* ((*anthropic-dialect* (or (anthropic-dialect backend) :official))
         (settings (coerce-settings settings))
         (model (or model (anthropic-default-model backend)))
         (max (or (and settings (llm-settings-max-tokens settings)) 4096)))
    (multiple-value-bind (system messages)
        (%wire-messages turns)
      (let ((body (%ht "model" model
                       "max_tokens" max
                       "system" (or system :omit)
                       "messages" messages
                       "temperature" (and settings (llm-settings-temperature settings))
                       "top_p" (and settings (llm-settings-top-p settings))
                       "stop_sequences" (and settings (llm-settings-stop settings))
                       "tools" (and tools (map 'vector #'%wire-tool
                                               (llm-protocol::%as-list tools)))
                       "tool_choice" (%wire-tool-choice tool-choice)
                       "stream" (if stream t :omit))))
        (%apply-settings-extra body settings)
        (values model body)))))

(defun %finish-reason (raw)
  (cond
    ((or (null raw) (eq raw :null)) :stop)
    ((or (string-equal raw "end_turn") (string-equal raw "stop_sequence")) :stop)
    ((string-equal raw "max_tokens") :length)
    ((string-equal raw "tool_use") :tool-use)
    ((or (string-equal raw "pause_turn") (string-equal raw "refusal")) :stop)
    (t :stop)))

(defun %usage (obj)
  (when (hash-table-p obj)
    (let ((in (or (gethash "input_tokens" obj) 0))
          (out (or (gethash "output_tokens" obj) 0)))
      (make-llm-usage :input-tokens in :output-tokens out
                      :total-tokens (+ in out)))))

(defun %tool-call-extras (block)
  (let ((extras nil))
    (maphash (lambda (k v)
               (unless (member k '("type" "id" "name" "input") :test #'string-equal)
                 (setf extras (list* (intern (string-upcase
                                              (substitute #\- #\_ k))
                                             :keyword)
                                     v extras))))
             block)
    extras))

(defun %parse-block (block)
  (when (hash-table-p block)
    (let ((type (gethash "type" block)))
      (cond
        ((string-equal type "text")
         (let ((cites (gethash "citations" block)))
           (if cites
               (make-instance 'anthropic-text-part
                              :text (%str (gethash "text" block))
                              :citations cites)
               (make-llm-text-part :text (%str (gethash "text" block))))))
        ((string-equal type "thinking")
         (make-llm-thinking-part :text (%str (gethash "thinking" block))
                                 :signature (gethash "signature" block)))
        ((string-equal type "redacted_thinking")
         (make-llm-thinking-part :text (%str (or (gethash "data" block) ""))
                                 :signature :redacted))
        ((or (string-equal type "tool_use") (%server-call-type-p type))
         (make-instance 'anthropic-tool-call-part
                        :id (gethash "id" block)
                        :name (gethash "name" block)
                        :arguments (let ((in (gethash "input" block)))
                                     (if (stringp in)
                                         in
                                         (stack-json:encode (or in (%ht)))))
                        :server-p (%server-call-type-p type)
                        :extras (%tool-call-extras block)))
        ((or (%server-result-type-p type)
             (string-equal type "document")
             (string-equal type "container_upload"))
         (make-anthropic-block-part block))
        (t (and type (make-anthropic-block-part block)))))))

(defun %parse-message (obj requested-model)
  (let* ((blocks (llm-protocol::%as-list (and (hash-table-p obj)
                                              (gethash "content" obj))))
         (parts (remove nil (mapcar #'%parse-block blocks))))
    (make-llm-response
     :id (and (hash-table-p obj) (gethash "id" obj))
     :parts parts
     :model (or (and (hash-table-p obj) (gethash "model" obj)) requested-model)
     :finish-reason (%finish-reason (and (hash-table-p obj)
                                         (gethash "stop_reason" obj)))
     :usage (%usage (and (hash-table-p obj) (gethash "usage" obj))))))

(defmethod generate ((backend anthropic-backend) turns &key model settings
                     tools tool-choice output)
  (declare (ignore output))
  (multiple-value-bind (model body)
      (%messages-body backend turns :model model :settings settings
                      :tools tools :tool-choice tool-choice)
    (multiple-value-bind (status text)
        (%request backend :post "/messages" body)
      (%parse-message (%decode status text) model))))

(defmethod list-models ((backend anthropic-backend) &key)
  (handler-bind ((llm-http-error
                  (lambda (c)
                    (when (eql (llm-http-error-status c) 404)
                      (use-value nil c)))))
    (let ((obj (multiple-value-bind (status text)
                   (%request backend :get "/models")
                 (%decode status text))))
      (or (mapcar (lambda (m)
                    (make-llm-model-info
                     :id (if (hash-table-p m) (gethash "id" m) (%str m))
                     :owned-by "anthropic"))
                  (llm-protocol::%as-list
                   (or (and (hash-table-p obj) (gethash "data" obj)) obj)))
          (list (make-llm-model-info :id (anthropic-default-model backend)
                                     :owned-by "anthropic"))))))
