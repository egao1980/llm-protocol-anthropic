(in-package #:llm-protocol-anthropic/tests)

;;; API key resolution (llm-protocol#10) and redaction (http-backend-async#54).

(defparameter +key+ "sk-ant-redact-4b8e2f17c9")
(defparameter +env-key+ "sk-ant-from-env-91d3a0")

(defun %token-free-p (string)
  (not (or (search +key+ string) (search +env-key+ string))))

(defmacro %with-env ((&rest bindings) &body body)
  "Bind env vars around BODY. NIL means unset (\"\": %ENV treats it as absent)."
  (let ((saved (gensym)))
    `(let ((,saved (mapcar (lambda (n) (cons n (uiop:getenv n)))
                           (list ,@(mapcar #'first bindings)))))
       (unwind-protect
            (progn
              ,@(loop for (name value) in bindings
                      collect `(setf (uiop:getenv ,name) (or ,value "")))
              ,@body)
         (loop for (n . v) in ,saved
               do (setf (uiop:getenv n) (or v "")))))))

(defun %auth-header (backend)
  (cdr (assoc "authorization" (llm-protocol-anthropic::%headers backend)
              :test #'string-equal)))

(deftest anthropic-api-key-not-supplied-reads-env
  (%with-env (("ANTHROPIC_API_KEY" +env-key+) ("ANTHROPIC_AUTH_TOKEN" nil))
    (let ((b (llm-protocol-anthropic:make-anthropic-backend)))
      (ok (equal +env-key+ (llm-protocol-anthropic:anthropic-api-key b)))
      (ok (equal (concatenate 'string "Bearer " +env-key+)
                 (http-protocol:reveal (%auth-header b)))))
    (ok (equal +env-key+ (llm-protocol-anthropic:anthropic-api-key
                          (llm-protocol-anthropic:make-anthropic-backend
                           :api-key :env)))))
  (%with-env (("ANTHROPIC_API_KEY" nil) ("ANTHROPIC_AUTH_TOKEN" +env-key+))
    (ok (equal +env-key+ (llm-protocol-anthropic:anthropic-api-key
                          (llm-protocol-anthropic:make-anthropic-backend))))))

(deftest anthropic-api-key-nil-or-none-means-no-key
  (%with-env (("ANTHROPIC_API_KEY" +env-key+) ("ANTHROPIC_AUTH_TOKEN" +env-key+))
    (dolist (none (list nil :none ""))
      (let ((b (llm-protocol-anthropic:make-anthropic-backend :api-key none)))
        (ok (null (llm-protocol-anthropic:anthropic-api-key b)))
        (ok (null (%auth-header b)))))))

(deftest anthropic-api-key-explicit
  (%with-env (("ANTHROPIC_API_KEY" +env-key+))
    (let ((b (llm-protocol-anthropic:make-anthropic-backend :api-key +key+)))
      (ok (equal +key+ (llm-protocol-anthropic:anthropic-api-key b))))
    (let ((b (llm-protocol-anthropic:make-anthropic-backend
              :api-key (http-protocol:make-secret +key+))))
      (ok (equal +key+ (llm-protocol-anthropic:anthropic-api-key b)))
      (setf (llm-protocol-anthropic:anthropic-api-key b) nil)
      (ok (null (%auth-header b)))))
  (ok (signals (llm-protocol-anthropic:make-anthropic-backend :api-key 42)
               'llm-protocol:llm-error)))

(deftest anthropic-key-never-prints
  (let* ((bt nil)
         (seen nil)
         (b (llm-protocol-anthropic:make-anthropic-backend
             :api-key +key+
             :base-url "http://example.invalid/v1"
             :request-fn
             (lambda (method url &key headers content want-stream)
               (declare (ignore method url content want-stream))
               (setf seen headers
                     bt (with-output-to-string (s)
                             (uiop:print-backtrace :stream s :count 60)))
               (values 401 "{\"error\":{\"message\":\"invalid api key\"}}"))))
         (err (handler-case (llm-protocol:generate b "hi" :model "claude-sonnet-4-20250514")
                (llm-protocol:llm-http-error (e) e))))
    ;; the key is on the headers, as SECRETs
    (ok (http-protocol:secret-p (cdr (assoc "x-api-key" seen :test #'string-equal))))
    (ok (http-protocol:secret-p (cdr (assoc "authorization" seen :test #'string-equal))))
    (ok (equal (concatenate 'string "Bearer " +key+)
               (http-protocol:reveal
                (cdr (assoc "authorization" seen :test #'string-equal)))))
    ;; ...but no printed form shows it
    (ok (typep err 'llm-protocol:llm-http-error))
    (ok (%token-free-p (format nil "~A ~S" err err)))
    (ok (%token-free-p (prin1-to-string seen)))
    (ok (%token-free-p (prin1-to-string b)))
    (ok (%token-free-p (with-output-to-string (s) (describe b s))))
    (ok (search "headers" (string-downcase bt)))
    (ok (%token-free-p bt))))
