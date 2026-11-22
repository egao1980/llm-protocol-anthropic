(in-package #:llm-protocol-anthropic/tests)

;;; llm-protocol-openai#13, same here: request timeouts are settings, and a stream is bounded by
;;; silence, not by a wall clock. A slow model's token stream (4-10 tok/s
;;; for minutes) must run to the end. The fake origin below answers over
;;; real HTTP through http-backend-async x libuv, with sub-second pacing so
;;; a 1-2 s "old cap" stands in for the old 180 s one.

(defmacro %with-async-http (&body body)
  "Bind http-backend-async × libuv like http-parity WITH-PARITY."
  `(let* ((eb (event-backend-libuv:make-libuv-backend))
          (el (event-protocol:make-event-loop eb))
          (http-backend-async:*event-backend-maker* (lambda () eb)))
     (event-protocol:with-event-backend (eb)
       (event-protocol:with-event-loop-var (el)
         (let ((http-protocol:*http-backend* (http-backend-async:make-async-backend)))
           ,@body)))))

(defun %read-crlf-line (stream)
  (let ((out (make-array 0 :element-type '(unsigned-byte 8)
                           :adjustable t :fill-pointer 0)))
    (loop for b = (read-byte stream nil nil)
          while b
          do (vector-push-extend b out)
             (when (and (>= (length out) 2)
                        (= (aref out (- (length out) 2)) 13)
                        (= (aref out (- (length out) 1)) 10))
               (return)))
    (when (plusp (length out))
      (string-right-trim '(#\Return #\Newline)
                         (babel:octets-to-string out :encoding :utf-8 :errorp nil)))))

(defun %slow-read-request (stream)
  "Read the request line, headers and Content-Length body. NIL at EOF."
  (let ((line (%read-crlf-line stream))
        (length 0))
    (when line
      (loop for h = (%read-crlf-line stream)
            while (and h (plusp (length h)))
            do (let ((colon (position #\: h)))
                 (when (and colon (string-equal "content-length" (subseq h 0 colon)))
                   (setf length (or (parse-integer h :start (1+ colon) :junk-allowed t)
                                    0)))))
      (when (plusp length)
        (read-sequence (make-array length :element-type '(unsigned-byte 8)) stream))
      t)))

(defun %slow-write (stream string)
  (write-sequence (babel:string-to-octets string :encoding :utf-8) stream)
  (force-output stream))

(defun %crlf (&rest lines)
  (format nil "~{~A~C~C~}"
          (loop for l in lines collect l collect #\Return collect #\Newline)))

(defun %slow-sse (stream steps)
  "Chunked text/event-stream. STEPS: (seconds . sse-text) pairs, the delay
   before each; a NIL text stalls that long and hangs up."
  (%slow-write stream (%crlf "HTTP/1.1 200 OK" "Content-Type: text/event-stream"
                             "Transfer-Encoding: chunked" "Connection: close" ""))
  (dolist (step steps)
    (sleep (car step))
    (unless (cdr step) (return-from %slow-sse))
    (let ((octets (babel:string-to-octets (cdr step) :encoding :utf-8)))
      (%slow-write stream (format nil "~X~C~C" (length octets) #\Return #\Newline))
      (write-sequence octets stream)
      (%slow-write stream (%crlf ""))))
  (%slow-write stream (%crlf "0" "")))

(defun %slow-json (stream delay text)
  "A Messages API answer with TEXT after DELAY seconds of silence."
  (sleep delay)
  (let ((body (babel:string-to-octets
               (stack-json:encode
                (%ht "id" "msg_1" "type" "message" "role" "assistant" "model" "local"
                     "stop_reason" "end_turn"
                     "usage" (%ht "input_tokens" 1 "output_tokens" 1)
                     "content" (vector (%ht "type" "text" "text" text))))
               :encoding :utf-8)))
    (%slow-write stream (%crlf "HTTP/1.1 200 OK" "Content-Type: application/json"
                               (format nil "Content-Length: ~D" (length body))
                               "Connection: close" ""))
    (write-sequence body stream)
    (force-output stream)))

(defun call-with-slow-origin (respond fn)
  "Serve each connection with (RESPOND stream) after reading the request;
   call FN with the /v1 base URL."
  (let* ((server (usocket:socket-listen "127.0.0.1" 0 :reuseaddress t
                                                      :element-type '(unsigned-byte 8)))
         (port (usocket:get-local-port server))
         (thread
           (bt:make-thread
            (lambda ()
              (loop
                (handler-case
                    (let ((c (usocket:socket-accept server
                                                    :element-type '(unsigned-byte 8))))
                      (bt:make-thread
                       (lambda ()
                         (unwind-protect
                              (ignore-errors
                               (let ((s (usocket:socket-stream c)))
                                 (when (%slow-read-request s)
                                   (funcall respond s))))
                           (ignore-errors (usocket:socket-close c))))
                       :name "llm-anthropic-slow-conn"))
                  (error () (return)))))
            :name "llm-anthropic-slow-origin")))
    (unwind-protect (funcall fn (format nil "http://127.0.0.1:~D/v1" port))
      (ignore-errors (usocket:socket-close server))
      (when (bt:thread-alive-p thread)
        (ignore-errors (bt:destroy-thread thread))))))

(defmacro with-slow-origin ((base-var respond) &body body)
  `(call-with-slow-origin ,respond (lambda (,base-var) (%with-async-http ,@body))))

(defun %event (type &rest kvs)
  (%sse-block (stack-json:encode (apply #'%ht "type" type kvs)) type))

(defun %text-delta (text)
  (%event "content_block_delta" "index" 0
          "delta" (%ht "type" "text_delta" "text" text)))

(defun %stream-head ()
  (concatenate 'string
               (%event "message_start"
                       "message" (%ht "id" "msg_1" "model" "local"
                                      "usage" (%ht "input_tokens" 1 "output_tokens" 0)))
               (%event "content_block_start" "index" 0
                       "content_block" (%ht "type" "text" "text" ""))))

(defun %token-steps (n seconds)
  "N text deltas SECONDS apart, then the block / message stop events."
  (append (list (cons 0 (%stream-head)))
          (loop for i below n
                collect (cons seconds (%text-delta (format nil "t~D " i))))
          (list (cons 0 (concatenate
                         'string
                         (%event "content_block_stop" "index" 0)
                         (%event "message_delta"
                                 "delta" (%ht "stop_reason" "end_turn")
                                 "usage" (%ht "output_tokens" n))
                         (%event "message_stop"))))))

(defun %token-text (n)
  (format nil "~{t~D ~}" (loop for i below n collect i)))

(defun %timed (thunk)
  "→ (values result-or-condition seconds)."
  (let ((start (get-internal-real-time)))
    (values (handler-case (funcall thunk)
              (http-protocol:http-timeout-error (e) e))
            (/ (- (get-internal-real-time) start) internal-time-units-per-second))))

(defun %backend (base &rest args)
  (apply #'llm-protocol-anthropic:make-anthropic-backend
         :base-url base :api-key :none :default-model "local" args))

(deftest steady-stream-outlives-the-old-cap
  ;; 10 tokens 0.25 s apart: 2.5 s in all, past a 1 s request timeout and a
  ;; 1 s idle timeout, but never silent for 1 s.
  (with-slow-origin (base (lambda (s) (%slow-sse s (%token-steps 10 0.25))))
    (let ((b (%backend base :request-timeout 1 :stream-idle-timeout 1)))
      (multiple-value-bind (r secs)
          (%timed (lambda () (llm-protocol:stream-generate b "hi" :model "local")))
        (ok (typep r 'llm-protocol:llm-response) (format nil "~A" r))
        (when (typep r 'llm-protocol:llm-response)
          (ok (equal (%token-text 10) (llm-protocol:llm-response-text r)))
          (ok (eq :stop (llm-protocol:llm-response-finish-reason r))))
        (ok (> secs 2.0) (format nil "the stream ran ~,2F s" secs))))))

(deftest silent-stream-fails-on-the-idle-timeout
  (with-slow-origin (base (lambda (s)
                            (%slow-sse s (list (cons 0.1 (concatenate 'string (%stream-head) (%text-delta "Hel")))
                                               (cons 6 nil)))))
    (let ((b (%backend base :stream-idle-timeout 1)))
      (multiple-value-bind (c secs)
          (%timed (lambda () (llm-protocol:stream-generate b "hi" :model "local")))
        (ok (typep c 'http-protocol:http-timeout-error) (format nil "~A" c))
        (ok (search "read timed out" (princ-to-string c)) (princ-to-string c))
        (ok (< 0.9 secs 4.0) (format nil "failed after ~,2F s" secs))))))

(deftest request-timeout-initarg-caps-a-non-streaming-call
  (with-slow-origin (base (lambda (s) (%slow-json s 2 "late")))
    (multiple-value-bind (c secs)
        (%timed (lambda ()
                  (llm-protocol:generate (%backend base :request-timeout 1) "hi"
                                         :model "local")))
      (ok (typep c 'http-protocol:http-timeout-error) (format nil "~A" c))
      (ok (< secs 1.9) (format nil "failed after ~,2F s" secs)))
    (let ((r (llm-protocol:generate (%backend base :request-timeout 5) "hi"
                                    :model "local")))
      (ok (equal "late" (llm-protocol:llm-response-text r))))
    (let ((r (llm-protocol:generate (%backend base :request-timeout nil) "hi"
                                    :model "local")))
      (ok (equal "late" (llm-protocol:llm-response-text r)) "NIL: no cap"))))

(defmacro %with-env ((&rest bindings) &body body)
  "Set env vars for BODY; restore (empty = unset for %ENV) after."
  (let ((olds (loop repeat (length bindings) collect (gensym "OLD"))))
    `(let ,(loop for (name) in bindings for old in olds
                 collect `(,old (or (uiop:getenv ,name) "")))
       (unwind-protect
            (progn
              ,@(loop for (name value) in bindings
                      collect `(setf (uiop:getenv ,name) ,value))
              ,@body)
         ,@(loop for (name) in bindings for old in olds
                 collect `(setf (uiop:getenv ,name) ,old))))))

(defun %env-settable-p ()
  (ignore-errors
   (%with-env (("LLM_ANTHROPIC_ENV_PROBE" "1"))
     (equal "1" (uiop:getenv "LLM_ANTHROPIC_ENV_PROBE")))))

(deftest timeouts-from-the-environment
  (if (%env-settable-p)
      (%check-timeouts-from-env)
      (skip "this Lisp cannot set environment variables")))

(defun %check-timeouts-from-env ()
  (%with-env (("ANTHROPIC_TIMEOUT" "1") ("ANTHROPIC_STREAM_IDLE_TIMEOUT" "none")
              ("ANTHROPIC_CONNECT_TIMEOUT" "7.5"))
    (let ((b (llm-protocol-anthropic:make-anthropic-backend)))
      (ok (eql 1 (llm-protocol-anthropic:anthropic-request-timeout b)))
      (ok (null (llm-protocol-anthropic:anthropic-stream-idle-timeout b)))
      (ok (= 7.5 (llm-protocol-anthropic:anthropic-connect-timeout b))))
    (ok (eql 9 (llm-protocol-anthropic:anthropic-request-timeout
                (llm-protocol-anthropic:make-anthropic-backend :request-timeout 9)))
        "an initarg wins over the env")
    (with-slow-origin (base (lambda (s) (%slow-json s 2 "late")))
      (multiple-value-bind (c secs)
          (%timed (lambda () (llm-protocol:generate (%backend base) "hi" :model "local")))
        (ok (typep c 'http-protocol:http-timeout-error) (format nil "~A" c))
        (ok (< secs 1.9) (format nil "ANTHROPIC_TIMEOUT=1 failed after ~,2F s" secs)))))
  (%with-env (("ANTHROPIC_TIMEOUT" "soon"))
    (ok (signals (llm-protocol-anthropic:make-anthropic-backend)
                 'llm-protocol:llm-error)
        "a bad value is an error, not a silent default"))
  (%with-env (("ANTHROPIC_TIMEOUT" "") ("ANTHROPIC_STREAM_IDLE_TIMEOUT" "")
              ("ANTHROPIC_CONNECT_TIMEOUT" ""))
    (let ((b (llm-protocol-anthropic:make-anthropic-backend)))
      (ok (eql 600 (llm-protocol-anthropic:anthropic-request-timeout b)))
      (ok (eql 300 (llm-protocol-anthropic:anthropic-stream-idle-timeout b)))
      (ok (eql 30 (llm-protocol-anthropic:anthropic-connect-timeout b))))))

(deftest request-timeout-spec-shape
  (let ((b (llm-protocol-anthropic:make-anthropic-backend
            :request-timeout 600 :stream-idle-timeout 300 :connect-timeout 30)))
    (ok (equal '(:connect 30 :read 300)
               (llm-protocol-anthropic::%request-timeout-spec b t))
        "streams: idle only, no total")
    (ok (equal '(:connect 30 :total 600)
               (llm-protocol-anthropic::%request-timeout-spec b nil))))
  (ok (signals (llm-protocol-anthropic:make-anthropic-backend :request-timeout -1)
               'llm-protocol:llm-error)))
