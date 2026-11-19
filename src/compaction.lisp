(in-package #:llm-protocol-anthropic)

;;; --- compaction on demand (beta compact-2026-09-04) --------------------------
;;; SERVER-COMPACT sends the conversation with "compaction": {"type":
;;; "summarize"}; the API writes no reply, only one signed `compaction` block
;;; (stop_reason "compaction"). That block then opens every later request in
;;; place of the turns it summarizes, sent back byte for byte, and each such
;;; request needs the beta header. Threshold compaction (compact_20260112,
;;; :server-compaction) cannot run on a request that carries a signed block,
;;; so it is left off those.

(defparameter +max-compaction-instructions+ 16384
  "Longest INSTRUCTIONS the API takes, in characters.")

(defmethod backend-supports-p ((backend anthropic-backend)
                               (feature (eql :compaction)))
  (eq (or (anthropic-dialect backend) :official) :official))

(defmethod compaction-part-text ((part anthropic-block-part))
  (and (compaction-part-p part)
       (gethash "content" (anthropic-block-part-block part))))

(defun %signed-compaction-block-p (b)
  (and (hash-table-p b)
       (equal "compaction" (gethash "type" b))
       (stringp (gethash "signature" b))))

(defun %body-carries-signed-compaction-p (body)
  (let ((messages (gethash "messages" body)))
    (and (vectorp messages)
         (some (lambda (m)
                 (let ((content (and (hash-table-p m) (gethash "content" m))))
                   (and (vectorp content)
                        (some #'%signed-compaction-block-p content))))
               messages))))

(defun %pending-tool-call-p (turns)
  "Ids of tool calls in the last assistant turn of TURNS that no later turn
answers (the API rejects a compaction request then), or NIL."
  (let* ((at (position :assistant turns :key #'llm-turn-role :from-end t))
         (assistant (and at (nth at turns))))
    (when assistant
      (let ((calls (loop for p in (llm-turn-parts assistant)
                         ;; Server tools are answered inside the same turn.
                         when (and (llm-tool-call-part-p p)
                                   (not (and (anthropic-tool-call-part-p p)
                                             (anthropic-tool-call-server-p p))))
                           collect (llm-tool-call-part-id p)))
            (answered (loop for turn in (nthcdr (1+ at) turns)
                            append (mapcar #'llm-tool-result-part-id
                                           (remove-if-not #'llm-tool-result-part-p
                                                          (llm-turn-parts turn))))))
        (set-difference calls answered :test #'equal)))))

(defmethod server-compact ((backend anthropic-backend) turns
                           &key model settings tools instructions)
  (check-type instructions (or null string))
  (when (and instructions
             (or (> (length instructions) +max-compaction-instructions+)
                 (zerop (length (string-trim '(#\Space #\Tab #\Newline) instructions)))))
    (error 'llm-error
           :message (format nil "compaction instructions must be 1..~d non-blank characters"
                            +max-compaction-instructions+)))
  (let ((turns (coerce-turns turns)))
    (when (%pending-tool-call-p turns)
      (error 'llm-error
             :message "compaction: the last assistant turn calls a tool with no result yet"))
    (multiple-value-bind (model body)
        (%messages-body backend turns :model model :settings settings :tools tools)
      ;; The API rejects these on a summarization call.
      (dolist (key '("context_management" "stop_sequences" "tool_choice"))
        (remhash key body))
      (setf (gethash "compaction" body)
            (%ht "type" "summarize" "instructions" instructions))
      (multiple-value-bind (status text retry-after)
          (let ((*request-betas* (%body-betas body)))
            (%request backend :post "/messages" body))
        (let* ((response (%parse-message (%decode status text retry-after) model))
               (block (find-if (lambda (p)
                                 (and (compaction-part-p p) (compaction-part-text p)))
                               (llm-response-parts response))))
          (if block
              (values (append (remove :system turns :key #'llm-turn-role :test-not #'eq)
                              (list (make-llm-turn :role :assistant :parts (list block))))
                      t response)
              (values turns nil response)))))))

;;; --- part state ----------------------------------------------------------------
;;; Anthropic parts carry JSON (blocks, citations, server tool extras); their
;;; state keeps it as JSON text, so a journal replays the exact bytes — a
;;; compaction block whose content or signature changed is rejected.

(defmethod part-state ((part anthropic-block-part))
  (list :type :anthropic-block
        :json (stack-json:encode (anthropic-block-part-block part))))

(defmethod state-part ((type (eql :anthropic-block)) state)
  (make-anthropic-block-part (stack-json:decode (getf state :json))))

(defmethod part-state ((part anthropic-text-part))
  (list :type :anthropic-text
        :text (llm-text-part-text part)
        :citations (and (anthropic-text-citations part)
                        (stack-json:encode (anthropic-text-citations part)))))

(defmethod state-part ((type (eql :anthropic-text)) state)
  (make-instance 'anthropic-text-part
                 :text (or (getf state :text) "")
                 :citations (and (getf state :citations)
                                 (stack-json:decode (getf state :citations)))))

(defmethod part-state ((part anthropic-tool-call-part))
  (let ((extras (make-hash-table :test 'equal)))
    (loop for (k v) on (anthropic-tool-call-extras part) by #'cddr
          do (setf (gethash (%option-key k) extras) v))
    (list :type :anthropic-tool-call
          :id (llm-tool-call-part-id part)
          :name (llm-tool-call-part-name part)
          :arguments (or (llm-tool-call-part-arguments part) "{}")
          :server-p (and (anthropic-tool-call-server-p part) t)
          :extras (and (plusp (hash-table-count extras))
                       (stack-json:encode extras)))))

(defmethod state-part ((type (eql :anthropic-tool-call)) state)
  (make-instance 'anthropic-tool-call-part
                 :id (getf state :id)
                 :name (getf state :name)
                 :arguments (or (getf state :arguments) "{}")
                 :server-p (getf state :server-p)
                 :extras (and (getf state :extras)
                              (%tool-call-extras (stack-json:decode (getf state :extras))))))
