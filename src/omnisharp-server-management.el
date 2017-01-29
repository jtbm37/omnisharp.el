(defvar omnisharp--server-info (make-hash-table :test 'equal))
(defvar omnisharp-default-http "http://localhost:2000/" "Set default http address")
(defvar-local omnisharp--project-root nil)

(defun make-omnisharp--server-info (process kind)
  `((:process . ,process)
    (:kind . ,kind)
    ;; This is incremented for each request. Do not modify it in other
    ;; places.
    (:request-id . 1)
    ;; alist of (request-id . response-handler)
    (:response-handlers . nil)
    (:started? . nil)))

(defun omnisharp--get-project-info ()
  (unless omnisharp--project-root
    (setq-local omnisharp--project-root (projectile-project-root)))
  (gethash omnisharp--project-root omnisharp--server-info))

(defun omnisharp--clear-response-handlers ()
  "For development time cleaning up impossible states of response
handlers in the current omnisharp--server-info."
  (setcdr (assoc :response-handlers (omnisharp--get-project-info))
          nil))

(defmacro comment (&rest body) nil)
(comment (omnisharp--clear-response-handlers))

(defun omnisharp--send-command-to-server (api-name contents &optional response-handler async)
  "Sends the given command to the server either via http or stdio.
The variable ASYNC has no effect when not using http."
  (let* ((root (projectile-project-root))
	 (project-info (gethash root omnisharp--server-info)))
    (unless project-info
      (if (yes-or-no-p "Use http ?")
	  (setq project-info (puthash root (make-omnisharp--server-info (read-from-minibuffer "http address: " omnisharp-default-http) 'http) omnisharp--server-info))
	(error "Start server with `omnisharp-start-omnisharp-server'.")))
    (if (eq (alist-get :kind project-info) 'http)
	(omnisharp--send-command-to-server-http (concat (alist-get :process project-info) api-name) contents response-handler async)
      (omnisharp--send-command-to-server-stdio project-info api-name contents response-handler))))

(defun omnisharp--send-command-to-server-http (url contents response-handler &optional async)
  "Sends the given command via curl"
  (omnisharp-post-http-message url response-handler contents async))

(defun omnisharp--send-command-to-server-stdio (project-info api-name contents &optional response-handler)
  "Sends the given command to the server and associates a
response-handler for it. The server will respond to this request
later and the response handler will get called then.

Returns the unique request id that the request is given before
sending."
  (-let* (((&alist :process process
		   :request-id request-id) project-info)
	  (request (omnisharp--make-request-packet api-name
						   contents
						   request-id)))
    (when omnisharp-debug
      (omnisharp--log (format "--> %s %s %s"
			      request-id
			      api-name
			      (prin1-to-string request))))

    ;; update current request-id and associate a response-handler for
    ;; this request
    (setcdr (assoc :request-id project-info) (+ 1 request-id))

    ;; requests that don't require handling are still added with a
    ;; dummy handler. This means they are pending. This is required
    ;; so that omnisharp--wait-until-request-completed can know when
    ;; the requests have completed.
    (setcdr (assoc :response-handlers project-info)
	    (-concat `((,request-id . ,(or response-handler #'identity)))
		     (cdr (assoc :response-handlers project-info))))

    (process-send-string process (concat (json-encode request) "\n"))
    request-id))

(defun omnisharp--send-command-to-server-sync (&rest args)
  "Like `omnisharp--send-command-to-server' but will block until the
request responded by the server."
  (omnisharp--wait-until-request-completed
   (apply 'omnisharp--send-command-to-server args)))

(defun omnisharp--make-request-packet (api-name contents request-id)
  (-concat `((Arguments . ,contents))
           `((Command . ,api-name)
             (Seq . ,request-id))))

(defun omnisharp--handle-server-message (process message-part)
  "Parse alists from accumulated json responses in the server's
process buffer, and handle them as server events"
  (condition-case maybe-error-data
      (let* ((messages-from-server (omnisharp--read-lines-from-process-output
                                    process message-part))
             (error-message (concat
                             "The server sent an unknown json message. "
                             "Inspect the omnisharp-server process buffer "
                             "to view recent messages from the server. "
                             "Set `omnisharp-debug' to t and inspect the "
                             "*omnisharp-debug* buffer for this error specifically."))
             (json-messages (--map (omnisharp--json-read-from-string it error-message)
                                   messages-from-server)))
        ;; should use -each here since it's for side effects only, but
        ;; it can't work with vectors. -map can, so use that instead.
        (-map #'omnisharp--handle-server-event json-messages))
    (error (let ((msg (format (concat "omnisharp--handle-server-message error: %s. "
                                      "See the OmniServer process buffer for detailed server output.")
                              (prin1-to-string maybe-error-data))))
             (omnisharp--log msg)
             (message msg)))))

(defun omnisharp--log-packet? (packet)
  (and (equal "event" (cdr (assoc 'Type packet)))
       (equal "log" (cdr (assoc 'Event packet)))))

(defun omnisharp--log-log-packet (packet)
  (-let (((&alist 'LogLevel log-level
                  'Message message) (cdr (assoc 'Body packet))))
    (when (equal log-level "ERROR")
      (message (format "<-- OmniSharp server error: %s"
                       (-first-item (s-lines message)))))
    (omnisharp--log (format "%s: %s" log-level message))))

(defun omnisharp--event-packet? (packet)
  (and (equal "event" (cdr (assoc 'Type packet)))))

(defun omnisharp--response-packet? (packet)
  (equal "response" (cdr (assoc 'Type packet))))

(defun omnisharp--ignorable-packet? (packet)
  ;; todo what exactly are these? can they be ignored?
  (and (assq 'Arguments packet)
       (assq 'Command packet)))

(defun omnisharp--handle-event-packet (packet)
  (-let* ((((&alist 'Type packet-type
                    'Event event-type) packet)))
    (cond ((-contains? '("ProjectAdded" "ProjectChanged") event-type)
           (comment ignore these for now.))

          ((equal "started" event-type)
           (omnisharp--log "The server has started")
           (message "The OmniSharp server is ready. Hacks and glory await!")
           (setcdr (assoc :started? server-info) t)))))

(defun omnisharp--handle-server-event (packet)
  "Takes an alist representing some kind of Packet, possibly a
ResponsePacket or an EventPacket, and processes it depending on
its type."
  (let ((server-info (omnisharp--get-project-info)))
    (cond ((omnisharp--ignorable-packet? packet)
           nil)

          ((omnisharp--response-packet? packet)
           (omnisharp--handle-server-response-packet packet server-info))

          ((omnisharp--log-packet? packet)
           (omnisharp--log-log-packet packet))

          ((omnisharp--event-packet? packet)
           (omnisharp--handle-event-packet packet))

          (t (omnisharp--log (format "<-- Received an unknown server packet: %s"
                                     (prin1-to-string packet)))))))

(defun omnisharp--remove-response-handler (server-info request-id)
  (setcdr (assoc :response-handlers server-info)
          (--remove (= (car it) request-id)
                    (-non-nil response-handlers))))

(defun omnisharp--handle-server-response-packet (packet server-info)
  "Calls the appropriate response callback for the received packet"
  (-let (((&alist 'Message message
                  'Body body
                  'Command command
                  'Success success?
                  'Request_seq request-id) packet)
         ((&alist :response-handlers response-handlers) server-info))
    ;; try to find the matching response-handler
    (-if-let* ((id-and-handler (--first (= (car it) request-id)
                                        response-handlers)))
        (-let (((request-id . response-handler) id-and-handler))
          (condition-case maybe-error-data
              (progn
                (omnisharp--log (if (equal success? :json-false)
                                    (format "<-- %s %s: request failed"
                                            request-id
                                            command
                                            (prin1-to-string body))
                                  (format "<-- %s %s: %s"
                                          request-id
                                          command
                                          (prin1-to-string body))))
                (omnisharp--remove-response-handler server-info request-id)
                (when (equal t success?)
                  (apply response-handler (list body))))
            (error
             (progn
               (let ((msg (format
                           (concat "\n"
                                   "omnisharp--handle-server-response-packet error: \n%s.\n\n"
                                   "Tried to handle this packet: \n%s\n\n"
                                   "This can mean an error in the handler function:\n%s\n\n")
                           (prin1-to-string maybe-error-data)
                           (prin1-to-string packet)
                           (prin1-to-string response-handler))))
                 (omnisharp--log msg)
                 (omnisharp--remove-response-handler server-info request-id)
                 (message msg))))))

      (omnisharp--log (format "<-- %s %s: Warning: internal error - response has no handler: %s"
                              request-id
                              command
                              body)))))

(defun omnisharp--at-full-line? ()
  ;; all platforms use \n as newline in emacs
  (s-ends-with? "\n"
                (substring-no-properties (or (thing-at-point 'line)
                                             ""))))

(defun omnisharp--marker-at-full-line? (position-or-marker)
  (save-excursion
    (goto-char position-or-marker)
    (omnisharp--at-full-line?)))

(defun omnisharp--read-lines-from-process-output (process message-part)
  "Problem: emacs reads output from the omnisharp-roslyn subprocess
not line by line, but by some amount of characters. The way we want
to read the omnisharp-roslyn output is line by line, since each
response seems to be exactly one line long.

This function returns full lines returned from the server process that
have not been returned before."
  (when (buffer-live-p (process-buffer process))
    (with-current-buffer (process-buffer process)
      ;; previous-text-marker will change if it refers to the marker
      ;; and the marker is changed. Get it as an integer instead to
      ;; avoid mutation
      (let ((previous-text-marker (save-excursion
                                    (goto-char (process-mark process))
                                    (point))))
        ;; Insert the text, advancing the process marker.
        (goto-char (buffer-end 1))
        (insert message-part)
        ;; Return previous pending lines only when the latest line
        ;; is complete. Might be slightly slower but easier to
        ;; implement
        (when (omnisharp--marker-at-full-line? (point))
          (set-marker (process-mark process) (point))
          ;; get the start of the last inserted line
          (goto-char previous-text-marker)
          (beginning-of-line)
          (let ((text (s-lines (buffer-substring-no-properties
                                (point)
                                (process-mark process)))))
            ;; don't store messages in the process buffer unless
            ;; debugging, as they can slow emacs down when they pile
            ;; up
            (when (not omnisharp-debug) (erase-buffer))
            (--filter (not (s-blank? it)) text)))))))

;;; todo stop-process

(provide 'omnisharp-server-management)
