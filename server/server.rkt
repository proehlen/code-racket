#lang racket

 (require racket/date json)

 (define loggingOn #t)

;; Open file for logging
(define logFile (open-output-file "/home/peter/racket-vscode-log" #:exists 'replace))
(define (log value)
  (when loggingOn
    (display value logFile))
  (flush-output logFile))


;; Print current date and time to log file
(log (string-append 
      "Log: "
      (date->string (current-date) #t)      
      "\n"))

(define (log-message message)
  (log message)
  (log "\n"))

(define (handle-incoming-request message) 
  (log "\nRequest:\n")
  (log-message message)
  (respond message))

(define (handle-incoming-notification message) 
  (log "\nNotification:\n")
  (log-message message))

(define (handle-incoming-message message) 
  (if (hash-has-key? message 'id) 
    (handle-incoming-request message)
    (handle-incoming-notification message)))

(define ( read-json-input )
  (let ((message (read-json)))
    (handle-incoming-message message)
    (skip-to-json)))

(define (skip-to-json)
  (let ((nextChar (peek-char)))
    (if (char=? nextChar #\{)
      (read-json-input)
      (let () (read-byte) (skip-to-json)))))

(define (respond request)
  (log "\nResponse:\n")
  (cond 
    ((equal? (hash-ref request 'method) "initialize") (onInitialize))
    ((equal? (hash-ref request 'method) "shutdown") (log "Shutting down by request...\n")(exit 0))))

(define (write-crlf) (write-byte #x0D) (write-byte #x0A))

(define (onInitialize)
  (let ((capabilities (make-hash '((textDocumentSync . 1))))    ; full sync per https://github.com/Microsoft/language-server-protocol/blob/master/protocol.md#initialize
      (result (make-hash))
      (response (make-hash '((jsonrpc . "2.0")(result . "OK")(id . 0)(error . null)))))  ; TODO get id from request
    (hash-set! result 'capabilities capabilities)
    (hash-set! response 'result result)
    (log response)
    (log "\n")

    ; Pre build response body so we can add length to header
    (define body (open-output-string))
    (write-json response body)

    ; Header must be ASCII which is different from body - oh boy.
    (reencode-output-port	 (current-output-port) "ascii")
    (display "Content-Type: application/vscode-jsonrpc; charset=utf8")
    (write-crlf)
    (write 'Content-Length:)
    (write-char #\space)
    (write (string-length (get-output-string body)))
    (write-crlf)
    (write-crlf)

    ; JSON must be UTF-8 which is different from header
    (reencode-output-port	(current-output-port) "UTF-8")
    (display (get-output-string body))
    (write-crlf)
    (flush-output)))
  

(skip-to-json)