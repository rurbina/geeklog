#lang racket 

(provide logp
         log-prepare
         log-exec
         log-add-tag
         log-remove-tag
         log-set-level
         ;log-set-output-port
         ;log-set-error-port
         )

(define output (current-output-port))

(define default-format "@host @timestamp @uri @method @result")

(define level 99)

(define (log-set-level new-level)
  (when (number? new-level) (set! level new-level))
  level)

(define (check-level x)
  (<= x level))

(define tags (make-hash '((:all . #t)
                          (info . #t)
                          (warning . #t)
                          (error . #t))))

(define (log-add-tag tag)
  (hash-set! tags tag #t))

(define (log-remove-tag tag)
  (hash-set! tags tag #f))

(define (check-tag tag)
  (or (null? tag)
      (and (hash-ref tags ':all #f) (hash-ref tags tag #t))))

(define prepared (make-hash))

(define (log-prepare #:host (host null)
                     #:timestamp (timestamp null)
                     #:uri (uri null)
                     #:method (method null)
                     #:result (result null))
  (map (lambda (x)
         (unless (null? x)
           (hash-set prepared x x)))
       '(host timestamp uri method result)))

(define (log-exec)
  (let* ([host (hash-ref prepared 'host)]
         [timestamp (hash-ref prepared 'timestamp)]
         [uri (hash-ref prepared 'uri)]
         [method (hash-ref prepared 'method)]
         [result (hash-ref prepared 'result)])
    (fprintf output default-format)))

(define (logp message
              #:color (color null)
              #:indent (indent 0)
              #:tag tag
              #:level (level 0)
              #:nl (nl #f))
  (when (and (check-tag tag) (check-level level))
    (let ([start (if (null? color) "" (format "\e[38;5;~am" color))]
          [end (if (null? color) "" "\e[0m")]
          [tabs (if (number? indent) (make-string indent #\tab) "")])
      (when nl (set! end (string-join (list end "\n") "")))
      (fprintf output "~a~a~a~a" tabs start (if (string? message) message (format "~a " message)) end))))


