#lang racket

(provide markdown)
(require commonmark)

(define (markdown text #:options options)
  (let ([output null]
        [parsed (string->document text)])
    (set! output (document->html parsed))
    output))
