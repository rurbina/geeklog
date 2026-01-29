#lang racket

(provide markdown)
(require commonmark
         commonmark/struct
         srfi/13)

(define (pre-wikilinks text)
  (if (not (regexp-match? #px"\\[\\[.*?\\]\\]" text)) text
      (let ([pos1 (string-contains text "[[")])
        (let ([before (substring text 0 pos1)]
              [pos2 (string-contains (substring text pos1) "]]")])
          (let ([content (substring text (+ 2 pos1) (+ pos1 pos2))]
                [after (substring text (+ 2 pos1 pos2))])
            (list before (link content content #f) (pre-wikilinks after)))))))

(define (walk-paragraphs blocks func #:options [options (make-hash)])
  (for/list ([item blocks])
    (if (not (paragraph? item)) item
        (let ([content (paragraph-content item)])
          (paragraph (for/list ([item (if (list? content) content (list content))])
                       (if (string? item) (func item) item)))))))

(define (markdown text #:options options)
  (let ([output null]
        [parsed (string->document text)])
    (set! parsed (document (walk-paragraphs (document-blocks parsed) pre-wikilinks) '()))
    (set! output (document->html parsed))
    output))
