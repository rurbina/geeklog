#lang racket/base

(require db)

(define geeklog/comments #t)

(define schema "
create table settings ();
create table comments (
  id int,
  doc varchar,
  name varchar,
  hash varchar,
  message varchar,
  post_date timestamp
);
")

;; comment database
(define dbc (sqlite3-connect #:database "comments.db" #:mode 'create))

;; regenerate database when needed
(with-handlers
  ([exn:fail? (lambda (e) (logp "reusing existing comments.db\n" #:tag 'info))])
  (query-exec metadb schema))

;; insert new comment
(define (comment-add #:doc doc
                     #:name name
                     #:hash hash
                     #:message msg
                     #:timestamp ts)
  (query-exec dbc
              "insert into comments (id, doc, name, hash, message, post_date)
		values ((select max(id) + 1 from comments where doc = $1), $1, $2, $3, $4, now())"
              doc name hash msg))

;; get comments for doc
(define (comments-get #:doc doc)
  (for/list ([row (query-rows
                   dbc
                   "select doc, id, name, hash, message, post_date from comments where doc = $1 order by id asc" doc)])
    (make-hash (for/list ([key '(doc id name hash message timestamp)]
                          [value row])
                 (cons key value)))))

