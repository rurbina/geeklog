#lang racket

(require geeklog/load
         geeklog/structs
         geeklog/log)

(provide search-docs)

;; search for documents
(define (search-docs
         #:settings        settings
         #:tags            [tags           '()]
         #:or-tags         [or-tags        '()]
         #:no-tags         [no-tags        '()]
         #:and-tags        [and-tags       '()]
         #:or-categories   [or-cats        '()]
         #:headers-only    [headers-only    #f]
         #:summary-only    [summary-only    #t]
         #:sort            [sort-key     'name]
         #:reverse         [sort-reverse    #f]
         #:no-future       [no-future       #f]
         #:older-than      [before-secs      0]
         #:newer-than      [after-secs       0]
         #:unparsed        [unparsed        #f]
         #:range-start     [range-start      0]
         #:range-count     [range-count     10]
         #:path            [path          null]
         #:data-path       [data-path     null])
  (define (log x #:indent (t null) #:level (l 1)) (logp x #:tag 'search #:indent (if (number? t) t 0) #:level l))
  (log "search-docs: " #:indent 2)
  (when (null? data-path)
    (set! data-path (string->path (string-append (hash-ref settings 'base-path) "/" (hash-ref settings 'data-path)))))
  (when (and (string? path) (not (string=? path "")))
    (set! data-path (build-path data-path (string->path path))))
  (let ([results '()]
        [file-path null]
        [now (current-seconds)]
        [doc-tags (lambda (doc) (hash-ref (gldoc-headers doc) 'tags '()))]
        [doc-cats (lambda (doc) (hash-ref (gldoc-headers doc) 'categories '()))])
    (log "searching... ")
    (set! results
          (for/list ([doc (for/list ([file (directory-list data-path)]
                                     #:when (file-exists? (build-path data-path file)))
                            (log `(file . ,file))
                            (set! file-path (build-path data-path file))
                            (with-handlers ([exn:fail? (lambda (e)
                                                         (logp (format "search-docs error: ~v\n" e) #:tag 'warning)
                                                         void)])
                              (load-doc (path->string (path-replace-extension (last (explode-path file-path)) #""))
                                        #:path file-path
                                        #:path-prefix path
                                        #:settings settings
                                        #:headers-only #t)))]
                     #:when (and [gldoc? doc]
                                 [or (empty? tags)
                                     (subset? tags (doc-tags doc))]
                                 [or (empty? or-tags)
                                     (not (empty? (set-intersect or-tags (doc-tags doc))))]
                                 [or (empty? and-tags)
                                     (subset? and-tags (doc-tags doc))]
                                 [or (empty? or-cats)
                                     (not (empty? (set-intersect or-cats (doc-cats doc))))]
                                 [or (empty? no-tags)
                                     (not (subset? no-tags (doc-tags doc)))]
                                 [or (< (hash-ref (gldoc-headers doc) 'timestamp (add1 now)) now)
                                     (not no-future)]
                                 [or (= before-secs 0)
                                     (< (hash-ref (gldoc-headers doc) 'timestamp before-secs) before-secs)]
                                 [or (= after-secs 0)
                                     (> (hash-ref (gldoc-headers doc) 'timestamp after-secs) after-secs)]
                                 #t))
            doc))
    (log "sorting... ")
    ;; sort and trim range
    (set! results (sort results (lambda (a b) (gldoc-sort a b sort-key))))
    (when sort-reverse (set! results (reverse results)))
    (set! results (cond ((< (length results) range-start)
                         '())
                        (else
                         (list-tail results range-start))))
    (set! results (cond ((< (length results) range-count) results)
                        (else (take results range-count))))
    (log "done\n")
    ;; load and process actual data
    (unless headers-only
      (set! results
            (for/list ([doc results])
              (load-doc null
                        #:path (hash-ref (gldoc-headers doc) 'path)
                        #:path-prefix path
                        #:unparsed unparsed
                        #:summary-only summary-only
                        #:settings settings))))
    results))
