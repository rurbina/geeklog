#lang racket

(provide all-defined-out)

(require geeklog/load
         geeklog/structs)

;; search for documents
(define (search-docs
         #:tags            [tags         '()]
         #:or-tags         [or-tags      '()]
         #:no-tags         [no-tags      '()]
         #:and-tags        [and-tags     '()]
         #:headers-only    [headers-only #t]
         #:sort            [sort-key     'name]
         #:reverse         [sort-reverse #f]
         #:no-future       [no-future    #f]
         #:older-than      [before-secs  0]
         #:newer-than      [after-secs   0]
         #:settings        [settings     (make-hash)]
         #:data-path       [data-path    null])
  (when (null? data-path)
    (set! data-path (string->path (string-append (hash-ref settings 'base-path) "/" (hash-ref settings 'data-path)))))
  (let ([results '()] [file-path null] [now (current-seconds)])
    (set! results
          (for/list ([doc (for/list ([file (directory-list data-path)]
                                     #:when (file-exists? (build-path data-path file)))
                            (set! file-path (build-path data-path file))
                            ;;(load-doc (path->string file-path) #:headers-only headers-only))]
                            (with-handlers ([exn:fail? (lambda (e)
                                                         (eprintf "\t\terror is ~v\n" e)
                                                         void)])
                              (eprintf "	\e[33msearch: \e[0m~v\n" (path->string file-path))
                              (load-doc (path->string (path-replace-extension (last (explode-path file-path)) #""))
                                        #:path file-path
                                        #:settings settings
                                        #:headers-only headers-only)))]
                     #:when (and [gldoc? doc]
                                 [or (empty? tags)
                                     (subset? tags (hash-ref (gldoc-headers doc) 'tags '()))]
                                 [or (empty? or-tags)
                                     (not (empty? (set-intersect or-tags (hash-ref (gldoc-headers doc) 'tags '()))))]
                                 [or (empty? and-tags)
                                     (subset? and-tags (hash-ref (gldoc-headers doc) 'tags '()))]
                                 [or (empty? no-tags)
                                     (not (subset? no-tags (hash-ref (gldoc-headers doc) 'tags '())))]
                                 [or (< (hash-ref (gldoc-headers doc) 'timestamp (add1 now)) now)
                                     (not no-future)]
                                 [or (= before-secs 0)
                                     (< (hash-ref (gldoc-headers doc) 'timestamp before-secs) before-secs)]
                                 [or (= after-secs 0)
                                     (> (hash-ref (gldoc-headers doc) 'timestamp after-secs) after-secs)]
                                 #t))
            doc))
    ;; sorting
    (set! results (sort results (lambda (a b) (gldoc-sort a b sort-key))))
    (if sort-reverse (reverse results) results)))

