#lang racket

(provide (struct-out gldoc))

(struct gldoc (headers
               body
               [summary #:auto]
               [parsed-body #:auto]
               [parsed-summary #:auto])
  #:mutable
  #:auto-value "")

