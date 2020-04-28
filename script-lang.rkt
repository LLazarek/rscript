#lang racket

(provide (all-from-out racket)
         (all-from-out racket/runtime-path)
         define-runtime-paths
         main
         (all-from-out "util.rkt")
         (all-from-out "cmdline.rkt"))

(require racket/runtime-path
         "cmdline.rkt"
         syntax/parse/define
         "util.rkt"
         (for-syntax racket/syntax))

(define-simple-macro (define-runtime-paths [name:id path:expr] ...)
  ;; Ugly dance to get the srcloc on the `define-runtime-path` exprs to be the
  ;; original location, since the srcloc of the expression is what it uses to
  ;; determine the path
  #:do [(define defs (syntax-e #'[(define-runtime-path name path) ...]))
        (define (replace-srcloc def)
          (define parts (syntax-e def))
          (datum->syntax this-syntax parts this-syntax))]
  #:with [def ...] (map replace-srcloc defs)
  (begin def ...))

(define-simple-macro (main #:arguments {[flags-name:id args-name:id] cmdline-e ...}
                           {~seq #:check [check:expr msg:expr]} ...
                           . body)
  (module+ main
    (match-define (cons flags-name args-name)
      (command-line/declarative cmdline-e ...))
    (unless check (raise-user-error msg)) ...
    . body))

