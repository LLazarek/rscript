#lang at-exp racket

(provide (all-defined-out))

(define (build-path-string . args)
  (path->string (apply build-path args)))
(define (simple-form-path-string p)
  (path->string (simple-form-path p)))
(define (find-relative-path-string base path)
  (path->string (find-relative-path (simple-form-path base)
                                    (simple-form-path path))))

(define (basename p #:with-directory? [dir? #f])
  (define-values {dir name _2} (split-path p))
  (define name-str (path->string name))
  (define dir-str (path->string dir))
  (if dir?
      (values dir-str name-str)
      name-str))

(define/contract (call-with-extended-environment env-vars thunk)
  ((hash/c string-environment-variable-name? string-no-nuls?)
   (-> any)
   . -> .
   any)

  (parameterize ([current-environment-variables
                  (environment-variables-copy (current-environment-variables))])
    (for ([(k v) (in-hash env-vars)])
      (putenv k v))
    (thunk)))

(define path-to-existant-directory?
  (and/c path-string? directory-exists?))
(define path-to-existant-file?
  (and/c path-string? file-exists?))


(define (user-prompt! msg [type 'Y/n])
  (display @~a{@msg [Y/n]: })
  (flush-output)
  (match (read-char)
    [(or #\n #\N) #f]
    [else #t]))

(define/contract (replace-in-file! path pat replacement)
  (path-to-existant-file? (or/c string? regexp?) string? . -> . void?)

  (define contents (file->string path))
  (define contents/replaced
    (regexp-replace pat contents replacement))
  (with-output-to-file path
    #:exists 'truncate
    (thunk (displayln contents/replaced))))

(define (system/string cmd)
  (call-with-output-string
   (λ (out)
     (parameterize ([current-output-port out]
                    [current-error-port out])
       (system cmd)))))
