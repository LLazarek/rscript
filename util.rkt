#lang at-exp racket

(provide (all-defined-out))

(require syntax/parse/define)

(define (build-path-string . args)
  (path->string (apply build-path args)))
(define (simple-form-path-string p)
  (path->string (simple-form-path p)))
(define (find-relative-path-string base path)
  (path->string (find-relative-path (simple-form-path base)
                                    (simple-form-path path))))

(define (basename p #:with-directory? [dir? #f])
  (define-values {dir-path name-path}
    (match (explode-path (simple-form-path p))
      [(list name)
       (values (current-directory)
               name)]
      [(list parent-path-parts ... name)
       (values (apply build-path parent-path-parts) name)]))
  (define dir-str (path->string dir-path))
  (define name-str (path->string name-path))
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

(define/contract (replace-in-file! path pat replacement)
  (path-to-existant-file? (or/c string? regexp?) string? . -> . void?)

  (define contents (file->string path))
  (define contents/replaced
    (regexp-replace pat contents replacement))
  (with-output-to-file path
    #:exists 'truncate
    (thunk (displayln contents/replaced))))

(define (with-output-to-string* thunk)
  (define results (box #f))
  (values (with-output-to-string
            (λ _ (call-with-values thunk (λ vals (set-box! results vals)))))
          (unbox results)))
(define (system/string cmd #:with-exit? [with-exit? #f])
  (match-define-values {output (list ok?)}
                       (with-output-to-string* (thunk (system cmd))))
  (if with-exit?
      (values output ok?)
      output))
(define (system*/string #:with-exit? [with-exit? #f]
                        . cmd-parts)
  (match-define-values {output (list ok?)}
                       (with-output-to-string* (thunk (apply system* cmd-parts))))
  (if with-exit?
      (values output ok?)
      output))

(define (read-user-input-line! msg)
  (display (~a msg " "))
  (flush-output)
  (string-trim (read-line)))

(define (user-prompt!* msg
                       raw-options
                       [none-result 'none]
                       #:retry-on-none? [retry-on-none? #t]
                       #:normalize [normalize string-downcase])
  (define options (map (compose1 normalize ~a) raw-options))
  (define answer
    (string-trim
     (normalize
      (read-user-input-line!
       @~a{@msg [{@(first options)}/@(string-join (rest options) "/")]: }))))
  (or (and (string=? answer "") (first raw-options))
      (for/first ([raw-option (in-list raw-options)]
                  [option-str (in-list options)]
                  #:when (string=? answer option-str))
        raw-option)
      (if retry-on-none?
          (user-prompt!* msg
                         raw-options
                         none-result
                         #:retry-on-none? #t)
          none-result)))

(define (user-prompt! msg)
  (match (user-prompt!* msg '(y n))
    ['y
     (displayln "You answered yes")
     #t]
    [else
     (displayln "You answered no")
     #f]))

(define-simple-macro (user-prompt*!/dispatch {~optional lead-msg}
                                             {~alt {~once [default-key
                                                            #:default
                                                            default-description
                                                            default-action ...]}
                                                   [key
                                                    description
                                                    action ...]} ...)
  (match (user-prompt!* @~a{
                            @(string-append {~? {~@ lead-msg "\n"}})@;
                            @default-key or empty : @default-description
                            @string-join[(list @~a{@key : @description} ...) "\n"]
                            }
                        '(default-key key ...))
    [default-key default-action ...]
    [key action ...]
    ...))

(define system-temp-dir (find-system-path 'temp-dir))
;; Call f with a new temp directory.
;; Delete the directory after f returns.
;; Result is whatever f produces.
(define (call-with-temp-directory f
                                  #:name-seed [seed "sc"]
                                  #:name [name (~a seed (current-milliseconds))]
                                  #:in [parent system-temp-dir])
  (define temp-dir (build-path parent name))
  (dynamic-wind
    (thunk (make-directory temp-dir))
    (thunk (f temp-dir))
    (thunk (delete-directory/files temp-dir))))

(define (file-name-string-from-path f)
  (define-values {_1 name _2} (split-path f))
  (path->string name))

(define ((path-ends-with name) p)
  (define p-name (file-name-string-from-path p))
  (string=? p-name name))

(define (pick-file-by-name files name #:key [key values])
  (findf (compose1 (path-ends-with name) key) files))

(define (file-or-directory-checksum path)
  (match (string-split
          (call-with-output-string
           (λ (str-out)
             (if (path-to-existant-file? path)
                 (parameterize ([current-output-port str-out])
                   (system @~a{md5sum @path}))
                 (parameterize ([current-output-port str-out]
                                [current-directory path])
                   (system
                    @~a{
                        find . -type f -exec md5sum '{}' ';' @;
                        | sort -k 2 @;
                        | md5sum
                        }))))))
    [(list* sum _) sum]
    [else #f]))

(define (path-replace-filename p new-name)
  (define-values {parent name _2} (split-path (simple-form-path p)))
  (build-path parent new-name))

(define (pretty-path p)
  (find-relative-path (simple-form-path (current-directory))
                      (simple-form-path p)))

