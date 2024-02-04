#lang racket

(require "script-lang.rkt")
(provide (all-from-out "script-lang.rkt")
         (all-from-out racket))

(module reader syntax/module-reader rscript)
