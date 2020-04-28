#lang racket/base

(require "script-lang.rkt")
(provide (all-from-out "script-lang.rkt"))

(module reader syntax/module-reader rscript)
