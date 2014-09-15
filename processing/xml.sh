#! /usr/bin/racket
#lang racket
(require processing/compile)

(ast->xml (build-ast (vector-ref (current-command-line-arguments) 0)))
