#! /usr/bin/racket
#lang racket

(require processing/lang/reader)
(require racket/pretty)

(displayln "### Processing")
(displayln
  (port->string (open-input-file (vector-ref (current-command-line-arguments)  0))))
(displayln "### Racket")
(pretty-display
  (processing-read
    (open-input-file (vector-ref (current-command-line-arguments) 0))))
