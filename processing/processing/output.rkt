#lang racket/base

(provide (all-defined-out))

(require racket/class
         "../util.rkt"
         "runtime-bindings.rkt")
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Print procedures
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define/types (print [Object arg] -> void)
              (display (arg->string arg)))

(define/types (println [Object arg] -> void)
              (displayln (arg->string arg)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Aux procedures
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Converts print arguments to be correctly printed
(define (arg->string arg)
  (format "~a"
          (cond
            [(and (boolean? arg) arg) "true"]
            [(and (boolean? arg) (not arg)) "false"]
            [(void? arg)  ""]
            [(not (null? arg)) arg]
            [else ""])))
