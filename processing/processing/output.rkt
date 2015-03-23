#lang racket/base

(provide (all-defined-out))

(require racket/class
         "../util.rkt"
         "runtime-bindings.rkt"
         "objects.rkt")
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Print procedures
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define/types (print [Object . args] -> void)
              (display
                (build-seperated-string (map arg->string args) #\space)))

(define/types (println [Object . args] -> void)
              (displayln
              (build-seperated-string (map arg->string args) #\space)))

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
            [(is-a? arg PVector) (send arg toString)]
            [(not (null? arg)) arg]
            [else ""])))
