#lang racket

(require (planet aml/rosetta))

(provide full-arc frame)

(define (full-arc p r a0 a1 h)
  (extrusion
    (subtraction
      (surface-arc p r a0 a1)
      (surface-arc p (* 0.98 r) a0 a1)) h))

(define (frame p l h)
  (subtraction
    (box p (+xyz p l l h))
    (box (+xy p (* l 0.01) (* l 0.01))
         (+xyz p (* l 0.99) (* l 0.99) h))))
