#lang racket

(require (planet aml/rosetta))

(provide full-arc frame)

(define (full-arc x y z r a0 a1)
  (let ([p (xyz x y z)])
    (subtraction
      (surface-arc p r a0 a1)
      (surface-arc p (* 0.99 r) a0 a1))))

(define (frame x y z l)
  (let ([p (xyz x y z)])
    (subtraction
      (box p (+xyz p l l 0.01))
      (box (+xy p (* l 0.01) (* l 0.01)) (+xyz p (* l 0.99) (* l 0.99) 0.01)))))
