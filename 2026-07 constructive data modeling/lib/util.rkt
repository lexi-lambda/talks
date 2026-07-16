#lang racket/base

(require racket/contract
         racket/function
         racket/math)

(provide (contract-out [make-lazy-parameter (-> any/c parameter?)]
                       [lazy-parameter/c (->* [contract?] [contract?] contract?)]
                       [turns (-> real? real?)]))

(define (make-lazy-parameter val)
  (make-derived-parameter (make-parameter val)
                          identity
                          (λ (v) (if (procedure? v) (v) v))))

(define (lazy-parameter/c in-ctc [out-ctc in-ctc])
  (parameter/c (or/c in-ctc (-> in-ctc)) out-ctc))

(define (turns n) (* 2 pi n))
