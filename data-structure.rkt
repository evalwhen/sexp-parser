#lang racket

(provide (all-defined-out))


(struct Node (type
              start
              end
              elts
              [size #:mutable]
              [ctx #:mutable])
  #:transparent)

(define comment?
  (lambda (n)
    (and (Node? n) (eq? 'comment (Node-type n)))))

(define paren?
  (lambda (n)
    (and (Node? n) (eq? 'paren (Node-type n)))))

(define token?
  (lambda (n)
    (and (Node? n) (eq? 'token (Node-type n)))))

(define str?
  (lambda (n)
    (and (Node? n) (eq? 'str (Node-type n)))))

(define character?
  (lambda (n)
    (and (Node? n) (eq? 'char (Node-type n)))))

(define newline?
  (lambda (n)
    (and (Node? n) (eq? 'newline (Node-type n)))))
