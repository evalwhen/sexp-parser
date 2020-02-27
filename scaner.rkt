#lang racket

(provide (all-defined-out))

(require "data-structure.rkt")

;; s-expression settings
;; please override for other languages.
(define *delims* (list "("  ")"  "["  "]"  "{"  "}" "'"  "`"  ","))
(define *line-comment* (list ";"))
(define *comment-start* "")
(define *comment-end* "")
(define *operators*  '())
(define *quotation-marks* '(#\" #\'))
(define *lisp-char* (list "#\\" "?\\"))
(define *significant-whitespaces* '())


(define (set-delims x)
  (set! *delims* x))

(define (set-line-comment x)
  (set! *line-comment* x))

(define (set-comment-start x)
  (set! *comment-start* x))

(define (set-comment-end x)
  (set! *comment-end* x))

(define (set-operators x)
  (set! *operators* x))

(define (set-quotation-marks x)
  (set! *quotation-marks* x))

(define (set-lisp-char x)
  (set! *lisp-char* x))

(define (set-significant-whitespaces x)
  (set! *significant-whitespaces* x))



;-------------------------------------------------------------
;                          scanner
;-------------------------------------------------------------

(define whitespace?  char-whitespace?)
(define alpha?       char-alphabetic?)
(define digit?       char-numeric?)

(define set-alpha
  (lambda (x)
    (set! alpha? x)))

(define char->string string)
(define fatal
  (lambda (who . args)
    (printf "~s: " who)
    (for-each display args)
    (display "\n")
    (error who "")))


; Is char c a delimeter?
(define delim?
  (lambda (c)
    (member (char->string c) *delims*)))


(define id?
  (lambda (s)
    (cond
     [(= 0 (string-length s)) #f]
     [(or (alpha? (string-ref s 0))
          (eq? #\_ (string-ref s 0)))
      (let loop ([i 1])
        (cond
         [(>= i (string-length s)) #t]
         [else
          (let ([c (string-ref s i)])
            (cond
             [(alpha? c) (loop (add1 i))]
             [(digit? c) (loop (add1 i))]
             [(char=? c #\_) (loop (add1 i))]
             [else #f]))]))]
     [else #f])))


(define numeral?
  (lambda (s)
    (cond
     [(= 0 (string-length s)) #f]
     [(digit? (string-ref s 0)) #t
      ;; (let loop ([i 1])
      ;;   (cond
      ;;    [(>= i (string-length s)) #t]
      ;;    [else
      ;;     (let ([c (string-ref s i)])
      ;;         (cond
      ;;          [(digit? c) (loop (add1 i))]
      ;;          [(char=? c #\.) (loop (add1 i))]
      ;;          [else #f]))]))
]
     [else #f])))



(define start-with
  (lambda (s start prefix)
    (let* ([prefix-str (if (char? prefix)
                           (char->string prefix)
                           prefix)]
           [len (string-length prefix-str)])
      (cond
       [(= len 0) #f]
       [(< (string-length s) (+ start len)) #f]
       [(string=? (substring s start (+ start len)) prefix-str)
        prefix]
       [else #f]))))



(define start-with-one-of
  (lambda (s start prefixes)
    (cond
     [(null? prefixes) #f]
     [(start-with s start (car prefixes))
      (car prefixes)]
     [else
      (start-with-one-of s start (cdr prefixes))])))

; (start-with-one-of "+>>=" 0 (list ">" #\+))



(define find-next
  (lambda (s start pred)
    (cond
     [(<= (string-length s) start) #f]
     [(pred s start) start]
     [else
      (find-next s (add1 start) pred)])))



; Find the first delim that match the start of s
(define find-delim
  (lambda (s start)
    (start-with-one-of s start *delims*)))



(define find-operator
  (lambda (s start)
    (start-with-one-of s start *operators*)))

; (find-operator ">> x" 0)



(define scan
  (lambda (s)
    (define scan1
      (lambda (s start)
        (cond
         [(= start (string-length s)) (values 'eof start)]

         [(start-with-one-of s start *significant-whitespaces*)
          (values (Node 'newline start (add1 start) '() #f #f)
                  (add1 start))]

         [(whitespace? (string-ref s start))
          (scan1 s (add1 start))]

         [(start-with-one-of s start *line-comment*) ; line comment
          (let ([line-end (find-next s start
                                     (lambda (s start)
                                       (eq? (string-ref s start) #\newline)))])
            (values (Node 'comment
                          start
                          (add1 line-end)
                          (substring s start line-end)
                          #f #f)
                    line-end))]

         [(start-with s start *comment-start*) ; block comment
          (let* ([line-end (find-next s start
                                      (lambda (s start)
                                        (start-with s start *comment-end*)))]
                 [end (+ line-end (string-length *comment-end*))])
            (values (Node 'comment
                          start
                          end
                          (substring s start end)
                          #f #f)
                    end))]

         [(find-delim s start) =>
          (lambda (delim)
            (let ([end (+ start (string-length delim))])
              (values (Node 'token start end delim #f #f)
                      end)))]

         [(find-operator s start) =>
          (lambda (op)
            (let ([end (+ start (string-length op))])
              (values (Node 'token start end op #f #f)
                      end)))]

         [(start-with-one-of s start *quotation-marks*)   ; string
          (let ([reg-match (or (regexp-match (regexp "^\"(\\\\.|[^\"])*\"")
                                             s start)
                               (regexp-match (regexp "^\'(\\\\.|[^\'])*\'")
                                             s start))])
            (cond
             [(not reg-match)
              (fatal 'scan "string match error")]
             [else
              (let* ([len (string-length (car reg-match))]
                     [end (+ start len)])
                (values (Node 'str start end (car reg-match) #f #f)
                        end))]))]

         [(start-with-one-of s start *lisp-char*)         ; scheme/elisp char
          (cond
           [(<= (string-length s) (+ 2 start))
            (error 'scan-string "reached EOF while scanning char")]
           [else
            (let ([end
                   (let loop ([end (+ 3 start)])
                     (cond
                      [(or (whitespace? (string-ref s end))
                           (delim? (string-ref s end)))
                       end]
                      [else (loop (add1 end))]))])
              (values (Node 'char start end (string-ref s (sub1 end)) #f #f)
                      end))])]

         [else                        ; identifier or number
          (let loop ([pos start] [chars '()])
            (cond
             [(or (<= (string-length s) pos)
                  (whitespace? (string-ref s pos))
                  (find-delim s pos)
                  (find-operator s pos))
              (let ([text (list->string (reverse chars))])
                (values (Node 'token start pos text #f #f)
                        pos))]
             [else
              (loop (add1 pos) (cons (string-ref s pos) chars))]))])))

    (let loop ([start 0] [toks '()])
      (let-values ([(tok newstart) (scan1 s start)])
        (cond
         [(eq? tok 'eof)
          (reverse toks)]
         [else
          (loop newstart (cons tok toks))])))))

