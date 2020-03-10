(define (date=? d1 d2)
  (let loop ([slots (class-slots <date>)])
    (cond
     [(null? slots) #t]
     [(not (equal? (~ d1(caar slots)) (~ d2(caar slots))))
      #f]
     [else
      (loop (cdr slots))])))

;; Utility for easy to compare text.
(define (date y m d H M S MS offset)
  (make-date (* MS 1000000) S M H d m y offset))

(define (daten y m d H M S NS offset)
  (make-date NS S M H d m y offset))

(define (should expected result :optional (msg #f))
  (test* (or msg (x->string expected))
         expected result))

(define (date-should expected result :optional (msg #f))
  (test* (or msg expected)
         expected result date=?))

(define (current-timezone)
  (date-zone-offset (current-date)))
