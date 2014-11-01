;;;
;;; Test rfc_3339
;;;

(use gauche.test)
(use srfi-19)

(test-start "rfc.3339")
(use rfc.3339)
(test-module 'rfc.3339)

(define (date=? d1 d2)
  (let loop ([slots (class-slots <date>)])
    (cond
     [(null? slots) #t]
     [(not (equal? (~ d1(caar slots)) (~ d2(caar slots))))
      #f]
     [else
      (loop (cdr slots))])))

(define (date y m d H M S MS offset)
  (make-date (* MS 1000000) S M H d m y offset))

(define (should expected result :optional (msg #f))
  (test* (or msg (x->string expected))
         expected result))

(define (date-should expected result :optional (msg #f))
  (test* (or msg expected)
         expected result date=?))

(date-should (date 2014 1 2 3 4 5 0 0)      (rfc3339-date->date "2014-01-02T03:04:05Z"))
(date-should (date 2014 1 2 3 4 5 0 32400)  (rfc3339-date->date "2014-01-02T03:04:05+09:00"))
(date-should (date 2014 1 2 3 4 5 0 -35400) (rfc3339-date->date "2014-01-02T03:04:05-09:50"))
(date-should (date 2014 1 2 3 4 5 0 0)      (rfc3339-date->date "2014-01-02t03:04:05z"))
(date-should (date 2014 1 2 3 4 5 100 0)    (rfc3339-date->date "2014-01-02t03:04:05.1Z"))
(date-should (date 2014 1 2 3 4 5 100 0)    (rfc3339-date->date "2014-01-02t03:04:05.10Z"))
(date-should (date 2014 1 2 3 4 5 100 0)    (rfc3339-date->date "2014-01-02t03:04:05.100Z"))
(date-should (date 2014 1 2 3 4 5 1 0)      (rfc3339-date->date "2014-01-02t03:04:05.001Z"))

(date-should (date 2014 1 2 3 4 5 0 0)      (rfc3339-date->date "2014-01-02 03:04:05Z"))

(should "2014-01-02T03:04:05.00Z" (date->rfc3339-date (rfc3339-date->date "2014-01-02 03:04:05Z")))

(should "2014-01-02T03:04:05Z"(date->rfc3339-date (rfc3339-date->date "2014-01-02T03:04:05Z")
                                                  :suppress-ms? #t))
(should "2014-01-02T12:04:05+0900" (date->rfc3339-date (rfc3339-date->date "2014-01-02T03:04:05Z")
                                                       :suppress-ms? #t :suppress-tz-colon? #t
                                                       :zone-offset 32400))

;; If you don't want `gosh' to exit with nonzero status even if
;; the test fails, pass #f to :exit-on-failure.
(test-end :exit-on-failure #t)




