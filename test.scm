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

(date-should (date 2014 1 2 3 4 5 0 0)      (rfc3339-date->date "2014-01-02T03:04:05Z"))
(date-should (date 2014 1 2 3 4 5 0 32400)  (rfc3339-date->date "2014-01-02T03:04:05+09:00"))
(date-should (date 2014 1 2 3 4 5 0 32460) (rfc3339-date->date "2014-01-02T03:04:05+09:01"))
(date-should (date 2014 1 2 3 4 5 0 32460) (rfc3339-date->date "2014-01-02T03:04:05+0901"))
(date-should (date 2014 1 2 3 4 5 0 32460) (rfc3339-date->date "2014-01-02T03:04:05+9:01"))
(date-should (date 2014 1 2 3 4 5 0 32460) (rfc3339-date->date "2014-01-02T03:04:05+9:1"))
(date-should (date 2014 1 2 3 4 5 0 32460) (rfc3339-date->date "2014-01-02T03:04:05+901"))
(date-should (date 2014 1 2 3 4 5 0 -35400) (rfc3339-date->date "2014-01-02T03:04:05-09:50"))
(date-should (date 2014 1 2 3 4 5 0 0)      (rfc3339-date->date "2014-01-02t03:04:05z"))
(date-should (date 2014 1 2 3 4 5 100 0)    (rfc3339-date->date "2014-01-02t03:04:05.1Z"))
(date-should (date 2014 1 2 3 4 5 100 0)    (rfc3339-date->date "2014-01-02t03:04:05.10Z"))
(date-should (date 2014 1 2 3 4 5 100 0)    (rfc3339-date->date "2014-01-02t03:04:05.100Z"))
(date-should (date 2014 1 2 3 4 5 1 0)      (rfc3339-date->date "2014-01-02t03:04:05.001Z"))
(date-should (daten 2014 1 2 3 4 5 555555555 0)      (rfc3339-date->date "2014-01-02t03:04:05.555555555Z"))

(date-should (date 2014 1 2 3 4 5 1 (current-timezone)) (rfc3339-date->date "2014-01-02t03:04:05.001"))

(date-should (date 2014 1 2 3 4 5 0 0)      (rfc3339-date->date "2014-01-02 03:04:05Z"))

(date-should (date 2014 1 2 3 4 5 0 0)      (rfc3339-date->date "2014-01-02 03:04:05 Z"))
(date-should (date 2014 1 2 3 4 5 0 32400)  (rfc3339-date->date "2014-01-02 03:04:05 +0900"))

(should "2014-01-02T03:04:05.00Z" (date->rfc3339-date (rfc3339-date->date "2014-01-02 03:04:05Z")))

(should "2014-01-02T03:04:05Z"(date->rfc3339-date (rfc3339-date->date "2014-01-02T03:04:05Z")
                                                  :suppress-ms? #t))
(should "2014-01-02T12:04:05+0900" (date->rfc3339-date (rfc3339-date->date "2014-01-02T03:04:05Z")
                                                       :suppress-ms? #t :suppress-tz-colon? #t
                                                       :zone-offset 32400))

(should "2014-01-02T03:14:06.00+00:10"
        (date->rfc3339-date (rfc3339-date->date "2014-01-02 03:04:05Z")
                            :zone-offset 601))
(should "2014-01-02T03:14:06.00+0010"
        (date->rfc3339-date (rfc3339-date->date "2014-01-02 03:04:05Z")
                            :zone-offset 601 :suppress-tz-colon? #t))

(let ([fracsec-date (rfc3339-date->date "2014-01-02 03:04:05.555555555Z")])
  (should "2014-01-02T03:04:05.55Z"
          (date->rfc3339-date fracsec-date))
  (should "2014-01-02T03:04:05.555Z"
          (date->rfc3339-date fracsec-date :sec-precision 3))
  (should "2014-01-02T03:04:05Z"
          (date->rfc3339-date fracsec-date :sec-precision 0))
  (should "2014-01-02T03:04:06Z"
          (date->rfc3339-date fracsec-date :sec-precision 0 :fraction-behavior 'ceiling))
  (should "2014-01-02T03:04:05Z"
          (date->rfc3339-date fracsec-date :sec-precision 0 :fraction-behavior 'floor))
  (should "2014-01-02T03:04:06Z"
          (date->rfc3339-date fracsec-date :sec-precision 0 :fraction-behavior 'round))
  (should "2014-01-02T03:04:06Z"
          (date->rfc3339-date fracsec-date :sec-precision 0 :fraction-behavior 'midpointup))
  (should "2014-01-02T03:04:05.55Z"
          (date->rfc3339-date fracsec-date :sec-precision 2 :fraction-behavior 'floor))
  (should "2014-01-02T03:04:05.556Z"
          (date->rfc3339-date fracsec-date :sec-precision 3 :fraction-behavior 'midpointup)))

(let ([fracsec-date (rfc3339-date->date "2014-01-02 03:04:06.50Z")])
  (should "2014-01-02T03:04:06Z"
          (date->rfc3339-date fracsec-date :sec-precision 0 :fraction-behavior 'round))
  (should "2014-01-02T03:04:07Z"
          (date->rfc3339-date fracsec-date :sec-precision 0 :fraction-behavior 'midpointup)))
  
(let ([fracsec-date (rfc3339-date->date "2014-01-02 03:04:05.444444444Z")])
  (should "2014-01-02T03:04:05Z"
          (date->rfc3339-date fracsec-date :sec-precision 0))
  (should "2014-01-02T03:04:06Z"
          (date->rfc3339-date fracsec-date :sec-precision 0 :fraction-behavior 'ceiling))
  (should "2014-01-02T03:04:05Z"
          (date->rfc3339-date fracsec-date :sec-precision 0 :fraction-behavior 'floor))
  (should "2014-01-02T03:04:05Z"
          (date->rfc3339-date fracsec-date :sec-precision 0 :fraction-behavior 'round))
  (should "2014-01-02T03:04:05Z"
          (date->rfc3339-date fracsec-date :sec-precision 0 :fraction-behavior 'midpointup))
  (should "2014-01-02T03:04:05.44Z"
          (date->rfc3339-date fracsec-date :sec-precision 2 :fraction-behavior 'floor)))

;; If you don't want `gosh' to exit with nonzero status even if
;; the test fails, pass #f to :exit-on-failure.
(test-end :exit-on-failure #t)




