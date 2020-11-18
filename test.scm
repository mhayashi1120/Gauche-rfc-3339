;;;
;;; Test rfc_3339
;;;

(use gauche.test)
(use srfi-19)

(test-start "rfc.3339")
(use rfc.3339)
(test-module 'rfc.3339)

(load "testlib.scm")

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
(date-should (date 2014 1 2 3 4 0 0 0)      (rfc3339-date->date "2014-01-02t03:04Z"))
(date-should (date 2014 1 2 3 4 0 0 32400)      (rfc3339-date->date "2014-01-02t03:04+09:00"))
(date-should (date 2014 1 2 3 4 0 0 32400)      (rfc3339-date->date "2014-01-02t03:04"))
(date-should (date 2014 1 2 0 0 0 0 32400)      (rfc3339-date->date "2014-01-02t"))
(date-should (date 2014 1 2 0 0 0 0 32400)      (rfc3339-date->date "2014-01-02 "))
(date-should (date 2014 1 2 0 0 0 0 32400)      (rfc3339-date->date "2014-01-02"))
(date-should (daten 2014 1 2 3 4 5 555555555 0)      (rfc3339-date->date "2014-01-02t03:04:05.555555555Z"))

(date-should (date 2014 1 2 3 4 5 1 (current-timezone)) (rfc3339-date->date "2014-01-02t03:04:05.001"))

(date-should (date 2014 1 2 3 4 5 0 0)      (rfc3339-date->date "2014-01-02 03:04:05Z"))

(date-should (date 2014 1 2 3 4 5 0 0)      (rfc3339-date->date "2014-01-02 03:04:05 Z"))
(date-should (date 2014 1 2 3 4 5 0 32400)  (rfc3339-date->date "2014-01-02 03:04:05 +0900"))

(should "2014-01-02T03:04:05.00Z"
        (date->rfc3339-date (rfc3339-date->date "2014-01-02 03:04:05Z")))

(should "2014-01-02T03:04:05Z"
        (date->rfc3339-date (rfc3339-date->date "2014-01-02T03:04:05Z")
                            :suppress-ms? #t))
(should "2014-01-02T12:04:05+0900"
        (date->rfc3339-date (rfc3339-date->date "2014-01-02T03:04:05Z")
                            :suppress-ms? #t :suppress-tz-colon? #t
                            :zone-offset 32400))

(should "2014-01-02T03:14:06.00+00:10"
        (date->rfc3339-date (rfc3339-date->date "2014-01-02 03:04:05Z")
                            :zone-offset 601))
(should "2014-01-02T03:14:06.00+0010"
        (date->rfc3339-date (rfc3339-date->date "2014-01-02 03:04:05Z")
                            :zone-offset 601 :suppress-tz-colon? #t))

(should "2014-01-02T03:04:05.00+00:00"
        (date->rfc3339-date (date 2014 01 02 03 04 05 0 0)
                            :zone-offset 'keep))

(should "2014-01-02T03:04:05.00Z"
        (date->rfc3339-date (date 2014 01 02 03 04 05 0 0)
                            :zone-offset 'UTC))

(should "2014-01-02T09:04:05.00+06:00"
        (date->rfc3339-date (date 2014 01 02 03 04 05 0 0)
                            :zone-offset (* 6 60 60)))

(should "2014-01-02T03:04:05.00 JST"
        (date->rfc3339-date (date 2014 01 02 03 04 05 0 32400)
                            :zone-offset " JST"))


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




