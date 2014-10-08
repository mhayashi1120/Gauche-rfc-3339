;;;
;;; Test rfc_3339
;;;

(use gauche.test)

(test-start "rfc.3339")
(use rfc.3339)
(test-module 'rfc.3339)

;; The following is a dummy test code.
;; Replace it for your tests.
(test* "test-rfc_3339" "rfc_3339 is working"
       (test-rfc_3339))

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

;;TODO test
(should (date=? (rfc3339-date->date "2014-01-02T03:04:05Z") (date 2014 1 2 3 4 5 0 0)))
(should (date=? (rfc3339-date->date "2014-01-02T03:04:05+09:00") (date 2014 1 2 3 4 5 0 32400)))
(should (date=? (rfc3339-date->date "2014-01-02T03:04:05-09:50") (date 2014 1 2 3 4 5 0 -35400)))

(should (date=? (rfc3339-date->date "2014-01-02 03:04:05Z") (date 2014 1 2 3 4 5 0 0)))

;; If you don't want `gosh' to exit with nonzero status even if
;; the test fails, pass #f to :exit-on-failure.
(test-end :exit-on-failure #t)




