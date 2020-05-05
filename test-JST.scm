(use gauche.test)
(use srfi-19)
(use rfc.3339)

(load "testlib.scm")

(test-start "rfc.3339 for JST")

;;;
;;; Special test for JST-9 locale
;;;

(should "2014-01-02T12:04:05.00"
        (date->rfc3339-date (date 2014 01 02 03 04 05 0 0)
                            :zone-offset #f))

(should "2014-01-02T12:04:05.00+09:00"
        (date->rfc3339-date (date 2014 01 02 03 04 05 0 0)
                            :zone-offset 'locale))

(date-should
 (date 2020 01 02 03 04 00 0 32400)
 (rfc3339-date->date "2020-01-02t03:04"))

(test-end :exit-on-failure #t)
