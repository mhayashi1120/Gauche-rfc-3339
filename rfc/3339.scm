;;;
;;; rfc/3339.scm - Parse rfc3339 Timestamps
;;;
;;;   Copyright (c) 2014,2020 Masahiro Hayashi <mhayashi1120@gmail.com>
;;;
;;;   Redistribution and use in source and binary forms, with or without
;;;   modification, are permitted provided that the following conditions
;;;   are met:
;;;
;;;   1. Redistributions of source code must retain the above copyright
;;;      notice, this list of conditions and the following disclaimer.
;;;
;;;   2. Redistributions in binary form must reproduce the above copyright
;;;      notice, this list of conditions and the following disclaimer in the
;;;      documentation and/or other materials provided with the distribution.
;;;
;;;   3. Neither the name of the authors nor the names of its contributors
;;;      may be used to endorse or promote products derived from this
;;;      software without specific prior written permission.
;;;
;;;   THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
;;;   "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
;;;   LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
;;;   A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
;;;   OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
;;;   SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED
;;;   TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR
;;;   PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF
;;;   LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
;;;   NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
;;;   SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
;;;
;;; RFC 3339 http://www.ietf.org/rfc/rfc3339.txt
;;;
;;; (e.g. "1937-01-01T12:00:27.87+00:20" "1985-04-12T23:20:50.52Z" "2020-04-12 23:20:50")

(define-module rfc.3339
  (use srfi-13)
  (use util.match)
  (use srfi-19)
  (use parser.peg)
  (export
   rfc3339-parse-date
   rfc3339-date-time-parser

   date->rfc3339

   rfc3339-date->date
   date->rfc3339-date))
(select-module rfc.3339)

(define $c $char)
(define digit ($one-of #[0-9]))

;;;
;;; parser (peg)
;;;

(define ($digit :optional (min 0) (max #f))
  ($do
   [s ($many digit min max)]
   ($return (string->number (rope->string s)))))

;; date-fullyear   = 4DIGIT
;; date-month      = 2DIGIT  ; 01-12
;; date-mday       = 2DIGIT  ; 01-28, 01-29, 01-30, 01-31 based on
;;                           ; month/year
;; full-date       = date-fullyear "-" date-month "-" date-mday
(define %full-date
  ($do
   [fullyear ($digit 4)]
   [($c #\-)]
   [month ($digit 2)]
   [($c #\-)]
   [day ($digit 2)]
   ($return (list fullyear month day))))

;; time-secfrac    = "." 1*DIGIT
(define %time-secfrac
  ($do
   [($c #\.)]
   [frac ($many digit 1 9)]
   ($return
    (let* ([s (rope->string frac)]
           [text (string-pad-right s 9 #\0)]
           [n (string->number text)])
      ;; nanosecond
      n))))

;; time-numoffset  = ("+" / "-") time-hour ":" time-minute
(define %time-numoffset
  (let1 %time
      ($or
       ($try
        ($do
         [hour ($digit 1 2)]
         [($c #\:)]
         [minute ($digit 1 2)]
         [$return (list hour minute)]))
       ;; extend format can omit `:'
       ($try
        ($do
         [h&m ($digit 3 4)]
         [$return
          (receive (hour minute)
              (div-and-mod h&m 100)
            (list hour minute))])))
    ($do
     [sign ($or ($c #\+) ($c #\-))]
     [hour&min %time]
     ($return
      (match-let1 (hour minute) hour&min
        (*
         (if (equal? sign #\+) 1 -1)
         (+ (* hour 60 60) (* minute 60))))))))

;; time-offset     = "Z" / time-numoffset
(define %time-offset
  ($try
   ($or
    ($do [($one-of #[zZ])] ($return 0))
    %time-numoffset)))

;; time-hour       = 2DIGIT  ; 00-23
;; time-minute     = 2DIGIT  ; 00-59
;; time-second     = 2DIGIT  ; 00-58, 00-59, 00-60 based on leap second
;;                           ; rules
;; partial-time    = time-hour ":" time-minute ":" time-second
;;                   [time-secfrac]
(define %partial-time
  ($do
   [hour ($digit 1 2)]
   [($c #\:)]
   [min ($digit 1 2)]
   [maybe-sec
    ;; Not mentioned about second part is optional, but accept.
    ($optional
     ($do
      [($c #\:)]
      [sec ($digit 1 2)]
      [frac ($optional %time-secfrac)]
      [$return (list sec frac)]))]
   ($return
    (match maybe-sec
      [(sec frac)
       (list hour min sec frac)]
      [else
       (list hour min #f #f)]))))

;; full-time       = partial-time time-offset
(define %full-time
  ($do
   [time ($optional %partial-time)]
   ;; extend format can insert `space'
   [($many ($c #\space))]
   [offset ($optional %time-offset)]
   ($return
    (list (or time (list #f #f #f #f)) offset))))

;; date-time       = full-date "T" full-time
(define %date-time
  ($do
   [date %full-date]
   [($optional
     ($try ($or
            ($one-of #[tT])
            ;; Section 5.6 NOTE
            ($c #\space))))]
   [time %full-time]
   ($return
    (match-let1 (year month day) date
      (match-let1 ((hour min sec nano) offset) time
        (list year month day hour min sec offset nano))))))

;;;
;;; Utility
;;;

(define (current-zone-offset)
  (date-zone-offset (current-date)))

;;;
;;; api
;;;

(define rfc3339-date-time-parser %date-time)

(define (rfc3339-parse-date text)
  (apply values (peg-parse-string %date-time text)))

(define (rfc3339-date->date text)
  (receive (year month day hour min sec offset nano)
      (rfc3339-parse-date text)
    (make-date (or nano 0) (or sec 0)
               (or min 0) (or hour 0) day month year
               (or offset (current-zone-offset)))))

;; datetime-separator: Print datetime with selected separator (Default: #\T)
;;     (e.g. #\space -> "2018-01-02 01:02:03Z")
;; suppress-time?: Suppress to print TIME spec.
;;     (e.g. "2018-01-02")
;; fraction-behavior: `round' / `ceiling' / `floor' / `midpointup' (Default: floor)
;;     First three symbols are same as gauche procedure.
;;     round (e.g. 0.0<=x<=0.5 => 0, 0.5<x<1.5 => 1, 1.5<=x<=2.5 => 2)
;;     midpointup is differ about 0.5 fraction. (e.g. 0.0<=x<0.5 => 0, 0.5<=x<1.5 => 1)
;;     This fraction is mentioned in 5.3. Rarely Used Options
;; sec-precision: Precision of the seconds. (Default: 2)
;;     Integer (<= 0 x 9) / seconds / second / deci / centi / ms / milli / micro / nano / ns
;; suppress-tz-colon?: Suppress to print zone-offset colon.
;; zone-offset: Print with specified timezone. (Default: UTC)
;;     `UTC' / `keep' / 'locale' / Integer / String / #f
;;     UTC: print with "Z" suffix.
;;     keep: Using DATE's zone-offset.
;;     locale: print with current locale timezone.
;;     Integer: Convert DATE's zone-offset to the value.
;;     String: Print timezone asis. This imply `keep' zone-offset.
;;     #f: suppress timezone
(define (rfc3339-print-date date :key (datetime-separator #\T)
                            (suppress-time? #f)
                            (fraction-behavior 'floor) (sec-precision 2)
                            (suppress-tz-colon? #f) (zone-offset 'UTC))
  (define (ensure-secfrac arg)
    (cond
     [(memq arg '(seconds second)) 0]
     [(memq arg '(deci)) 1]
     [(memq arg '(centi)) 2]
     [(memq arg '(ms milli)) 3]
     [(memq arg '(micro)) 6]
     [(memq arg '(nano ns)) 9]
     [(and (number? arg) (<= 0 arg 9)) arg]
     [else
      (error "Not a supported SECFRAC-LENGTH" arg)]))

  (define (secfrac-handler arg)
    (ecase fraction-behavior
      ['floor floor->exact]
      ['round round->exact]
      ['ceiling ceiling->exact]
      ['midpointup (^x (floor->exact (+ x 1/2)))]))

  (define (ensure-timezone arg date)
    (cond
     [(memq arg '(locale #f)) (date-zone-offset (current-date))]
     [(or (memq arg '(keep #f))
          (string? arg))
      (date-zone-offset date)]
     [(integer? arg) arg]
     [(eq? arg 'UTC) 0]
     [else
      (error "Not a supported offset" arg)]))

  (define (convert-zone-offset date tz-offset)
    (time-utc->date (date->time-utc date) tz-offset))

  (let* ([tz-offset (ensure-timezone zone-offset date)]
         [d (convert-zone-offset date tz-offset)]
         [frac-length (ensure-secfrac sec-precision)])
    (display (date->string d "~Y-~m-~d"))
    (display datetime-separator)
    (unless suppress-time?
      (display (date->string d "~H:~M:"))
      (let* ([sec (date-second d)]
             [secfrac (/ (date-nanosecond d) (expt 10 9))]
             [real-sec (+ sec secfrac)]
             [rounder (secfrac-handler fraction-behavior)])
        (if (> frac-length 0)
          (format #t "~2,'0d" sec)
          (let ([sec-value (rounder real-sec)])
            (format #t "~2,'0d" sec-value)))
        (when (> frac-length 0)
          (display ".")
          (let* ([fmt #`"~,|frac-length|,,'0d"]
                 [frac-value (rounder (* secfrac (expt 10 frac-length)))])
            (format #t fmt frac-value)))))
    (cond
     [(eq? zone-offset #f)]
     [(eq? zone-offset 'UTC)
      (display "Z")]
     [(or (integer? zone-offset)
          (memq zone-offset '(locale keep)))
      (receive (h s) (div-and-mod tz-offset (* 60 60))
        (let ([sign (if (< h 0) #\- #\+)]
              [hour (abs h)]
              [min (div s 60)])
          (format #t "~a" sign)
          (format #t "~2,'0d" hour)
          (unless suppress-tz-colon?
            (format #t ":"))
          (format #t "~2,'0d" min)))]
     [(string? zone-offset)
      (display zone-offset)]
     [else
      (errorf "zone-offset: ~a is not supported" zone-offset)])))

;; Utility function wrap `rfc3339-print-date' . KEYWORDS is passed to the function.
(define (date->rfc3339 date . keywords)
  (with-output-to-string
    (^() (apply rfc3339-print-date date keywords))))

(define (date->rfc3339-date date :key
                            (suppress-ms? #f)
                            :allow-other-keys keywords)
  (let1 converter
      (^ (fraction-length)
        (apply date->rfc3339
               date
               :sec-precision fraction-length
               keywords))
    (cond
     [suppress-ms?
      (converter 'seconds)]
     [else
      ;; RFC 3339 document only explicitly mention about 2 digit.
      (converter 2)])))

