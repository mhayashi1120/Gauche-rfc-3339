;;;
;;; rfc/3339.scm - Parse rfc3339 Timestamps
;;;
;;;   Copyright (c) 2014 Masahiro Hayashi <mhayashi1120@gmail.com>
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
;;; (e.g. 1937-01-01T12:00:27.87+00:20 1985-04-12T23:20:50.52Z)

(define-module rfc.3339
  (use util.match)
  (use srfi-19)
  (use parser.peg)
  (export
   rfc3339-date->date
   date->rfc3339-date))
(select-module rfc.3339)

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
   [frac ($many digit 1 3)]
   ($return
    (let* ([text (rope->string frac)]
           [n (string->number text)])
      ;;TODO refactor
      ;; nanosecond
      (cond
       [(= (string-length text) 3)
        (* n 1000000)]
       [(= (string-length text) 2)
        (* n 10000000)]
       [else
        (* n 100000000)])))))

;; time-numoffset  = ("+" / "-") time-hour ":" time-minute
;; time-offset     = "Z" / time-numoffset
(define %time-nummoffset
  ($do
   [sign ($try ($or ($c #\+) ($c #\-)))]
   [hour ($digit 2)]
   [($c #\:)]
   [minute ($digit 2)]
   ($return
    (*
     (if (equal? sign #\+) 1 -1)
     (+ (* hour 60 60) (* minute 60))))))

(define %time-offset
  ($try
   ($or
    ($do [($one-of #[zZ])] ($return 0))
    %time-nummoffset)))

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
   [($c #\:)]
   [sec ($digit 1 2)]
   [frac ($optional %time-secfrac 0)]
   ($return (list hour min sec frac))))
   
;; full-time       = partial-time time-offset
(define %full-time
  ($do
   [time %partial-time]
   [offset %time-offset]
   ($return (list time offset))))

;; date-time       = full-date "T" full-time
(define %date-time
  ($do
   [date %full-date]
   [($try ($or
           ($one-of #[tT])
           ;; Section 5.6 NOTE
           ($c #\space)))]
   [time %full-time]
   ($return
    (match-let1 (year month day) date
      (match-let1 ((hour min sec nano) offset) time
        (make-date nano sec min hour day month year offset))))))

(define (rfc3339-date->date text)
  (peg-parse-string %date-time text))

(define (date->rfc3339-date date :optional (zone 0))
  (let1 d (time-utc->date (date->time-utc (current-date)) zone)
    (with-output-to-string
      (^()
        (display (date->string d "~Y-~m-~dT~H:~M:~S"))
        (display ".")
        (format #t "~d" (div (slot-ref d 'nanosecond) 10000000))
        (cond
         [(= zone 0)
          (display "Z")]
         [else
          (receive (h s) (div-and-mod zone (* 60 60))
            (let ([sign (if (< h 0) #\- #\+)]
                  [hour (abs h)]
                  [min (div s 60)])
              (format #t "~a~2,'0d:~2,'0d"
                      sign hour min)))])))))

