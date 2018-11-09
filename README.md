# Herbert-Chope+-2
End of Semester project
#lang racket

(require net/url)
(require json)
(require math)


;setting up bear
(define token "hfhfhfhkcjjcjckkcjckccjckcj")
(define countries (list (cons "AD" "42.546245,1.601554")))
(define (getgeocode country_code) 
    (define (scan-countries ccode clist)
        (cond 
            ((null? clist) "country code not found")
            (else
                (if
                    (eq? ccode (car (car clist)))
                    (car clist)
                    (scan-countries ccode (cdr clist))
                )
            )
        )
    )
    (scan-countries country_code countries)
)

(define (buildgeocodeparam geocode) 
    (string-append (cdr geocode) ",1000mi")
)

; tweets
(define authHeader (string-append "Authorization: Bearer " token))
(define geocodeParam (string-append "geocode" "=" (buildgeocodeparam (getgeocode "RU"))))
(define countParam (string-append "count" "=" "100"))
(define tweetmodeParam (string-append "tweet_mode" "=" "extended"))
(define apiUrl 
    (string-append 
        "https://api.twitter.com/1.1/search/tweets.json" 
        "?" geocodeParam 
        "&" countParam
        "&" tweetmodeParam
    ) 
)
(define destUrl (string->url apiUrl))
(define twitterApiRequest (get-pure-port destUrl (list authHeader)))
(define resjson (port->string  twitterApiRequest))
(close-input-port twitterApiRequest)

;change response to hash map
(define resobj (string->jsexpr resjson))
(define statuses (hash-ref resobj 'statuses))


;create one big string
(define 
    (buildbigstring statuses)
    (let 
        (
            (bigstring "")
        )
        (define (scan-statuses sub_statuses)
            (cond 
              ((null? sub_statuses) bigstring)
              (else
                (begin
                    (set! bigstring (string-append bigstring (hash-ref (car sub_statuses) 'full_text)))
                    (scan-statuses (cdr sub_statuses))
                )
              )
            )
        )
        (scan-statuses statuses)
    )
)

(define statusstring (buildbigstring statuses))


;analyse mood
(display "###########################")
(newline)
(display "Analysing : ")
(display "[")
(display (length statuses))
(display "]")
(newline)
(newline)
(display statusstring)
(newline)


(define tokens (document->tokens statusstring #:sort? #t))
(define sentiments-raw (list->sentiment tokens #:lexicon 'nrc))
(define sentiments-agg (aggregate sum ($ sentiments-raw 'sentiment) ($ sentiments-raw 'freq)))
(display sentiments-agg)

(newline)

)

(display "###########################")

