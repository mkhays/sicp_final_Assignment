#lang racket
(require data-science-master)
(require plot)
(require math)
(require json)

;;; This function reads line-oriented JSON (as output by massmine),
;;; and packages it into an array. For very large data sets, loading
;;; everything into memory like this is heavy handed. For data this small,
;;; working in memory is simpler
(define (json-lines->json-array #:head [head #f])
  (let loop ([num 0]
             [json-array '()]
             [record (read-json (current-input-port))])
    (if (or (eof-object? record)
            (and head (>= num head)))
        (jsexpr->string json-array)
        (loop (add1 num) (cons record json-array)
              (read-json (current-input-port))))))

;;; Normalize case, remove URLs, remove punctuation, and remove spaces
;;; from each tweet. This function takes a list of words and returns a
;;; preprocessed subset of words/tokens as a list
(define (preprocess-text lst)
  (map (λ (x)
         (string-normalize-spaces
          (remove-punctuation
           (remove-urls
            (string-downcase x)))))
       lst))
;;; Read in the entire tweet database (3200 tweets from Trump's timeline)
(define mytweets (string->jsexpr
                (with-input-from-file "TweetsUganda400.json" (λ () (json-lines->json-array)))))
;;; Remove just the tweet text and source from each tweet
;;; hash. Finally, remove retweets.
;;; Remove just the tweet text, source, and timestamp from each tweet
;;; hash. Finally, remove retweets.
(define t
  (let ([tmp (map (λ (x) (list (hash-ref x 'text) (hash-ref x 'source)
                               (hash-ref x 'created_at))) mytweets)])
    (filter (λ (x) (not (string-prefix? (first x) "RT"))) tmp)))




;;Definition created to get the strings of the cleaned tweets and pass them to next definition for ploting 
(define getStringTweets
    (local[
           (define (joinedString tlist1 acc)
             (cond [(empty? tlist1) acc]
                   [else (joinedString (rest tlist1) (string-join (list acc (first(first tlist1)))))]
                   )
             )
           ](joinedString t "")) )

;;; Next, we capture the text from our input port, removing capitalization, 
;;; punctuation, and then extra spaces
;;;(define moby-text (string-normalize-spaces
		;;;   (remove-punctuation
		;;;    (string-downcase (port->string in)) #:websafe? #t)))
			 
;;; Close the input port
;;;(close-input-port in)

;;; To begin our sentiment analysis, we extract each unique word
;;; and the number of times it occurred in the document
(define words (document->tokens getStringTweets #:sort? #t))

;;; Using the nrc lexicon, we can label each (non stop-word) with an
;;; emotional label. 
(define sentiment (list->sentiment words #:lexicon 'nrc))

;;; We can take a sneak peak at the data...
(take sentiment 5)

;;; sentiment, created above, consists of a list of triplets of the pattern
;;; (token sentiment freq) for each token in the document. Many words will have 
;;; the same sentiment label, so we aggregrate (by summing) across such tokens.
(aggregate sum ($ sentiment 'sentiment) ($ sentiment 'freq))


;;; Better yet, we can visualize this result as a barplot (discrete-histogram)
(let ([counts (aggregate sum ($ sentiment 'sentiment) ($ sentiment 'freq))])
  (parameterize ((plot-width 800))
    (plot (list
	   (tick-grid)
	   (discrete-histogram
	    (sort counts (λ (x y) (> (second x) (second y))))
	    #:color "Pink"
	    #:line-color "MediumSlateBlue"))
	  #:x-label "Emotional Label"
	  #:y-label "Frequency")))
