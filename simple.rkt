#lang racket/base
(require racket/cmdline
         racket/string
         racket/set
         racket/format
         racket/struct
         racket/system
         racket/hash
         net/url
         net/head
         net/uri-codec
         racket/port
         )

; from https://gitlab.com/RayRacine/string-util/-/blob/master/main.rkt
(define (string-first-char-occurrence s ch)
  (let ((len (string-length s)))
    (let loop ((i 0))
      (if (eq? i len)
          #f
          (if (char=? (string-ref s i) ch)
              i
              (loop (add1 i)))))))

(define (printSet theSet)
  (for [(e theSet)]
    (printf"~a\n" e)
  ))


(define sync? #f) ; test server sync?


(define host "localhost")

(define-values (port)
  (command-line
   #:once-each
   [("--sync") "Test the \"sync\" service"
    (set! sync? #t)]
   #:args (port)
   (values port)))

;fork a process to run the server
(define-values (sp out in err)
  (subprocess (current-error-port) #f #f "./utter" port))


(sleep 0.5) ; make sure it's started up



(define root-url
  (string->url
   (format "http://localhost:~a/" port)))

(define fail? #f)

;functions for the various API endpoints

(define (listen user [server-url root-url])
  (call/input-url (struct-copy url (combine-url/relative server-url "listen")
                               [query (list (cons 'user user))])
                  get-pure-port
                  port->string))

(define (utter user message [server-url root-url])
  (string-trim
   (port->string
    (post-pure-port (combine-url/relative server-url "utter")
                    (string->bytes/utf-8
                     (alist->form-urlencoded (list (cons 'user user) (cons 'utterance message))))
                    '("Content-Type: application/x-www-form-urlencoded")
                    ))))

(define (shh user id [server-url root-url])
  (port->string
   (post-pure-port (combine-url/relative server-url "shh")
                     (string->bytes/utf-8
                      (alist->form-urlencoded (list (cons 'user user) (cons 'id id))))
                      '("Content-Type: application/x-www-form-urlencoded")
                      )))


(define (syncWithServer hostname port user)
  (port->string
   (post-pure-port (combine-url/relative root-url "sync")
                   (string->bytes/utf-8
                    (alist->form-urlencoded (list (cons 'user user)
                                                  (cons 'hostname hostname)
                                                  (cons 'port port))))
                   '("Content-Type: application/x-www-form-urlencoded")
                   )))

(struct utterance (user message) #:transparent)
(struct postedUtter (id utterance) #:transparent)

(define (set->string s)
  (apply ~a #:separator " " (set->list s)))

(define utterList (list
                   (utterance "CS_UTE" "CS rulez")
                   (utterance "CE_UTE" "CE rulez")
                   (utterance "CS_UTE" "CS rulez more")))


(define (responseLineToPosted line user)
  (define sp (string-first-char-occurrence line #\space))
  (define id (substring line 0 sp))
  (define message (substring line (+ sp 1)))
  (postedUtter id (utterance user message))
  )


(define (listenResponseToSet listenResponse user)
  (list->set
   (map (lambda (str) (responseLineToPosted str user))
        (filter non-empty-string? (string-split listenResponse #rx"\r?\n")))))


;ACTUAL TESTS START HERE

(define expectedUtterances (make-hash))
(for ([u utterList])
  (define insertedId (apply utter (struct->list u)) )
  (define user (utterance-user u))
  (define existingUtters  (hash-ref! expectedUtterances user mutable-set))
  (set-add! existingUtters (postedUtter insertedId u)))

(for ([user (hash-keys expectedUtterances)])
  (let ([ actualResults (listenResponseToSet (listen user) user) ])
    (unless (set=? (hash-ref expectedUtterances user) actualResults)
      (set! fail? #t)
      (printf (string-append "wrong utterances set after test posts\n"
                             "expected ~a\n"
                             "got ~a\n")
              (set->string (hash-ref expectedUtterances user))
              (set->string actualResults)))))



(define CSUser "CS_UTE")
(define CSUtters (hash-ref expectedUtterances CSUser))
(for ([post (set->list CSUtters)])
  (set-remove! CSUtters post)
  (shh "CS_UTE" (postedUtter-id post))
  (define actualResults (listenResponseToSet (listen CSUser) CSUser))
  (unless (set=? CSUtters actualResults)
    (set! fail? #t)
    (printf (string-append "wrong utterances for CS_USER after removing ~a\n"
                           "expected ~a\n and got ~a\n")
            post CSUtters actualResults))
  ; check the CE posts
  (define ceResults (listenResponseToSet (listen "CE_UTE") "CE_UTE"))
  (unless (set=? ceResults (hash-ref expectedUtterances "CE_UTE"))
    (set! fail? #t)
    (printf (string-append "wrong utterances for CE_USER after removing ~a\n"
                           "expected ~a\n and got ~a\n")
            post (hash-ref expectedUtterances "CE_UTE") ceResults)))



(when sync?

  (define s2Utters (list
                    (utterance "CS_UTE" "CS Rulez on server 2")
                    (utterance "CE_UTE" "CE Rulez on server 2")
                    (utterance "CE_UTE" "CE still uttering on server 2")))

  (define otherServerPort (number->string (+ (string->number port) 10)))

  (define-values (sp2 out2 in2 err2)
    (subprocess (current-error-port) #f #f "./utter" otherServerPort))

  (sleep 0.5) ; make sure the subprocess starts

  (define other-root-url
    (string->url (format "http://localhost:~a/" otherServerPort)))

  (define s2Posts (make-hash))
  (for ([u s2Utters])
    (define insertedID (utter (utterance-user u) (utterance-message u) other-root-url))
    (define user (utterance-user u))
    (define userSet (hash-ref! s2Posts user mutable-set))
    (set-add! userSet (postedUtter insertedID u))
    )

  (hash-set! expectedUtterances "CE_UTE"
             (set-union (set) (hash-ref expectedUtterances "CE_UTE")
                        (hash-ref s2Posts "CE_UTE")))

;  (hash-union! expectedUtterances s2Posts
;               #:combine (lambda (s1 s2)
;                           (set-union (set) s1 s2)))

  (syncWithServer "localhost" otherServerPort "CE_UTE")


  (for ([user (hash-keys expectedUtterances)])
    (let ([ actualResults (listenResponseToSet (listen user) user) ])
      (unless (set=? (hash-ref expectedUtterances user) actualResults)
        (set! fail? #t)
        (printf (string-append "wrong utterances set after sync for user ~a\n"
                               "expected\n")
                user)
        (printSet (hash-ref expectedUtterances user))
        (printf "got\n")
        (printSet actualResults))))

  (subprocess-kill sp2 #f)
  )




(subprocess-kill sp #f)

;; Conclusion
(if fail?
    ((printf "Simple tests failed\n")
     (exit 1))
    (printf "Simple tests passed\n"))
