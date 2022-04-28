#lang racket/base
(require racket/cmdline
         net/url
         net/uri-codec
         racket/port
         racket/string
         racket/list
         racket/place
         racket/set
         racket/struct)

;; Number of entries will be the product of these:
(define NUM-PLACES 8)       ; base parallelism
(define NUM-THREADS 5)     ; more concurrency

(define NUM-USERS 4)
(define NUM-POSTS 100)

(define NUM-REPEATS 1)

(define POST-LENGTH 10) ; size of each entry

;; Sending lots of headers sometimes will
;; make some connections sluggish, maybe worth
;; a try:
(define MAX-JUNK-HEADERS 0)

(define-values ( port)
  (command-line
   #:once-each
   [("--lo") "Run a short, low-stress variant"
    (set! NUM-USERS 2)
    (set! NUM-THREADS 2)
    (set! NUM-POSTS 2)]
   #:args (port)
   (values port)))



; from https://gitlab.com/RayRacine/string-util/-/blob/master/main.rkt
(define (string-first-char-occurrence s ch)
  (let ((len (string-length s)))
    (let loop ((i 0))
      (if (eq? i len)
          #f
          (if (char=? (string-ref s i) ch)
              i
              (loop (add1 i)))))))


;fork a process to run the server
  (define-values (sp out in err)
    (subprocess (current-error-port) #f #f "./utter" port))
  (sleep 0.5) ;let it get started


(module+ main



  (define results
    (map place-wait
         (for/list ([p NUM-PLACES])
           (define pl (run-group))
           (place-channel-put pl p)
           pl)))

  (subprocess-kill sp #f)

  (unless (andmap zero? results)
    (raise-user-error 'stress "one or more places failed")))

(define (run-group)
  (place
   pch
   (define p (sync pch))

   (define root-url
     (string->url
      (format "http://localhost:~a/" port)))



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



   (struct utterance (user message) #:transparent)
   (struct postedUtter (id utterance) #:transparent)

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




   (define (id->user j)
        (format "u~a" j))


   (log-error "posting")
   (for-each
    sync
    (for/list ([k NUM-THREADS])
      (thread
       (lambda ()

         (define threadPosts (make-hash))

         (for ([j NUM-USERS])
           (define user (id->user j))
           (define userPosts (mutable-set))
           (for ([r NUM-POSTS])

             (define message (format "post[~a,~a,~a,~a]~a" p k j r (make-string POST-LENGTH #\*)))
             (define u (utterance user message))
             (define insertedId (apply utter (struct->list u)) )
             (set-add! userPosts (postedUtter insertedId u))
             )
           (hash-set! threadPosts user userPosts)
           )
         ; now check to make sure all the posts are there
         (for ([j NUM-USERS])
           (define user (id->user j))

           (define expectedUserPosts (hash-ref threadPosts user))
           (define postsOnServer (listenResponseToSet (listen user) user))

           (for [(p expectedUserPosts)]
             (unless (set-member? postsOnServer p)
               (raise-user-error 'stress "Missing post ~a from user ~a" p user)))


           )

         ; now delete them all
         (for ([j NUM-USERS])
           (define user (id->user j))
           (define expectedUserPosts (hash-ref threadPosts user))
           (for [(p expectedUserPosts)]
             (shh user (postedUtter-id p))
             )

           (define postsOnServer (listenResponseToSet (listen user) user))

           ; they should NOT be there anymore
           (for [(p expectedUserPosts)]
             (unless (not (set-member? postsOnServer p))
               (raise-user-error 'stress "Post ~a from user ~a should have been deleted, but is still there" p user)))

           )
         ))))))
