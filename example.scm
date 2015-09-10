(use-modules
 (ice-9 vlist)
 (ice-9 match)
 (ice-9 rw))
;;(use-modules (epoll))
(load "epoll.scm")

#| This example shows how to use epoll, by requesting a webpage from gnu.org

It uses a table of readers, so that when epoll signals a file descriptor
needs an update, it can look up the reading procedure by that descriptor. It has a similar table of writers, who disable themselves once they've run out of stuff to write.

One can use the same writer procedure for each fd, but each fd needs its own outgoing buffer so you're going to have to close over that procedure anyway. Multiple writing procedures allows you to have one fd being written by (write-string/partial) on calculated data, or buffered data, while another writer could be writing the contents of a file via sendfile, and a third could use the pipe/splice technique to proxy data from one socket to another.

The output buffering is decidedly a hack, and adds to the buffer by appending the incoming piece to the buffer as a string. That MAY not be as inefficient as one would think, if guile can intelligently realloc the buffer by extending the buffer's old memory block, ala the /shared interface.

This example doesn't check the output buffer and try to pause whatever's producing data to write, when it's too full. That would be problematic if we were uploading gigabytes of data maybe. Something to consider if writing proxies that can't use pipe/splice for some reason, or file servers that can't use sendfile for some reason (i.e. content filtering applications).

Ideally, one would add code in the writing procedure, that when the buffer is empty (or empty "enough"), it looks up a list of paused writers according to that fd, and resumes one or more of them. Then a "don't write anymore" error would let the writer dump a continuation into that list and buzz off.
|#

(define (print . args)
  (display args)
  (newline))

(define (fixed-shutdown fd how)
  (catch 'system-error
         (lambda ()
           (shutdown fd how))
         (lambda (type proc thingy messages codes)
           (cond
            ((= (car codes) 107)
             ;; ENOTCONN is often returned by shutdown, due to confused kernel developers
             #t)
            (else
             (throw type proc thingy messages codes))))))


;; table of fd -> readers/writers
(define readers (make-hash-table 0))
(define writers (make-hash-table 0))

(epoll-signals 1 2 3 13 14 15 17) ; where should these be imported from?

(define epfd (epoll-create))
(print "epfd" epfd)

(define (lets-read! fd)
  (print "want read" fd)
  ;; is it incorrect to use EPOLL_CTL_ADD here?
  ;; we only have to set the first reader, then we can just
  ;; return the new reader, and not call lets-read! again.
  (epoll-ctl epfd
             (if (hash-get-handle writers fd)
                 EPOLL_CTL_MOD
                 EPOLL_CTL_ADD)
             fd EPOLLIN))

(define (lets-write! fd)
  (epoll-ctl epfd
             (if (hash-get-handle readers fd)
                 EPOLL_CTL_MOD
                 EPOLL_CTL_ADD)
             fd (logior EPOLLIN EPOLLOUT))
  (print "want write" fd))


(define (done-reading! fd)
  ;; this is correct, right? We're done reading when we return #f
  ;; so finalize the socket closure?
  (if (hash-get-handle writers fd)
      (epoll-ctl epfd EPOLL_CTL_MOD fd EPOLLOUT)
      (epoll-ctl epfd EPOLL_CTL_DEL fd))
  (hash-remove! readers fd)
  (print "done reading woo" readers writers))

(define (done-writing! fd)
  (if (hash-get-handle readers fd)
      (epoll-ctl epfd EPOLL_CTL_MOD fd EPOLLIN)
      (epoll-ctl epfd EPOLL_CTL_DEL fd))
  (hash-remove! writers fd)
  (print "done writing" readers writers))


;; these return #f for the convenience of prompting the main-loop
;; to retry the operation on the new handlers.

;; this hack's needed so that reader procedures can start writing
;; and vice versa. You could just call the new writer from
;; within the old writer, but this works to do itfor both readers
;; and writers

(define (start-reading! fd reader)
  (when (not (hash-ref readers fd))
        (lets-read! fd))
  (hash-set! readers fd reader)
  #f)

(define (start-writing! fd writer)
  (when (not (hash-ref writers fd))
        (lets-write! fd))
  (hash-set! writers fd writer)
  #f)

(define (string-writer port s done)
  ;; exceedingly simple writer that outputs the string then quits
  (define start 0) ;; could be immutable, with some call/cc trickery, eh...
  (lambda (fd)
    (print "writing string" (string-length s) start)
    (let writing ()
      (if (= (string-length s) start)
          (begin
            (done-writing! fd)
            (done))
          (let ((amt (write-string/partial s port start)))
            (when (not (= 0 amt))
                  (set! start (+ start amt))
                  (writing)))))))

(define (connected port)
  ;; first called to say we connected
  ;; keep in mind connection is a WRITE operation
  (print "Trying to connect")
  (lambda (fd)
    (print "We connected yay")
    (start-reading! fd (do-reading port))
    (start-writing!
     (port->fdes port)
     (string-writer port "GET / HTTP/1.1\r\nHost: www.gnu.org\r\nConnection: close\r\n\r\n"
                    (lambda () (fixed-shutdown port 1))))))

;; return not #f if done reading for now, #f if need to keep reading
(define (do-reading port)
  (define buf (make-string #x1000))
  (define out (open-output-file "page.html"))
  (lambda (fd)
    (print "starting to read")
    (let readmore ()
      (let ((amt (read-string!/partial buf port)))
        (match amt
               (#f (fixed-shutdown port 0)
                   (close-output-port out)
                   (done-reading! fd))
               (0 42)
               (else
                (let ((derp (write-string/partial buf out 0 amt)))
                  (when (not (= derp amt))
                        (error "Filesystem error, or you're using a network file system.")))
                (print "got reply" amt)
                (readmore)))))))

(define (start-connecting-somewhere!)
  (define ai (car (getaddrinfo "www.gnu.org" "http")))
  (define sock  (socket (addrinfo:fam ai) (addrinfo:socktype ai)
                        (addrinfo:protocol ai)))
  (fcntl sock F_SETFL (logior (fcntl sock F_GETFL) O_NONBLOCK))
  ;; catch a system error b/c connect fails when nonblocking
  (catch 'system-error
         (lambda () (connect sock (addrinfo:addr ai)))
         (lambda (type name thing args codes)
           (when (not (= (car codes) 115))
                 (throw type name thing args codes))))
  (catch 'epoll
         (lambda ()
           (print (start-writing! (port->fdes sock) (connected sock))))
         (lambda (type errno)
           (print "ERROR" (strerror errno))
           (throw type errno))))

(start-connecting-somewhere!)

;; EPOLLIN events: connection accepted, data received, file opened
;; EPOLLOUT connection created, ready to write
;; - should clear when no data left to write

(define (main-loop)
  (let forever ()
    (when
     (not (and (= 0 (hash-count (const #t) writers))
               (= 0 (hash-count (const #t) readers))))
     ;; okay maybe not forever
     (let ((events (epoll-wait epfd 1000 92)))
       (let loop ((events events))
         (print 'events events EPOLLOUT EPOLLIN)
         (match events
                (((fd . events) rest ...)
                 (when (not (= 0 (logand events EPOLLIN)))
                       (let retry ()
                         (let
                             ((read-some-input (hash-ref readers fd)))
                           (if (eq? #f (read-some-input fd))
                               (retry)
                               (print "Done reading for now")))))
                 (when (not (= 0 (logand events EPOLLOUT)))
                       (let retry ()
                         (let
                             ((write-some-output (hash-ref writers fd)))
                           (if (eq? #f (write-some-output fd))
                               (retry)
                               (print "Done writing for now")))))
                 (loop rest))
                (else (forever))))))))
(print 'all 'done)

(main-loop)
